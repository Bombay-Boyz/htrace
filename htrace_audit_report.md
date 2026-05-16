# htrace — Comprehensive Source Code Audit Report

> **Scope**: Full line-by-line audit of all source files.
> **User-confirmed fixed**: C-1 (Shutdown Race), C-2 (Unsupervised forkIO), M-5 (Worker cancel on timeout).
> **Methodology**: Every finding is grounded in direct source reading. File and line references are exact.

---

## Executive Summary

The codebase is well-structured for a Haskell library: it uses `GHC2021`, `Text`/`ByteString` throughout, avoids partial functions, and has comprehensive test coverage. The three confirmed fixes (C-1, C-2, M-5) have been correctly implemented. However, **7 substantive defects remain open** (2 HIGH, 5 MEDIUM), plus several LOW issues and one new finding not in either prior report.

---

## Status of User-Confirmed Fixes

### ✅ C-1 — Shutdown Race: FIXED CORRECTLY

**File**: `src/Trace/Export/Batch.hs`

The three-state `ExporterState` machine has been implemented as recommended:

```haskell
data ExporterState = Running | Draining | Stopped
  deriving stock (Eq, Show)
```

`guardedEnqueue` performs the state check and enqueue atomically in a single `atomically` block:

```haskell
guardedEnqueue stateVar queue dropChan ne =
  atomically $ do
    st <- readTVar stateVar
    case st of
      Running  -> enqueueSTM queue dropChan ne
      Draining -> pure (ExportFailure ExporterShutDown)
      Stopped  -> pure (ExportFailure ExporterShutDown)
```

This correctly eliminates the TOCTOU window. The `Draining` transition happens at the top of `doShutdown` before any wait, so new enqueues are rejected immediately. **Fix is correct and complete.**

---

### ✅ C-2 — Unsupervised forkIO: FIXED CORRECTLY

**File**: `src/Trace/Export/Batch.hs`

`forkIO` has been replaced with `async` from `Control.Concurrent.Async`:

```haskell
workerAsync   <- async (worker queue stateVar workerDone)
notifierAsync <- async (notifier dropChan stateVar notifierDone)
```

Both handles are stored and used in `doShutdown` with `waitCatch`/`cancel`. **Fix is correct.**

---

### ✅ M-5 — Worker Cancel on Timeout: FIXED CORRECTLY

**File**: `src/Trace/Export/Batch.hs` — `doShutdown`

The timeout/cancel logic is now:

```haskell
workerExited <- timeout deadlineMicros (atomically (takeTMVar workerDone))
case workerExited of
  Just () -> do
    result <- waitCatch workerAsync
    case result of
      Left ex -> safeLog logError ("htrace: worker thread crashed: " <> ...)
      Right () -> pure ()
  Nothing -> do
    cancel workerAsync
    void (waitCatch workerAsync)   -- ensures cancellation completes
    safeLog logWarn "..."
```

The worker is hard-cancelled on timeout, and `waitCatch` after `cancel` ensures the thread is fully stopped before `exporterShutdown inner` is called. The same pattern is applied to `notifierAsync`. **Fix is correct and complete.**

---

## Remaining Open Issues

---

### 🔴 HIGH — H-1: `doFlush` Bypasses the Batch Worker (UNRESOLVED)

**File**: `src/Trace/Export/Batch.hs` — `doFlush` (line ~394)

This issue from the independent audit remains entirely unaddressed. `doFlush` still drains directly from the queue and calls `exporterExport inner` without going through the worker:

```haskell
doFlush queue = do
  batch <- atomically $ drainBatch queue (maxQueueSize cfg)
  case NE.nonEmpty batch of
    Nothing -> pure (Right ())
    Just ne -> do
      mResult <- timeout flushTimeoutMicros (exporterExport inner ne)
      ...
```

**The race is real**: `drainBatch` uses `tryReadTBQueue` in a loop inside `atomically`. The worker's own `drainBatch` also uses `tryReadTBQueue`. Both run concurrently against the same `TBQueue`. Individual `tryReadTBQueue` calls are atomic, so no span is double-taken, but the worker can be mid-batch while `doFlush` is also exporting a batch. The inner OTLP exporter (`doExport`) has no documented thread-safety guarantee beyond what `http-client`'s `Manager` provides (which is safe for concurrent requests). The result is two concurrent `httpLbs` calls against the same endpoint with no ordering guarantee — a collector may process them out of causal order.

Additionally, `guardedFlush` reads `stateVar` with `readTVarIO` (non-atomic):

```haskell
guardedFlush stateVar queue = do
  st <- readTVarIO stateVar    -- non-atomic read
  case st of
    Running -> doFlush queue   -- another thread calls doShutdown here
    ...
```

Between the read and `doFlush`, the state can transition to `Draining`. `doFlush` then races with the worker's drain-on-shutdown. This is a narrower TOCTOU than C-1 was, but is the same class of bug.

**Required fix**: Route flush through the worker via a sentinel, as recommended in the audit:

```haskell
data WorkItem
  = SpanItem (NonEmpty FinishedSpan)
  | FlushBarrier (MVar ())

-- doFlush: inject a barrier and block until worker signals it
doFlush = do
  barrier <- newEmptyMVar
  atomically (writeTBQueue queue (FlushBarrier barrier))
  readMVar barrier
```

---

### 🔴 HIGH — H-2: `memoryExporter` Accumulates in Reversed Per-Batch Order (UNRESOLVED)

**File**: `src/Trace/Export/Types.hs` — `memoryExporter`

```haskell
let doExport ne = do
      atomically $ modifyTVar' tvar (NE.toList ne <>)
      pure (ExportSuccess (NE.length ne))

    readAll = fmap reverse (readTVarIO tvar)
```

`NE.toList ne <> existingList` **prepends** each new batch to the front. After two exports of `[a,b]` then `[c,d]`, the TVar holds `[c,d,a,b]`. `reverse` gives `[b,a,d,c]`. Within each batch, order is **inverted**. For `readAll` to give `[a,b,c,d]` the accumulator must append:

```haskell
-- Correct:
atomically $ modifyTVar' tvar (<> NE.toList ne)
-- readAll = readTVarIO tvar  (no reverse needed)
```

This is a silent bug in the primary test utility. Tests that assert on span ordering by index (e.g., `spans !! 0` is the first span) will silently pass with wrong spans when more than one batch has been exported. This has almost certainly masked bugs in `BatchSpec` and `IntegrationSpec`.

---

### 🟡 MEDIUM — H-3: `Semigroup SpanAttrs` Convention Violation (UNRESOLVED)

**File**: `src/Trace/Attributes.hs`

```haskell
instance Semigroup SpanAttrs where
  SpanAttrs a <> SpanAttrs b = SpanAttrs (Map.union b a)
```

The implementation swaps operands to `Map.union`, making `<>` right-biased (right operand wins). This violates the Haskell `Semigroup` convention where `x <> y` means `x` takes precedence. The comment says "Right-biased: new values override existing ones" — the intent is documented, but the implementation will surprise any generic combinator:

- `mconcat [base, override1, override2]` — `override2` wins (correct for right-bias intent)
- `stimes 2 x` — effectively `x <> x`, harmless but semantically odd
- Any library function that uses `(<>)` with the documented Haskell convention will behave backwards

The immediate call site in `Monad.hs` happens to work because it was written to match the non-standard instance:

```haskell
si { siAttributes = siAttributes si <> attrs kvs }
-- siAttributes si is 'a', attrs kvs is 'b'
-- Map.union b a → attrs kvs wins ✓  (correct behaviour, accidental)
```

**Required fix**: Flip to left-bias (standard) and swap the call site argument order:

```haskell
-- Attributes.hs:
instance Semigroup SpanAttrs where
  SpanAttrs a <> SpanAttrs b = SpanAttrs (Map.union a b)  -- left wins (standard)

-- Monad.hs — setSpanAttrs: new values should win, so pass them as LEFT:
si { siAttributes = attrs kvs <> siAttributes si }
```

---

### 🟡 MEDIUM — H-4: Notifier Lost-Wakeup on Full `dropChan` (UNRESOLVED)

**File**: `src/Trace/Export/Batch.hs` — `enqueueSTM`, `notifier`

```haskell
when (nDropped > 0) $ do
  full <- isFullTBQueue dropChan
  when (not full) $
    writeTBQueue dropChan nDropped
```

When `dropChan` is full, the drop count is silently discarded. The notifier thread is blocked in `orElse` waiting on `dropChan`, which will never receive a write. `onDroppedSpans` is never called for those overflow events. Under sustained queue saturation this means drop reporting goes dark exactly when it is most needed.

**Required fix**: Replace the bounded channel with an atomic counter:

```haskell
droppedCounter <- newTVarIO (0 :: Int)

-- In enqueueSTM (replace dropChan write):
when (nDropped > 0) $ modifyTVar' droppedCounter (+ nDropped)

-- In worker loop, after each export cycle:
reportDropped droppedCounter (onDroppedSpans cfg)
  where
    reportDropped counter cb = do
      n <- atomically $ do
        n <- readTVar counter
        when (n > 0) $ writeTVar counter 0
        pure n
      when (n > 0) $ cb n
```

---

### 🟡 MEDIUM — M-1: `TraceM` Is a Concrete Type Alias (UNRESOLVED)

**File**: `src/Trace/Monad.hs`

```haskell
type TraceM = ReaderT TraceContext IO
```

All exported functions (`inSpanM`, `setSpanAttr`, `addEvent`, etc.) are hardcoded to `TraceM`. Any application using `Effectful`, `Polysemy`, `ReaderT AppEnv IO`, or any other effect stack cannot use `inSpanM` without discarding their stack. This is a library design defect: an OpenTelemetry library should integrate into existing monadic stacks, not mandate its own.

**Required fix**: Parameterise over `MonadUnliftIO m` and `MonadReader TraceContext m`:

```haskell
inSpanM
  :: (MonadUnliftIO m, MonadReader TraceContext m)
  => SpanName -> SpanKind -> SpanAttrs -> (Span -> m a) -> m a
inSpanM name kind initialAttrs body = do
  TraceContext parent tracer <- ask
  withRunInIO $ \runInIO ->
    inSpanCore tracer parent name kind initialAttrs $ \sp ->
      runInIO $ local (\tc -> tc { tcCurrentSpanContext = Just (spanContext sp) })
                      (body sp)
```

`TraceM` can remain as a convenience alias but must not appear in exported function signatures.

---

### 🟡 MEDIUM — M-3: `GzipCompression` Exported but Fails at Runtime (UNRESOLVED)

**File**: `src/Trace/Export/Otlp.hs`

```haskell
data Compression = NoCompression | GzipCompression
  deriving stock (Show, Eq)
```

`GzipCompression` is exported and type-checks, but `otlpExporter` returns `Left (ExporterUnsupportedCompression ...)` at runtime when it is requested. A caller can write perfectly valid, type-checked code that always fails at initialisation. This breaks the principle of making illegal states unrepresentable.

**Required fix**: Remove `GzipCompression` from the public export list until it is implemented. Move it to `Trace.Internal.Otlp` or behind a `{-# WARNING #-}` pragma:

```haskell
-- Temporary: hide from public API
{-# WARNING GzipCompression "GzipCompression is not yet implemented" #-}
```

Or simply do not export the constructor from the module's export list.

---

### 🟡 MEDIUM — M-6: `parseTraceparent` Rejects Valid Uppercase Hex (UNRESOLVED)

**File**: `src/Trace/Propagation.hs` — `decodeHex`

```haskell
decodeHex expectedBytes t
  | Text.length t /= expectedBytes * 2 = Left (...)
  | otherwise =
      case Base16.decode (TE.encodeUtf8 t) of
        Right bs -> Right bs
        Left  e  -> Left (Text.pack e)
```

`base16-bytestring >= 1.0` requires lowercase input and returns `Left` for uppercase hex. The W3C Trace Context spec (§4.3) requires lowercase for version `00` but says parsers **SHOULD** accept uppercase for forward compatibility. A conformant sender using a future version with uppercase hex IDs will receive `PropagationInvalid (InvalidTraceId ...)` from this library, breaking distributed traces silently.

`validVersion` normalises the version to lowercase via `Text.toLower`, but `decodeHex` feeds the raw (potentially uppercase) text to `Base16.decode`. This is inconsistent.

**Required fix**:

```haskell
decodeHex expectedBytes t =
  let normalised = Text.toLower t
  in if Text.length normalised /= expectedBytes * 2
       then Left ("expected " <> Text.pack (show (expectedBytes * 2)) <> " hex chars")
       else case Base16.decode (TE.encodeUtf8 normalised) of
              Right bs -> Right bs
              Left  e  -> Left (Text.pack e)
```

---

### 🟡 MEDIUM — M-7: `withTracing` Does Not Flush Before Shutdown (UNRESOLVED)

**File**: `src/Trace/Monad.hs` — `withTracing`

```haskell
bracket
  (pure ())
  (\_ -> exporterShutdown batched)
  (\_ -> action tracer)
```

`exporterShutdown` transitions state to `Draining` and waits for the worker to drain. This works — the worker will export remaining spans before exiting. However:

1. If `action tracer` throws, `bracket` calls `exporterShutdown` immediately. Spans that are in the queue but not yet exported have only `shutdownTimeout` seconds to get out.
2. There is no explicit flush to drain the queue first. A flush before shutdown gives the inner exporter a clean slate before it is torn down.

Adding an explicit flush is low-risk and makes the shutdown sequence predictable and documented:

```haskell
(\_ -> do
    _ <- exporterFlush batched   -- drain on best-effort basis
    exporterShutdown batched)
```

---

## New Finding (Not in Either Prior Report)

### 🟡 MEDIUM — NEW: `withTracing` Does Not Propagate `BatchConfigError`

**File**: `src/Trace/Monad.hs` — `withTracing`

```haskell
batchedR <- batchExporter batchCfg inner
case batchedR of
  Left be ->
    exporterShutdown inner
      *> pure (Left (ExporterBatchInit be))
  Right batched -> ...
```

When `batchExporter` returns `Left`, `exporterShutdown inner` is called before returning `Left (ExporterBatchInit be)`. This is correct. However, `defaultBatchConfig` is used directly with only `onDroppedSpans` and `batchLogger` overridden:

```haskell
let batchCfg = defaultBatchConfig
      { onDroppedSpans = defaultOnDroppedSpans (configLogger cfg)
      , batchLogger    = configLogger cfg
      }
```

`defaultBatchConfig` has `maxQueueSize = 2048`, `maxExportBatch = 512`, etc. There is no way for the caller to customise batch parameters through `withTracing`. If `BatchConfig` ever needs to be user-configurable (e.g., via environment variables `OTEL_BSP_MAX_QUEUE_SIZE`, `OTEL_BSP_MAX_EXPORT_BATCH_SIZE` as specified in the OTel spec), `withTracing` would need to accept or derive these. Currently those OTel spec env vars are silently ignored.

**Recommended fix**: Add a `batchConfig :: Maybe BatchConfig` field to `TracingConfig`, or load `OTEL_BSP_*` vars in `fromEnv`. As a minimum, document the gap.

---

## Low Severity (Confirmed, Unchanged)

### L-1: Empty Benchmarks
**File**: `bench/BatchBench.hs` — still empty. `bench/Main.hs` defines `benchmarks = []`. No performance regression detection is possible.

### L-2: Duplicate `Base16` Import
**File**: `src/Trace/Export/Otlp.hs` — `Data.ByteString.Base16` is imported twice as `Base16`. Will produce `-Wduplicate-exports` under strict profiles. Remove one.

### L-3: All Internal Modules Publicly Exposed
**File**: `htrace.cabal` — `Trace.Export.Batch`, `Trace.Core`, `Trace.Attributes`, `Trace.Export.Otlp` are all `exposed-modules`. Implementation internals (`BatchConfig`, `guardShutDown`, `enqueueSTM`) are stable API as far as downstream consumers are concerned. Move to `other-modules` or `Trace.Internal.*`.

### L-4: `fromEnv` Hardcodes `stderrLogger`
**File**: `src/Trace/Config.hs` — `<*> pure stderrLogger` is hardcoded. Applications with structured loggers cannot inject their logger via the environment-loading path without reconstructing `TracingConfig` manually.

### L-5: `ConfigError` Derives `Eq` on `Double`
**File**: `src/Trace/Config.hs` — `InvalidSampleRate !Double` derives `Eq`. `Eq` on `Double` is broken for `NaN`. While `mkSampleRate` prevents `NaN` from reaching this constructor in practice, the instance is technically unsound.

---

## Issues Confirmed Fixed vs. Audit

| ID | Finding | Status |
|----|---------|--------|
| C-1 | Shutdown race — spans lost | ✅ **FIXED** |
| C-2 | Unsupervised forkIO — silent failures | ✅ **FIXED** |
| C-3 | No retry on transient export failure | ⚠️ **OPEN** (not in scope per user) |
| H-1 | `doFlush` bypasses worker | 🔴 **OPEN** |
| H-2 | `memoryExporter` order bug | 🔴 **OPEN** |
| H-3 | `Semigroup SpanAttrs` backwards convention | 🟡 **OPEN** |
| H-4 | Notifier lost-wakeup on full `dropChan` | 🟡 **OPEN** |
| M-1 | `TraceM` concrete type alias | 🟡 **OPEN** |
| M-2 | Unbounded span attributes/events | 🟡 **OPEN** (not audited here — confirmed by prior audit) |
| M-3 | `GzipCompression` exported but unusable | 🟡 **OPEN** |
| M-4 | HTTP Manager pool not configured | 🟡 **OPEN** (not audited here — confirmed by prior audit) |
| M-5 | `doShutdown` worker cancel on timeout | ✅ **FIXED** |
| M-6 | `parseTraceparent` rejects valid uppercase hex | 🟡 **OPEN** |
| M-7 | No flush before shutdown | 🟡 **OPEN** |
| NEW | `withTracing` doesn't expose `BatchConfig` / OTel BSP vars | 🟡 **OPEN** |
| L-1 | Empty benchmarks | 🟡 **OPEN** |
| L-2 | Duplicate `Base16` import | 🟡 **OPEN** |
| L-3 | All modules publicly exposed | 🟡 **OPEN** |
| L-4 | `fromEnv` hardcodes `stderrLogger` | 🟡 **OPEN** |
| L-5 | `ConfigError Eq` on `Double` | 🟡 **OPEN** |

---

## Recommended Next Steps (Priority Order)

1. **H-1** — Route flush through worker via `FlushBarrier` sentinel. Medium effort, eliminates a data-race class.
2. **H-2** — Fix `memoryExporter` accumulator direction (`<>` vs prepend + `reverse`). Trivial, restores test utility correctness.
3. **H-3** — Fix `Semigroup SpanAttrs` to left-bias and update call sites. Small, prevents future confusion with generic combinators.
4. **H-4** — Replace `dropChan` with an atomic counter. Small, ensures drop reporting is lossless.
5. **M-6** — Normalise hex to lowercase before `Base16.decode`. Trivial, improves W3C compliance.
6. **M-3** — Remove `GzipCompression` from public exports. Trivial, makes API type-safe.
7. **M-7** — Add explicit flush before shutdown in `withTracing`. Trivial.
8. **NEW** — Expose `OTEL_BSP_*` config loading in `fromEnv`. Medium, completes OTel spec compliance.
9. **M-1** — Parameterise `TraceM` API on `MonadUnliftIO`. Large, but necessary for production adoption.
10. **L-2, L-4** — Fix duplicate import and hardcoded logger. Trivial.
11. **L-3** — Restructure cabal exposed-modules. Medium.
12. **L-1** — Add benchmark suite. Medium.
