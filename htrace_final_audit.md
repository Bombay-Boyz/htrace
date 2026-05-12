# htrace — Final Production Readiness Audit

**Auditor:** Senior Software Architect / Haskell review  
**Codebase:** htrace v0.1.0.0  
**Date:** May 2026  
**Modules reviewed:** `Trace.Core`, `Trace.Monad`, `Trace.Propagation`, `Trace.Config`, `Trace.Export.Batch`, `Trace.Export.Otlp`, `Trace.Export.Types`, test suite, bench

---

## Executive Summary

htrace is a well-structured, idiomatic Haskell tracing library. The module decomposition is clean, STM usage is mostly disciplined, and the public API surface is intentionally narrow. For internal tooling or developer-facing observability in non-critical services it is already usable.

For mission-critical, regulated, or high-availability environments — finance, defence, safety-critical infrastructure — it is **not yet suitable**. The core issues are not superficial: they concern shutdown determinism, worker supervision, telemetry durability, and several correctness gaps that are only visible under adversarial or failure conditions.

This report verifies, corrects, and extends the prior audit. Every finding below has been confirmed against the actual source code.

---

## Severity Classification

| Severity | Meaning |
|---|---|
| **Critical** | Can deadlock, corrupt telemetry semantics, or produce unrecoverable failure |
| **High** | Unsafe for production in regulated or HA environments |
| **Medium** | Operational weakness likely to cause future incidents |
| **Mild** | Design limitation, standards gap, or maintainability concern |

---

## Prior Audit Verdict

The previous audit's three Critical findings (C1 timeout-via-race, C2 blocking shutdown, C3 unsupervised workers) are **confirmed correct**. The High and Medium findings are also confirmed. Several prior descriptions overstate the *mechanism* slightly while the conclusion holds; corrections are noted inline. New findings not present in the prior audit are marked **[NEW]**.

---

# Critical Findings

---

## C1 — Export timeout does not bound execution

**Location:** `Trace.Export.Batch`, `worker` and `doFlush`

**Confirmed code:**
```haskell
race
  (threadDelay timeoutMicros)
  (try (exporterExport inner ne) :: IO (Either SomeException ExportResult))
```

**Verdict:** Confirmed and accurate.

`race` from `UnliftIO.Async` delivers an async exception (`AsyncCancelled`) to the losing branch. This is only safe if the loser is interruptible. The OTLP exporter calls `httpLbs` from `http-client`, which internally uses `Network.Socket` operations wrapped in `withForeignPtr` and TLS state machines — all of which have non-interruptible sections. A stalled TLS handshake or a kernel send-buffer that never drains will not respond to `AsyncCancelled`, leaving the export goroutine alive indefinitely.

**Consequences:** Thread accumulation, memory growth, scheduler degradation, duplicate export attempts if the timed-out thread eventually unblocks, and shutdown deadlock (see C2).

**Correction on prior audit:** The prior audit suggested `withAsync` + `cancel` as a fix. This is necessary but not sufficient on its own, because `cancel` also delivers `AsyncCancelled`. The real fix requires ensuring the inner exporter is interruptible, which means either:

1. Using `http-client`'s built-in `responseTimeout` (already done — see H5 below for a new finding about the double-timeout issue this creates), plus making the `Batch` layer's own timeout a safety net with a separate OS-level cancel mechanism, or
2. Running the exporter in a subprocess/process boundary.

**Recommended fix:**
```haskell
-- Use withAsync + a hard cancel, and document that
-- inner exporters MUST be interruptible or use their own timeout.
withAsync (exporterExport inner ne) $ \a -> do
  result <- System.Timeout.timeout timeoutMicros (wait a)
  case result of
    Nothing -> cancel a >> pure (Left (ExportTimeout (exportTimeout cfg)))
    Just v  -> pure (Right v)
```

Also add an `exporterIsInterruptible :: Bool` field to `SpanExporter` so the batch layer can assert the contract at construction time rather than silently hoping it holds.

---

## C2 — Shutdown blocks forever on exporter hang

**Location:** `Trace.Export.Batch`, `doShutdown`

**Confirmed code:**
```haskell
doShutdown _queue shutdownVar workerDone notifierDone = do
  atomically (writeTVar shutdownVar True)
  _ <- atomically (takeTMVar workerDone)
  _ <- atomically (takeTMVar notifierDone)
  exporterShutdown inner
```

**Verdict:** Confirmed and accurate.

If the worker thread is blocked inside a hung exporter call, it never reaches `atomically (putTMVar done ())`, and `doShutdown` blocks forever. The `_queue` parameter being unnamed (prefixed `_`) confirms the queue is not drained or abandoned before waiting — there is no fallback path.

**New sub-issue not in prior audit:** `exporterShutdown inner` is called *after* waiting for both `workerDone` and `notifierDone`. If the shutdown itself hangs (the inner exporter's shutdown does a final flush), the whole thing is still stuck. There is no total deadline on the shutdown sequence.

**Recommended fix:**
```haskell
shutdownWithDeadline :: Int -> BatchState -> IO ()
shutdownWithDeadline deadlineMicros st = do
  atomically (writeTVar (shutdownVar st) True)
  result <- System.Timeout.timeout deadlineMicros $
    atomically $ do
      takeTMVar (workerDone st)
      takeTMVar (notifierDone st)
  when (isNothing result) $
    logWarn (batchLogger cfg) "htrace: shutdown timed out; abandoning worker"
  exporterShutdown inner  -- best-effort; also needs a deadline
```

The API should expose `shutdownTimeout :: NominalDiffTime` in `BatchConfig` and default it to a value like 5s, separate from `exportTimeout`.

---

## C3 — Background workers are unsupervised

**Location:** `Trace.Export.Batch`

**Confirmed code:**
```haskell
_ <- forkIO (worker queue shutdownVar workerDone)
_ <- forkIO (notifier dropChan shutdownVar notifierDone)
```

**Verdict:** Confirmed and accurate.

Plain `forkIO` threads with no supervision. Any uncaught exception terminates the thread silently. The `TMVar` slots (`workerDone`, `notifierDone`) are never filled if the thread dies abnormally, which feeds directly into C2's blocking shutdown. The application continues running with no active telemetry export and no signal that this has happened.

**Correction on prior audit:** The prior audit suggested `async` with linked workers. Linking is actually undesirable here — if the worker crashes, you don't want to crash the application, you want to restart the worker or at minimum log and signal degraded state. Supervision semantics require explicit restart logic, not linking.

**Recommended fix:**
```haskell
-- Supervised worker with restart and health TVar
data WorkerHealth = WorkerHealthy | WorkerDegraded Text | WorkerDead
  deriving (Show, Eq)

supervisedWorker
  :: TVar WorkerHealth
  -> IO ()   -- the work loop
  -> IO ()
supervisedWorker health loop = go (3 :: Int)
  where
    go 0 = atomically (writeTVar health (WorkerDead))
    go n = do
      result <- try loop
      case result of
        Right ()           -> pure ()  -- clean exit (shutdown path)
        Left (e :: SomeException) -> do
          atomically (writeTVar health (WorkerDegraded (Text.pack (show e))))
          threadDelay 100_000  -- brief back-off before restart
          go (n - 1)
```

Expose `workerHealth :: IO WorkerHealth` from the returned `SpanExporter` (or a companion type) for operator introspection.

---

## C4 — `recordException` is non-atomic [NEW]

**Location:** `Trace.Monad`, `recordException`

**Confirmed code:**
```haskell
recordException sp e = do
  let evAttrs = ...
  _ <- setStatusError sp (Text.pack (displayException e))
  addEvent sp "exception" evAttrs
```

Each of `setStatusError` and `addEvent` calls `modifySpan`, which runs its own `atomically` block. These are two separate STM transactions with observable interleavings:

- Thread A calls `recordException`, commits `setStatusError`.
- Thread B reads the span internals — sees `StatusError` without the `exception` event.
- Thread A then commits `addEvent`.
- Another reader sees the event without the error status if thread A was preempted between the two commits after the first write.

For exception spans, the error status and the event are a logical unit. Separating them violates the "consistent snapshot" guarantee that STM is supposed to provide.

This was listed as Medium in the prior audit. After reading the actual code I am promoting it to **Critical** because `recordException` is likely to be called on every error path in instrumented services, and the race window — though small — is real and deterministic to reproduce under contention.

**Recommended fix:**
```haskell
recordException sp e = do
  now <- clockNow (spanClock sp)
  let evAttrs = ...
      msg     = Text.pack (displayException e)
  modifySpan sp $ \si ->
    si { siStatus = StatusError (mkErrorMessageFallback msg)
       , siEvents = SpanEvent "exception" now evAttrs : siEvents si
       }
```

A single `modifySpan` call means the status and event are written in one STM transaction and are always visible together.

---

# High Severity Findings

---

## H1 — Queue drop is silent to the operator

**Location:** `Trace.Export.Batch`, `enqueue`

**Verdict:** Confirmed. The `onDroppedSpans` callback exists and fires, but the return value of `exporterExport` when drops occur is:

```haskell
pure (ExportSuccess (n - dropped))
```

This reports a successful partial enqueue. The caller sees `ExportSuccess 3` when they submitted 10 spans and 7 were dropped. The name `ExportSuccess` is semantically misleading for a partial result.

**New sub-issue:** The `dropChan` is itself bounded at 64. If the notifier thread is behind, drop notifications can be silently lost:

```haskell
when (not full) $ writeTBQueue dropChan nDropped
```

Under sustained saturation the notifier cannot keep up, `dropChan` fills, and some drop counts are never delivered to `onDroppedSpans`. The operator's drop counter becomes an undercount.

**Recommended fix:**

Introduce a new result variant:
```haskell
data ExportResult
  = ExportSuccess !Int
  | ExportPartial !Int !Int   -- accepted, dropped
  | ExportFailure !ExportError
```

Replace the `dropChan` indirection with a monotonic `TVar Int` counter that can be read directly. This is always accurate, never lossy, and eliminates the notifier thread entirely:

```haskell
totalDropped :: TVar Int
-- increment atomically in enqueue; expose via a health-check accessor
```

---

## H2 — Export failures permanently discard spans

**Location:** `Trace.Export.Batch`, `worker`

**Verdict:** Confirmed. On any non-success result (timeout, exception, `ExportFailure`) the batch is logged and discarded. No retry, no dead-letter queue, no requeue.

**Nuance added:** The batch is drained from the queue *before* the export attempt. If the export fails, the spans are already gone from the queue and from memory. This is not just a missing feature — it is a structural decision that makes retry impossible without significant redesign (would need to hold the batch reference until confirmed delivery).

**Recommended fix:** Retain the batch reference across the export attempt. On failure, re-enqueue at the front (priority queue or separate retry deque) with exponential backoff and a retry budget:

```haskell
data RetryState = RetryState
  { retryCount   :: !Int
  , retryBatch   :: !(NonEmpty FinishedSpan)
  , nextRetryAt  :: !UTCTime
  }
```

A simple TBQueue for retries with a cap on `retryCount` is sufficient for most production use cases.

---

## H3 — `GzipCompression` is advertised but not implemented

**Location:** `Trace.Export.Otlp`

**Confirmed comment:**
```haskell
-- Note: 'GzipCompression' is defined but not yet honoured in v0.1.0.0.
```

**Verdict:** Confirmed. The constructor is exported in the public API. Operators who set `otlpCompression = GzipCompression` get no compression and no error.

**Recommended fix:** Until implemented, either:
- Remove `GzipCompression` from the exported API entirely, or
- Return `Left (ExporterCompressionUnsupported)` at construction time when `GzipCompression` is requested.

Never silently ignore a feature flag that controls data transport behavior.

---

## H4 — Internal telemetry subsystem has no observability

**Location:** All of `Trace.Export.Batch`, `Trace.Export.Types`

**Verdict:** Confirmed. No metrics are emitted for queue depth, export latency, timeout rate, drop rate, worker liveness, or retry state.

**New observation:** The `InternalLogger` abstraction exists and is well-designed, but is text-only. Structured diagnostics (machine-readable key-value pairs) would allow operators to wire htrace's health into their existing monitoring infrastructure without log parsing.

**Recommended fix:** Add a `TelemetryMetrics` record alongside `InternalLogger`:

```haskell
data TelemetryMetrics = TelemetryMetrics
  { metricsQueueDepth      :: IO Int
  , metricsExportedTotal   :: IO Int
  , metricsDroppedTotal    :: IO Int
  , metricsExportErrors    :: IO Int
  , metricsExportTimeouts  :: IO Int
  , metricsWorkerLive      :: IO Bool
  }
```

Populate via `TVar` counters in the batch worker. Expose via `batchExporterMetrics` alongside the exporter. The default implementation uses `stderrLogger`-style no-ops so it is non-breaking.

---

## H5 — Double-timeout: OTLP and Batch timeouts interact silently [NEW]

**Location:** `Trace.Export.Otlp` + `Trace.Export.Batch`

**Observed code in `otlpExporter`:**
```haskell
responseTimeout = responseTimeoutMicro
  (round (otlpTimeout cfg * 1_000_000))
```

**Observed code in `defaultBatchConfig`:**
```haskell
exportTimeout = 30
```

**Problem:** There are two independent timeout layers that operators configure separately, with no enforcement of their relationship. If `otlpTimeout` (the HTTP-level timeout, default 10s from `fromEnv`) is larger than `exportTimeout` (the batch-level timeout, default 30s), the HTTP request will be killed by the batch layer before `http-client`'s own timeout fires. If `otlpTimeout` is smaller, the HTTP layer kills the request internally and `http-client` returns an exception, which the batch layer catches and logs — at which point the batch timeout has not yet fired, and the batch loop immediately continues correctly.

The ordering is therefore: `otlpTimeout` should always be strictly less than `exportTimeout`. This invariant is nowhere documented, nowhere validated, and not mentioned in any config type. An operator who sets `otlpTimeout = 60` and `exportTimeout = 30` is in the broken configuration described in C1.

**Recommended fix:** Either collapse to a single timeout (simplest), or validate the relationship at construction time in `withTracing`:

```haskell
when (otlpTimeout otlpCfg >= exportTimeout batchCfg) $
  logWarn logger "htrace: otlpTimeout >= exportTimeout; \
                 \HTTP timeout will never fire before batch timeout"
```

---

# Medium Severity Findings

---

## M1 — `recordException` non-atomicity (promoted to C4 above)

See C4.

---

## M2 — Mutable live spans create race surfaces

**Location:** `Trace.Core`, `Trace.Monad`

**Verdict:** Confirmed. Spans are mutable `TVar SpanInternals` throughout their lifetime, with no ownership discipline enforced by the type system.

**New observation:** The `modifySpan` function is exported from `Trace.Monad` in the public API. This means any code with a `Span` reference can mutate it at any time, from any thread. There is no scoping mechanism that says "only the thread that created this span may mutate it."

**Recommended fix:** Restrict `modifySpan` to `internal` and expose only the named mutators (`setSpanAttr`, `addEvent`, etc.) in the public API. This is already almost the case — `modifySpan` is listed in the export list of `Trace.Monad` — removing it from `module Trace` (the facade) would close the surface without breaking the internal design.

---

## M3 — W3C `tracestate` and baggage propagation missing

**Location:** `Trace.Propagation`

**Verdict:** Confirmed. `traceparent` is parsed and emitted; `tracestate` is neither parsed nor forwarded. Baggage propagation is absent entirely.

**New observation:** `injectHeaders` silently drops any incoming `tracestate`. In a proxy or middleware scenario, htrace will receive a request with `tracestate` populated by an upstream vendor system, parse the `traceparent` correctly, and then re-inject *only* `traceparent` on outbound calls, destroying the vendor-specific state. This is a silent interoperability regression.

**Recommended fix:** At minimum, preserve and forward `tracestate` as an opaque value even before full parsing support is added:

```haskell
tracestateHeader :: CI ByteString
tracestateHeader = CI.mk "tracestate"

injectHeaders :: SpanContext -> Maybe Text -> [Header] -> [Header]
injectHeaders ctx mTracestate hs =
  (traceparentHeader, ...) :
  maybe id (\ts -> ((tracestateHeader, TE.encodeUtf8 ts) :)) mTracestate $
  filter ((/= traceparentHeader) . fst) $
  filter ((/= tracestateHeader) . fst) hs
```

---

## M4 — Future-version `traceparent` parsing may be over-permissive

**Location:** `Trace.Propagation`

**Confirmed code:**
```haskell
(v : tid : sid : flgs : _) ->
  if not (validVersion v) ...
```

**Verdict:** Confirmed. Future W3C versions that change field semantics will be parsed with v0.0 semantics and the unknown fields silently discarded.

**New observation:** The comment in `parseTraceparent` says "per W3C Trace Context specification §4.3 (forward compatibility)" — this is correct per the current spec, so the behavior is *intentional*. The prior audit's framing as a bug is slightly too strong. The real concern is: if a future version changes the meaning of existing fields (e.g., flags), the parser will misinterpret them without warning.

**Recommended fix:** Add a logged warning (not an error) when a non-`00` version is encountered, so operators know they may be receiving newer-format headers. A `PropagationResult` variant `PropagationFutureVersion !Text !SpanContext` would allow callers to handle this distinctly.

---

## M5 — Exporter lifecycle is not type-enforced

**Location:** `Trace.Export.Types`

**Confirmed comment:**
```haskell
-- ^ Release resources. Subsequent calls to 'exporterExport' are
--   undefined behaviour; callers must not use the exporter after shutdown.
```

**Verdict:** Confirmed. The "undefined behaviour" comment is accurate and honest but represents a real API hazard.

**New observation:** `withTracing` correctly uses `bracket` to call `exporterShutdown batched` on exit. However, the `Tracer` record (which holds a reference to `batched`) is passed to the user action. After `withTracing` returns, the user could retain a reference to the `Tracer` and call `flush` or `inSpan` on it. `flush` calls `exporterFlush . tracerExporter`, which after shutdown has undefined behavior.

This is not a theoretical concern — any code that captures a `Tracer` in a `IORef`, caches it, or stores it in shared state is exposed.

**Recommended fix:** Use a `TVar Bool` `isShutDown` flag in the exporter record and check it at the start of `exporterExport` and `exporterFlush`, returning a clear `ExportFailure (ExporterShutDown)` rather than undefined behavior. This is a one-line change per operation and eliminates the hazard entirely without needing linear types.

---

## M6 — Orphan span detection absent

**Location:** `Trace.Monad`, `inSpanCore`

**Verdict:** Confirmed. `inSpanCore` uses `bracket` correctly for normal completion and exception paths, so spans are always ended. However, a span can remain "active" indefinitely if the thread running the body blocks forever (deadlock, infinite loop, blocking FFI). The span holds a live `TVar` in memory and counts as active for the duration.

**New observation:** In `RecordOnly` mode (`SamplingDecision`), spans are recorded but not exported. These orphan spans accumulate in memory if the thread never terminates, with no cleanup path. The `SpanState` correctly models this with `SpanActive`, `SpanEnded`, and `SpanDropped`, but `SpanDropped` is only set by the sampler at creation, not by any watchdog.

**Recommended fix:** Add a configurable `maxSpanDuration :: Maybe NominalDiffTime` to `BatchConfig`. A periodic sweep (the existing worker thread could double as a watchdog) marks overdue `SpanActive` spans as `SpanEnded` with a synthetic end time and emits a warning event. This is strictly opt-in and zero cost when not configured.

---

## M7 — `SpanAttrs` `Semigroup` instance is right-biased inconsistently with documentation [NEW]

**Location:** `Trace.Attributes`

**Confirmed code:**
```haskell
instance Semigroup SpanAttrs where
  SpanAttrs a <> SpanAttrs b = SpanAttrs (Map.union b a)
```

The `Semigroup` instance is right-biased: `b` overrides `a`. However:

- The module-level comment on `setSpanAttrs` says "Right-biased: new values override existing ones" — which implies `new <> existing`, meaning the *right* operand is "new". This is consistent with how `setSpanAttrs` uses it: `siAttributes si <> attrs kvs`.
- The `attrs` builder uses `Map.fromList`, which is left-biased for duplicates (last key wins in the input list order). This differs from the `Semigroup` bias.
- The `Semigroup` comment in `Attributes.hs` says "Right-biased merge: keys in the right operand override the left" — which is correct for `<>` but is the opposite of `Map.union`'s behavior (which is left-biased). The implementation achieves right-bias via `Map.union b a` (arguments reversed), but this is subtle and fragile. A maintainer reading `Map.union a b` in a diff will not notice the reversal is intentional.

This is not wrong today, but it is a latent correctness trap.

**Recommended fix:** Use `Map.unionWith (\_old new -> new) a b` instead of `Map.union b a`. This makes the right-bias explicit and self-documenting. Add a property test: `∀ a b k. k ∈ b → lookupAttr k (a <> b) == lookupAttr k b`.

---

## M8 — `inSpanCore` exports directly on span finalization, bypassing batch queue [NEW]

**Location:** `Trace.Monad`, `inSpanCore`, `finalize`

**Confirmed code:**
```haskell
finalize exportOnEnd sp = do
  ...
  case (exportOnEnd, mFinished) of
    (True, Just fs) -> do
      result <- try (exporterExport (tracerExporter tracer) (fs NE.:| []))
```

When a span ends, `finalize` calls `exporterExport` directly on the tracer's exporter. In the normal `withTracing` setup, `tracerExporter` is the *batched* exporter, so this call goes through the queue — correct.

However, `Tracer` is a plain record. Nothing prevents constructing a `Tracer` with a raw (non-batched) `SpanExporter`. In that case, span finalization calls the inner exporter synchronously, in the finalizer, on the user's thread. If the exporter is slow (an HTTP call with a 30s timeout), the `bracket` cleanup will block for up to that timeout before the user's function returns.

This is not guarded against, not documented, and not detectable from the type.

**Recommended fix:** Document the contract explicitly: "The exporter in `tracerExporter` should be a batched exporter. Providing a synchronous exporter will block span finalization." Alternatively, enforce it by making `Tracer` construction only possible via `withTracing`, removing the public `Tracer {..}` constructor exposure (it is currently constructable directly by users).

---

## M9 — `encodeBase16` is a hand-rolled reimplementation [NEW]

**Location:** `Trace.Export.Otlp`

**Confirmed code:**
```haskell
encodeBase16 :: ByteString -> ByteString
encodeBase16 = LBS.toStrict . LBS.concatMap byteToHex . LBS.fromStrict
  where
    byteToHex b = ...
```

The comment says "without the base16-bytestring import clash." The `base16-bytestring` package is a well-tested, SIMD-accelerated implementation. This hand-rolled version:

1. Converts `ByteString → LazyByteString → [Word8] → LazyByteString → ByteString`, doing multiple full copies.
2. Processes one byte at a time.
3. Produces incorrect output for bytes above 127 if the `LBS.concatMap` semantics diverge from expected (though in practice this is fine for byte values).

For TraceId (16 bytes) and SpanId (8 bytes) the performance difference is negligible. But this is test-grade code in a production path.

**Recommended fix:** Add `base16-bytestring` as a dependency (it is already present in the Haskell ecosystem and zero-cost to add) and replace with `Data.ByteString.Base16.encode`. Remove the workaround comment — if there was an import clash, it should be resolved by module qualification, not by reimplementation.

---

## M10 — `OTEL_TRACES_SAMPLER_ARG` accepts only positive doubles; zero is rejected [NEW]

**Location:** `Trace.Config`, `loadSamplerConfig`

**Confirmed code:**
```haskell
readPositiveDouble str = case TR.double (Text.pack str) of
  Right (d, rest) | Text.null rest -> Just d
  _                                -> Nothing
-- ...
case arg >>= readPositiveDouble of
  Nothing -> Failure ...
```

`readPositiveDouble` returns `Just` for any positive double, but `mkSampleRate` accepts `[0, 1]`. The value `0.0` from `OTEL_TRACES_SAMPLER_ARG=0` is rejected by `readPositiveDouble` (returns `Nothing` for `0`), causing a confusing `MissingRequiredVar` error rather than `InvalidSampleRate`.

A user setting `OTEL_TRACES_SAMPLER_ARG=0` to achieve "sample nothing" gets a misleading error telling them the variable is missing.

**Recommended fix:** Rename and fix the predicate:
```haskell
readNonNegativeDouble str = case TR.double (Text.pack str) of
  Right (d, rest) | Text.null rest && d >= 0 -> Just d
  _ -> Nothing
```

---

# Mild Findings

---

## L1 — Custom `Validation` applicative reinvents a wheel

**Location:** `Trace.Config`

**Verdict:** Confirmed and fair. The custom implementation is correct and small. The concern is future drift as `Config` grows. The `validation` or `either` packages provide battle-tested equivalents. This is a judgment call — for a library with a minimal dependency footprint, inline is defensible. The implementation should at minimum have a property test verifying error accumulation.

---

## L2 — Benchmarks are empty stubs

**Location:** `bench/BatchBench.hs`, `bench/Main.hs`

**Confirmed code:**
```haskell
module BatchBench where
-- (empty)

main = defaultMain benchmarks where benchmarks = []
```

Both files are empty. The `baseline.csv` is also empty. There is no performance baseline at all. For a batching exporter, the critical performance properties to benchmark are:

- Enqueue throughput under single-producer and multi-producer load
- Flush latency distribution (p50, p99, p999)
- Queue saturation behavior (what happens when producers outrun the worker)
- Worker loop overhead per batch

**Recommended fix:** Implement at minimum: single-span enqueue throughput, 100-span batch enqueue, flush-under-load. Use `tasty-bench`'s `--csv` output to establish a regression baseline in CI.

---

## L3 — Test suite lacks concurrency stress coverage

**Location:** `test/`

**Verdict:** Confirmed. Tests use `threadDelay` as a synchronization primitive in several places (e.g. `threadDelay 300_000` after enqueuing a span to "let the worker fire"). This is timing-dependent and will produce flaky results on slow CI machines.

**New observation:** There are no property-based tests (`QuickCheck` or `hedgehog`) despite the codebase having a `Generators` module with span generators. The generators exist but are only used in non-property `hspec` tests.

**Recommended fix:** Replace timing-dependent tests with explicit synchronization (use `MVar` or `TMVar` to signal when the worker has processed a batch). Add property tests for:
- `parseTraceparent . emitTraceparent = PropagationSuccess` (roundtrip)
- `SpanAttrs` semigroup laws
- `BatchConfig` validator completeness (all invalid configs produce `Left`)

---

## L4 — `StatusOk` encodes as code 1, but OTLP spec uses code 2 for Error [NEW]

**Location:** `Trace.Export.Otlp`, `encodeStatus`

**Confirmed code:**
```haskell
encodeStatus = \case
  StatusUnset      -> object ["code" .= (0 :: Int)]
  StatusOk         -> object ["code" .= (1 :: Int)]
  StatusError em   -> object ["code" .= (2 :: Int), "message" .= ...]
```

Per the [OpenTelemetry Protocol specification](https://opentelemetry.io/docs/specs/otlp/) and the proto definition for `Status`:
- `STATUS_CODE_UNSET = 0`
- `STATUS_CODE_OK = 1`
- `STATUS_CODE_ERROR = 2`

The encoding is **correct** as written. This finding from any reviewer claiming 1 is wrong should be dismissed. However: the OTLP JSON format encodes these as strings (`"STATUS_CODE_UNSET"`, `"STATUS_CODE_OK"`, `"STATUS_CODE_ERROR"`) in some collector implementations, and as integers in others. The current implementation uses integers, which is correct for the protobuf-JSON mapping but may cause silent rejection by collectors that expect string enums.

**Recommended fix:** Add a test using a real collector snapshot or a golden test against a known-good OTLP payload to verify end-to-end encoding. The `test/snapshots/otlp-single-span.json` file is currently empty — populate it and use it as a golden test.

---

## L5 — `Resource` attributes are not attached to exported spans [NEW]

**Location:** `Trace.Config`, `Trace.Monad`, `Trace.Export.Otlp`

**Confirmed:** `TracingConfig` has a `configResource :: Resource` field. `withTracing` constructs a `Tracer` but does not thread `configResource` into the tracer or the exporter. `encodeOtlp` builds `resourceSpans` but does not populate the `resource` field:

```haskell
encodeOtlp spans = object
  [ "resourceSpans" .= [ object
      [ "scopeSpans" .= ...
        -- No "resource" key here
      ]
    ]
  ]
```

The `resource` object in an OTLP payload is the standard way to attach `service.name`, SDK attributes, and deployment environment to all spans from a process. Without it, all spans from htrace appear with no resource identity in the collector. This makes them difficult to filter, attribute, or correlate in Jaeger, Tempo, or any OTLP backend.

This is not a crash or a data loss issue, but it means the `Resource` configuration is entirely non-functional today — a silent no-op.

**Recommended fix:** Thread `Resource` through `Tracer` and include it in `encodeOtlp`:
```haskell
encodeOtlp :: Resource -> [FinishedSpan] -> Value
encodeOtlp resource spans = object
  [ "resourceSpans" .= [ object
      [ "resource"   .= encodeResource resource
      , "scopeSpans" .= ...
      ]
    ]
  ]
```

---

## L6 — `InstrumentationScope` is hardcoded in `withTracing` [NEW]

**Location:** `Trace.Monad`, `withTracing`

**Confirmed code:**
```haskell
tracerScope = InstrumentationScope "htrace" (Just "0.1.0.0")
```

The instrumentation scope (library name and version) is hardcoded to htrace's own identity. Users of htrace who want spans attributed to their own service library (standard OTel practice) cannot configure this. A tracing library should set its scope to the *library doing the instrumentation*, not to itself.

**Recommended fix:** Add `configScope :: InstrumentationScope` to `TracingConfig` and default it to `InstrumentationScope "htrace" (Just "0.1.0.0")`. Also, `encodeOtlp` needs to emit `scope` under `scopeSpans` using this value — currently `scopeSpans` omits the `scope` field entirely.

---

# Summary Table

| ID | Severity | Module | Finding | Verified |
|---|---|---|---|---|
| C1 | Critical | Batch | Timeout-via-race does not bound FFI/blocked exporters | ✓ |
| C2 | Critical | Batch | Shutdown blocks forever if exporter hangs | ✓ |
| C3 | Critical | Batch | Workers are unsupervised `forkIO` threads | ✓ |
| C4 | Critical | Monad | `recordException` is non-atomic (two separate STM txns) | ✓ NEW promoted |
| H1 | High | Batch | Queue drop silently returns `ExportSuccess`; `dropChan` also lossy | ✓ extended |
| H2 | High | Batch | Export failures discard spans with no retry | ✓ |
| H3 | High | Otlp | `GzipCompression` exported but not implemented | ✓ |
| H4 | High | Batch | No internal metrics/observability for telemetry pipeline | ✓ |
| H5 | High | Otlp+Batch | Double-timeout layers interact silently; invariant undocumented | NEW |
| M2 | Medium | Core/Monad | `modifySpan` in public API; no mutation ownership discipline | ✓ extended |
| M3 | Medium | Propagation | `tracestate` silently dropped; breaks upstream vendor chains | ✓ extended |
| M4 | Medium | Propagation | Future-version parsing over-permissive; no warning emitted | ✓ |
| M5 | Medium | Types | Post-shutdown use of exporter is UB with no runtime guard | ✓ extended |
| M6 | Medium | Monad | No orphan span detection or max-duration enforcement | ✓ |
| M7 | Medium | Attributes | `Semigroup` right-bias via `Map.union b a` is subtly fragile | NEW |
| M8 | Medium | Monad | `inSpanCore` exports synchronously if given a non-batched exporter | NEW |
| M9 | Medium | Otlp | Hand-rolled base16 encoder; slow, unnecessary | NEW |
| M10 | Medium | Config | `OTEL_TRACES_SAMPLER_ARG=0` gives misleading `MissingRequiredVar` | NEW |
| L1 | Mild | Config | Custom `Validation` applicative | ✓ |
| L2 | Mild | bench/ | Benchmarks are empty stubs | ✓ |
| L3 | Mild | test/ | Timing-dependent tests; no property tests despite generators | ✓ extended |
| L4 | Mild | Otlp | Status codes correct but golden test file is empty | NEW |
| L5 | Mild | Otlp | `Resource` config is threaded nowhere; absent from OTLP output | NEW |
| L6 | Mild | Monad | `InstrumentationScope` hardcoded; `scope` missing from `scopeSpans` | NEW |

---

# Prior Audit Corrections

The following prior audit claims require correction or clarification:

**C1 fix suggestion** — `withAsync` + `cancel` is insufficient as a complete fix. See C1 above.

**C3 fix suggestion** — Linked workers (`async` with linking) will crash the application on worker death. Supervision with restart semantics is the correct model, not linking.

**M1 reclassification** — `recordException` non-atomicity is Critical, not Medium. It affects every error path under concurrency.

**L4 status codes** — Encoding `StatusOk = 1`, `StatusError = 2` is correct per OTLP spec. The prior audit is silent on this; it is not a bug.

**M4 framing** — The future-version forward-compatibility behavior is per-spec, not a bug. The concern is about the absence of a warning, not the behavior itself.

---

# Mandatory Before Mission-Critical Deployment

Listed in priority order. Each item has a direct safety consequence.

1. Fix `recordException` atomicity (C4) — one-line fix with high correctness impact
2. Add shutdown deadline to `doShutdown` (C2) — prevents application hang
3. Replace `race`-based timeout with supervised async (C1) — prevents thread leaks
4. Supervised worker restart with health reporting (C3) — prevents silent telemetry death
5. Attach `Resource` to OTLP output (L5) — without this, all spans are identity-less
6. Guard post-shutdown exporter use with a `TVar` flag (M5) — eliminates UB
7. Fix `recordException` to be one STM transaction (C4 fix)
8. Document and validate `otlpTimeout < exportTimeout` invariant (H5)

# Strongly Recommended

1. Retry pipeline with backoff (H2)
2. `ExportPartial` result variant; monotonic drop counter (H1)
3. Implement or remove `GzipCompression` (H3)
4. Internal metrics record (`TelemetryMetrics`) (H4)
5. Preserve and forward `tracestate` (M3)
6. Populate `InstrumentationScope` in `scopeSpans` output (L6)
7. Fix `OTEL_TRACES_SAMPLER_ARG=0` error message (M10)
8. Replace hand-rolled base16 with `base16-bytestring` (M9)
9. Populate golden test snapshot for OTLP encoding (L4)

---

# Positive Engineering Observations

These are genuine strengths, not boilerplate:

- The `Validation` applicative in `Config` accumulates all errors correctly rather than short-circuiting. This is the right choice for configuration loading.
- `redactHeaders` in `Otlp` demonstrates good operational security hygiene — credential-bearing header names are proactively redacted in `Show` output.
- `safeLog` wrapping the logger in a `try` block prevents logger failures from crashing the worker. This is the correct defensive pattern.
- `traceIdRatioSampler` uses `Integer` arithmetic to avoid `Double` precision loss in the threshold computation. This is subtle and correct.
- `mkSpanName`, `mkErrorMessage`, `mkEndpoint`, `mkSampleRate` — all constructors are validated. The "smart constructor" discipline is consistent.
- `bracket`-based span lifecycle in `inSpanCore` guarantees spans are always finalized even on exception. This is the most important correctness property for a tracing library.
- `SpanDropped` state in the span lifecycle is explicit and propagated correctly.
- `parentBasedSampler` correctly inherits the parent's sampling decision rather than re-sampling.
- The test suite, while incomplete for concurrency, has good coverage of the happy paths and several failure paths (timeout logging, exception logging, drop callbacks).

---

# Final Assessment

| Environment | Prior Assessment | Revised Assessment |
|---|---|---|
| Internal tooling | Suitable | Suitable |
| Developer observability | Suitable | Suitable |
| Standard SaaS | Reasonable with monitoring | Reasonable — fix C4, H5, L5 first |
| Large-scale cloud | Requires hardening | Requires hardening (C1-C3 mandatory) |
| Financial infrastructure | Not yet suitable | Not yet suitable |
| Defence / safety-critical | Not suitable | Not suitable |

The revised assessment is substantially the same as the prior audit. The new findings (H5, M7-M10, L4-L6) add surface area to the work required but do not change the top-level verdict. The most impactful single change that could be made today — before any architecture work — is fixing `recordException` atomicity (C4) and attaching `Resource` to OTLP output (L5). Both are small, safe, targeted changes with immediate operational value.
