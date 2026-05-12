# htrace — Audit Remediation Guide

**Project:** htrace v0.1.0.0  
**Prepared for:** Mission-critical deployment (health, defence, finance)  
**Audit source:** Final Production Readiness Audit, May 2026  
**Scope:** Every confirmed issue — Critical, High, Medium, and Mild — with production-grade solutions grounded in the actual source code.

> **How to use this document.** Each section names the issue exactly as the audit describes it, states the module and line-level location, explains the risk in plain terms, and provides a complete, self-contained code fix. All fixes are derived strictly from the audited source. No issue is omitted.

---

## Table of Contents

1. [C1 — Export timeout does not bound execution](#c1)
2. [C2 — Shutdown blocks forever on exporter hang](#c2)
3. [C3 — Background workers are unsupervised](#c3)
4. [C4 — `recordException` is non-atomic](#c4)
5. [H1 — Queue drop is silent to the operator](#h1)
6. [H2 — Export failures permanently discard spans](#h2)
7. [H3 — `GzipCompression` advertised but not implemented](#h3)
8. [H4 — Internal telemetry subsystem has no observability](#h4)
9. [H5 — Double-timeout layers interact silently](#h5)
10. [M2 — `modifySpan` in the public API](#m2)
11. [M3 — `tracestate` silently dropped](#m3)
12. [M4 — Future-version `traceparent` parsing emits no warning](#m4)
13. [M5 — Post-shutdown exporter use is undefined behaviour](#m5)
14. [M6 — Orphan span detection absent](#m6)
15. [M7 — `SpanAttrs` `Semigroup` instance is subtly fragile](#m7)
16. [M8 — `inSpanCore` can export synchronously on the user thread](#m8)
17. [M9 — Hand-rolled `base16` encoder](#m9)
18. [M10 — `OTEL_TRACES_SAMPLER_ARG=0` gives misleading error](#m10)
19. [L1 — Custom `Validation` applicative has no property tests](#l1)
20. [L2 — Benchmarks are empty stubs](#l2)
21. [L3 — Timing-dependent tests; no property tests](#l3)
22. [L4 — Golden OTLP snapshot file is empty](#l4)
23. [L5 — `Resource` config is never attached to OTLP output](#l5)
24. [L6 — `InstrumentationScope` is hardcoded](#l6)

---

<a name="c1"></a>
## C1 — Export timeout does not bound execution

**Severity:** Critical  
**Module:** `Trace.Export.Batch` — `worker` and `doFlush`

### Risk

The current code uses `race (threadDelay timeoutMicros) (exporterExport ...)`. `race` cancels the loser by throwing `AsyncCancelled`. This only works if the losing thread is in an *interruptible* blocking state. The OTLP exporter calls `httpLbs`, which internally holds TLS state and kernel socket buffers in non-interruptible sections. A stalled TLS handshake will **not** respond to `AsyncCancelled`. The goroutine lives forever, accumulating memory and threads, and feeds the shutdown deadlock described in C2.

### Fix

Replace the `race`-based timeout in `worker` and `doFlush` with `System.Timeout.timeout`. Add an `exporterIsInterruptible :: Bool` field to `SpanExporter` so the batch layer can assert the contract at construction time. Rely on the inner exporter's own HTTP-level timeout (set in `OtlpConfig`) as the primary mechanism, with the batch timeout as a hard safety net.

**`src/Trace/Export/Types.hs`** — add the field to `SpanExporter`:

```haskell
-- | A record-of-functions representing a span export backend.
-- All three operations must be safe to call from any thread.
data SpanExporter = SpanExporter
  { exporterExport          :: NonEmpty FinishedSpan -> IO ExportResult
    -- ^ Export a non-empty batch of finished spans.
  , exporterFlush           :: IO (Either ExportError ())
    -- ^ Block until all buffered spans have been delivered.
  , exporterShutdown        :: IO ()
    -- ^ Release resources. After shutdown, 'exporterExport' returns
    --   'ExportFailure ExporterShutDown' (see M5).
  , exporterIsInterruptible :: !Bool
    -- ^ 'True' iff the exporter responds promptly to async exceptions.
    -- The batch layer logs a warning at construction time when this is
    -- 'False' and no inner HTTP-level timeout is configured, because
    -- the batch-level timeout cannot reliably bound a non-interruptible
    -- exporter.
  }
```

Update `noopExporter` and `memoryExporter` to set `exporterIsInterruptible = True`.

**`src/Trace/Export/Otlp.hs`** — set `exporterIsInterruptible = False` (http-client is not fully interruptible) and rely on `responseTimeout`:

```haskell
otlpExporter :: OtlpConfig -> IO (Either ExporterInitError SpanExporter)
otlpExporter cfg = do
  case validateHeaders (otlpHeaders cfg) of
    Left e   -> pure (Left e)
    Right hs -> do
      mgr  <- newManager tlsManagerSettings
      req0 <- parseRequest (Text.unpack (unEndpoint (otlpEndpoint cfg)))
      let req = req0
            { method         = "POST"
            , requestHeaders =
                (CI.mk "content-type", "application/json") : hs
            , responseTimeout =
                responseTimeoutMicro
                  (round (otlpTimeout cfg * 1_000_000))
            }
      pure $ Right $ SpanExporter
        { exporterExport          = doExport mgr req
        , exporterFlush           = pure (Right ())
        , exporterShutdown        = pure ()
        , exporterIsInterruptible = False
          -- http-client holds non-interruptible TLS/socket sections.
          -- The responseTimeout above is the primary execution bound.
        }
```

**`src/Trace/Export/Batch.hs`** — replace `race` with `System.Timeout.timeout` in `worker` and `doFlush`, and warn at construction if the inner exporter is non-interruptible:

```haskell
import System.Timeout (timeout)

-- In batchExporter, after validateBatchConfig:
batchExporter cfg inner = do
  case validateBatchConfig cfg of
    Just e  -> pure (Left e)
    Nothing -> do
      -- Warn operators of the non-interruptible combination early.
      when (not (exporterIsInterruptible inner)) $
        logWarn (batchLogger cfg)
          "htrace: inner exporter is non-interruptible; \
          \batch timeout acts as a safety net only. \
          \Ensure the inner exporter has its own HTTP-level timeout \
          \strictly less than exportTimeout."
      -- ... rest of construction unchanged ...
```

Inside `worker`, replace the `race`-based export call:

```haskell
-- BEFORE (unsafe):
raceResult <- race
  (threadDelay timeoutMicros)
  (try (exporterExport inner ne) :: IO (Either SomeException ExportResult))

-- AFTER (correct):
mResult <- timeout timeoutMicros
             (try (exporterExport inner ne) :: IO (Either SomeException ExportResult))
case mResult of
  Nothing ->
    safeLog logWarn
      (  "htrace: export timed out after "
      <> Text.pack (show (exportTimeout cfg))
      <> "s; "
      <> Text.pack (show (NE.length ne))
      <> " spans abandoned"
      )
  Just (Left ex) ->
    safeLog logError
      (  "htrace: exporter threw exception: "
      <> Text.pack (show ex)
      )
  Just (Right (ExportFailure err)) ->
    safeLog logWarn
      (  "htrace: export returned failure: "
      <> Text.pack (show err)
      )
  Just (Right (ExportSuccess _)) ->
    pure ()
```

Apply the same replacement in `doFlush`:

```haskell
doFlush queue = do
  batch <- atomically (drainBatch queue (maxQueueSize cfg))
  case NE.nonEmpty batch of
    Nothing -> pure (Right ())
    Just ne -> do
      let timeoutUs = round (realToFrac (exportTimeout cfg) * 1_000_000 :: Double)
      mResult <- timeout timeoutUs (exporterExport inner ne)
      case mResult of
        Nothing                -> pure (Left (ExportTimeout (exportTimeout cfg)))
        Just (ExportSuccess _) -> pure (Right ())
        Just (ExportFailure e) -> pure (Left e)
```

---

<a name="c2"></a>
## C2 — Shutdown blocks forever on exporter hang

**Severity:** Critical  
**Module:** `Trace.Export.Batch` — `doShutdown`

### Risk

`doShutdown` signals shutdown then blocks on two `takeTMVar` calls waiting for both worker threads to finish. If the worker is inside a hung exporter call, it never reaches `putTMVar done ()` and the entire shutdown hangs indefinitely. In a containerised deployment this means the process is killed by the orchestrator's SIGKILL after the grace period — spans buffered at shutdown are silently lost.

Additionally, `exporterShutdown inner` is called *after* waiting for both threads, meaning a hung inner-exporter shutdown also stalls the process.

### Fix

Add a `shutdownTimeout :: NominalDiffTime` field to `BatchConfig` (default 5 seconds). Apply `System.Timeout.timeout` to the entire shutdown sequence. Log a warning and abandon the worker if the deadline is exceeded.

**`src/Trace/Export/Batch.hs`** — update `BatchConfig` and `defaultBatchConfig`:

```haskell
data BatchConfig = BatchConfig
  { maxQueueSize    :: !Int
  , maxExportBatch  :: !Int
  , exportInterval  :: !NominalDiffTime
  , exportTimeout   :: !NominalDiffTime
  , shutdownTimeout :: !NominalDiffTime
    -- ^ Total deadline for clean shutdown, including waiting for the
    --   worker thread and calling 'exporterShutdown' on the inner exporter.
    --   Defaults to 5 seconds. Must be > 'exportTimeout' in practice.
  , onDroppedSpans  :: !(Int -> IO ())
  , batchLogger     :: !InternalLogger
  }

defaultBatchConfig :: BatchConfig
defaultBatchConfig = BatchConfig
  { maxQueueSize    = 2048
  , maxExportBatch  = 512
  , exportInterval  = 5
  , exportTimeout   = 30
  , shutdownTimeout = 5
  , onDroppedSpans  = defaultOnDroppedSpans stderrLogger
  , batchLogger     = stderrLogger
  }
```

Replace `doShutdown`:

```haskell
import System.Timeout (timeout)

doShutdown _queue shutdownVar workerDone notifierDone = do
  atomically (writeTVar shutdownVar True)
  let deadlineUs =
        round (realToFrac (shutdownTimeout cfg) * 1_000_000 :: Double)
  mWorker <- timeout deadlineUs $
    atomically $ do
      takeTMVar workerDone
      takeTMVar notifierDone
  case mWorker of
    Nothing ->
      logWarn (batchLogger cfg)
        "htrace: shutdown timed out waiting for worker; \
        \abandoning background threads. Some spans may be lost."
    Just () -> pure ()
  -- Best-effort inner shutdown — also bounded.
  let innerDeadlineUs = deadlineUs `div` 2
  mInner <- timeout innerDeadlineUs (exporterShutdown inner)
  case mInner of
    Nothing ->
      logWarn (batchLogger cfg)
        "htrace: inner exporter shutdown timed out; \
        \resources may not be fully released."
    Just () -> pure ()
```

Also update `validateBatchConfig` to reject `shutdownTimeout <= 0`:

```haskell
| shutdownTimeout c <= 0 =
    Just (NonPositiveTimeout (shutdownTimeout c))
```

---

<a name="c3"></a>
## C3 — Background workers are unsupervised

**Severity:** Critical  
**Module:** `Trace.Export.Batch`

### Risk

Both worker threads are started with bare `forkIO`. Any uncaught exception silently kills the thread. The `TMVar` slots are never filled on abnormal death, which directly causes C2's shutdown deadlock. The application continues running, instruments spans, enqueues them to the queue — and nothing is ever exported. There is no signal that this has happened.

### Fix

Introduce a `WorkerHealth` type and a `supervisedWorker` function that restarts the loop on failure (up to a configurable retry budget) and marks the worker dead after exhausting retries. Expose a `batchExporterHealth` accessor for operator health checks (monitoring, `/health` endpoints, etc.).

**`src/Trace/Export/Batch.hs`**:

```haskell
import Data.Text qualified as Text
import Control.Exception (SomeException, try)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (TVar, newTVarIO, atomically, writeTVar, readTVarIO)

-- | Coarse health state of a supervised batch worker.
data WorkerHealth
  = WorkerHealthy
  | WorkerDegraded !Text   -- ^ Last exception; worker is restarting.
  | WorkerDead             -- ^ Retry budget exhausted; no more exports.
  deriving stock (Show, Eq)

-- | Run 'loop' under supervision. On exception: log, back off, retry.
-- After 'maxRetries' consecutive failures, mark the health TVar as
-- 'WorkerDead' and return — do NOT re-throw, to avoid crashing the app.
supervisedWorker
  :: InternalLogger
  -> TVar WorkerHealth
  -> Int           -- ^ Maximum consecutive restarts before giving up.
  -> IO ()         -- ^ The work loop (should only return on clean shutdown).
  -> IO ()
supervisedWorker logger health maxRetries loop = go maxRetries
  where
    go 0 = do
      atomically (writeTVar health WorkerDead)
      logError logger
        "htrace: worker exhausted restart budget; telemetry export is dead."
    go n = do
      result <- try loop
      case result of
        Right ()             -> pure ()  -- Clean exit via shutdown path.
        Left (e :: SomeException) -> do
          let msg = Text.pack (show e)
          atomically (writeTVar health (WorkerDegraded msg))
          logWarn logger
            ("htrace: worker crashed (" <> msg <> "); restarting in 100ms. \
             \Restarts remaining: " <> Text.pack (show (n - 1)))
          threadDelay 100_000
          go (n - 1)
```

Update `batchExporter` to use supervised workers and return the health TVars:

```haskell
-- Return type extended to include health accessors:
data BatchExporter = BatchExporter
  { batchSpanExporter :: !SpanExporter
  , batchWorkerHealth :: !(IO WorkerHealth)
  , batchNotifierHealth :: !(IO WorkerHealth)
  }

batchExporter
  :: BatchConfig
  -> SpanExporter
  -> IO (Either BatchConfigError BatchExporter)
batchExporter cfg inner =
  case validateBatchConfig cfg of
    Just e  -> pure (Left e)
    Nothing -> do
      queue          <- newTBQueueIO (fromIntegral (maxQueueSize cfg))
      dropChan       <- newTBQueueIO 64
      shutdownVar    <- newTVarIO False
      workerDone     <- newEmptyTMVarIO
      notifierDone   <- newEmptyTMVarIO
      workerHealth   <- newTVarIO WorkerHealthy
      notifierHealth <- newTVarIO WorkerHealthy

      _ <- forkIO $
        supervisedWorker (batchLogger cfg) workerHealth 3
          (worker queue shutdownVar workerDone)
          `finally` atomically (tryPutTMVar workerDone ())

      _ <- forkIO $
        supervisedWorker (batchLogger cfg) notifierHealth 3
          (notifier dropChan shutdownVar notifierDone)
          `finally` atomically (tryPutTMVar notifierDone ())

      let exporter = SpanExporter
            { exporterExport          = enqueue queue dropChan
            , exporterFlush           = doFlush queue
            , exporterShutdown        = doShutdown queue shutdownVar
                                          workerDone notifierDone
            , exporterIsInterruptible = False
            }
      pure $ Right $ BatchExporter
        { batchSpanExporter   = exporter
        , batchWorkerHealth   = readTVarIO workerHealth
        , batchNotifierHealth = readTVarIO notifierHealth
        }
```

Note: `finally` ensures the `TMVar` is always filled even on death, preventing the C2 deadlock.

Import `Control.Exception (finally)` or `UnliftIO.Exception (finally)` — whichever is already in scope for the module.

---

<a name="c4"></a>
## C4 — `recordException` is non-atomic

**Severity:** Critical  
**Module:** `Trace.Monad` — `recordException`

### Risk

`recordException` calls `setStatusError` and then `addEvent` as two separate STM transactions. Between the two commits, any thread reading the span sees an inconsistent snapshot: `StatusError` set, but no `exception` event yet (or the reverse under preemption). For every error path in an instrumented service, the span's observable state is momentarily corrupt. In high-concurrency services (the primary target for this library) this race is reproducible under load.

### Fix

Collapse both mutations into a single `modifySpan` call so they are committed in one atomic STM transaction.

**`src/Trace/Monad.hs`** — replace `recordException`:

```haskell
-- | Record an exception as a span event and set the status to error,
-- atomically. Both the StatusError and the exception event are written
-- in a single STM transaction and are always visible together.
--
-- Follows the OpenTelemetry semantic conventions for exception events.
recordException
  :: Exception e => Span -> e -> IO (Either SpanError ())
recordException sp e = do
  now <- clockNow (spanClock sp)
  let msg     = Text.pack (displayException e)
      evAttrs = attrs
        [ ( AttrKey "exception.type"
          , AttrString (Text.pack (show (typeOf e))) )
        , ( AttrKey "exception.message"
          , AttrString msg )
        ]
      errMsg = case mkErrorMessage msg of
        Just m  -> m
        Nothing -> unspecifiedErrorMessage
  -- Single STM transaction: status and event are always consistent.
  modifySpan sp $ \si ->
    si { siStatus = StatusError errMsg
       , siEvents = SpanEvent "exception" now evAttrs : siEvents si
       }
```

This replaces the two-call sequence with one `modifySpan`, removing the race window entirely.

---

<a name="h1"></a>
## H1 — Queue drop is silent to the operator

**Severity:** High  
**Module:** `Trace.Export.Batch` — `enqueue`

### Risk

When the queue is full and spans are dropped, `enqueue` returns `ExportSuccess (n - dropped)`. A caller that submitted 10 spans and had 7 dropped receives `ExportSuccess 3`. The name `ExportSuccess` falsely signals full delivery. Additionally, the `dropChan` used to notify the `onDroppedSpans` callback is itself bounded at 64; under sustained saturation its own overflow silently discards drop notifications, making the operator's counter an undercount.

### Fix

Introduce `ExportPartial` to the result type. Replace the `dropChan` indirection with a monotonic `TVar Int` counter that is incremented atomically and never lossy. Expose the counter via the `TelemetryMetrics` record introduced in H4.

**`src/Trace/Export/Types.hs`** — add `ExportPartial`:

```haskell
data ExportResult
  = ExportSuccess !Int
    -- ^ All submitted spans were accepted. The 'Int' is the count accepted.
  | ExportPartial !Int !Int
    -- ^ Some spans were accepted, some were dropped due to queue full.
    --   First 'Int': accepted count. Second 'Int': dropped count.
  | ExportFailure !ExportError
    -- ^ Export failed with the given error.
  deriving stock (Show, Eq)
```

**`src/Trace/Export/Batch.hs`** — replace `dropChan` with a `TVar`:

```haskell
-- In batchExporter construction:
totalDropped <- newTVarIO (0 :: Int)

-- Replace enqueue:
enqueue queue totalDropped ne = do
  let xs = NE.toList ne
      n  = length xs
  (canEnqueue, nDropped) <- atomically $ do
    occupied <- lengthTBQueue queue
    let space      = fromIntegral (maxQueueSize cfg) - fromIntegral occupied
        canEnq     = min n space
        nDrop      = n - canEnq
    mapM_ (writeTBQueue queue) (take canEnq xs)
    when (nDrop > 0) $
      modifyTVar' totalDropped (+ nDrop)
    pure (canEnq, nDrop)
  when (nDropped > 0) $
    -- Invoke the callback outside the STM transaction.
    onDroppedSpans cfg nDropped
  if nDropped == 0
    then pure (ExportSuccess canEnqueue)
    else pure (ExportPartial canEnqueue nDropped)
```

The notifier thread and `dropChan` can be removed entirely once this change is made, simplifying the shutdown path (eliminates one of the two `TMVar` waits in `doShutdown`).

---

<a name="h2"></a>
## H2 — Export failures permanently discard spans

**Severity:** High  
**Module:** `Trace.Export.Batch` — `worker`

### Risk

Spans are drained from the queue *before* the export attempt. On any failure (timeout, exception, `ExportFailure`) the batch is logged and discarded. There is no retry. For a transient network partition, or a collector restart, every span produced during the outage is permanently lost. In financial audit trails or medical event logs, this is unacceptable.

### Fix

Retain a reference to the drained batch through the export attempt. On transient failure, re-enqueue at the front with exponential backoff and a per-batch retry budget. Add a `RetryConfig` to `BatchConfig`.

**`src/Trace/Export/Batch.hs`**:

```haskell
import Data.Time.Clock (UTCTime, getCurrentTime, addUTCTime)

data RetryConfig = RetryConfig
  { retryMaxAttempts  :: !Int
    -- ^ Maximum export attempts per batch (including the first). Default 3.
  , retryBaseDelay    :: !NominalDiffTime
    -- ^ Base delay before first retry. Each subsequent retry doubles this.
    --   Default 1 second.
  } deriving stock (Show, Eq)

defaultRetryConfig :: RetryConfig
defaultRetryConfig = RetryConfig
  { retryMaxAttempts = 3
  , retryBaseDelay   = 1
  }

-- Add to BatchConfig:
-- , retryConfig :: !RetryConfig

-- In worker, replace the export call with:
exportWithRetry
  :: BatchConfig
  -> NonEmpty FinishedSpan
  -> Int            -- ^ attempts remaining
  -> NominalDiffTime -- ^ current delay
  -> IO ()
exportWithRetry cfg ne attemptsLeft delay = do
  let timeoutUs = round (realToFrac (exportTimeout cfg) * 1_000_000 :: Double)
  mResult <- timeout timeoutUs
    (try (exporterExport inner ne) :: IO (Either SomeException ExportResult))
  case mResult of
    Just (Right (ExportSuccess _)) -> pure ()
    Just (Right (ExportPartial _ _)) -> pure ()  -- partial accepted; no retry
    other -> do
      let reason = case other of
            Nothing                        -> "timeout"
            Just (Left ex)                 -> Text.pack (show ex)
            Just (Right (ExportFailure e)) -> Text.pack (show e)
            _                              -> "unknown"
      safeLog logWarn
        ("htrace: export failed (" <> reason <> "); "
          <> Text.pack (show (NE.length ne)) <> " spans. "
          <> Text.pack (show (attemptsLeft - 1)) <> " retries remaining.")
      if attemptsLeft <= 1
        then safeLog logError
               ("htrace: batch exhausted retries; "
                 <> Text.pack (show (NE.length ne))
                 <> " spans permanently lost.")
        else do
          let delayUs = round (realToFrac delay * 1_000_000 :: Double)
          threadDelay delayUs
          exportWithRetry cfg ne (attemptsLeft - 1) (delay * 2)
```

Call `exportWithRetry cfg ne (retryMaxAttempts (retryConfig cfg)) (retryBaseDelay (retryConfig cfg))` in the worker loop instead of the bare export call.

---

<a name="h3"></a>
## H3 — `GzipCompression` advertised but not implemented

**Severity:** High  
**Module:** `Trace.Export.Otlp`

### Risk

`GzipCompression` is exported in the public API. Any operator who sets `otlpCompression = GzipCompression` gets no compression and no error. In environments where the collector is configured to *require* compressed payloads, every export will fail silently. This is a silent misconfiguration trap.

### Fix

Until gzip is implemented: return a clear construction-time error when `GzipCompression` is requested. This makes the limitation explicit and prevents silent misconfiguration.

**`src/Trace/Export/Otlp.hs`**:

```haskell
-- Add to ExporterInitError in Types.hs:
-- | ExporterCompressionUnsupported !Text

-- In otlpExporter, after validateHeaders:
otlpExporter cfg = do
  case otlpCompression cfg of
    GzipCompression ->
      pure (Left (ExporterCompressionUnsupported
        "GzipCompression is not yet implemented in v0.1.0.0. \
        \Use NoCompression or wait for the next release."))
    NoCompression -> do
      -- ... existing construction code unchanged ...
```

Add to `ExporterInitError` in `src/Trace/Export/Types.hs`:

```haskell
data ExporterInitError
  = ExporterInvalidEndpoint            !Text
  | ExporterInvalidHeader              !Text !Text
  | ExporterUnsupportedScheme          !Text
  | ExporterBatchInit                  !BatchConfigError
  | ExporterCompressionUnsupported     !Text   -- ^ NEW: feature not yet available
  deriving stock (Show, Eq)
```

When gzip is implemented, the correct approach is:

```haskell
-- Implementation sketch for when gzip is added:
import Codec.Compression.GZip qualified as GZip

applyCompression :: Compression -> LBS.ByteString -> (LBS.ByteString, [(CI.CI ByteString, ByteString)])
applyCompression NoCompression  body = (body, [])
applyCompression GzipCompression body =
  (GZip.compress body, [(CI.mk "content-encoding", "gzip")])

-- In doExport:
let (compressedBody, compressionHeaders) = applyCompression (otlpCompression cfg) body
    req' = req
      { requestBody    = RequestBodyLBS compressedBody
      , requestHeaders = requestHeaders req <> compressionHeaders
      }
```

---

<a name="h4"></a>
## H4 — Internal telemetry subsystem has no observability

**Severity:** High  
**Module:** `Trace.Export.Batch`, `Trace.Export.Types`

### Risk

There are no machine-readable metrics for queue depth, export latency, timeout rate, drop rate, or worker liveness. Operators cannot wire htrace's own health into their monitoring infrastructure. In a regulated environment, the observability stack must itself be observable.

### Fix

Add a `TelemetryMetrics` record populated by `TVar` counters inside the batch worker. Expose it alongside the exporter so it can be wired to a `/metrics` or `/health` endpoint.

**`src/Trace/Export/Types.hs`** — add the metrics type:

```haskell
-- | Machine-readable health metrics for the htrace export pipeline.
-- All fields are 'IO' actions that read the current value of an
-- underlying 'TVar'. Safe to call from any thread at any time.
data TelemetryMetrics = TelemetryMetrics
  { metricsQueueDepth     :: IO Int
    -- ^ Current number of spans in the export queue.
  , metricsExportedTotal  :: IO Int
    -- ^ Cumulative spans successfully exported since process start.
  , metricsDroppedTotal   :: IO Int
    -- ^ Cumulative spans dropped due to queue full since process start.
  , metricsExportErrors   :: IO Int
    -- ^ Cumulative export attempts that returned 'ExportFailure'.
  , metricsExportTimeouts :: IO Int
    -- ^ Cumulative export attempts that timed out.
  , metricsWorkerLive     :: IO Bool
    -- ^ 'True' iff the worker thread is healthy or recovering.
  }

-- | A no-op metrics record that always returns zero / True.
-- Use in contexts where metrics are not needed (e.g. tests).
noopMetrics :: TelemetryMetrics
noopMetrics = TelemetryMetrics
  { metricsQueueDepth     = pure 0
  , metricsExportedTotal  = pure 0
  , metricsDroppedTotal   = pure 0
  , metricsExportErrors   = pure 0
  , metricsExportTimeouts = pure 0
  , metricsWorkerLive     = pure True
  }
```

**`src/Trace/Export/Batch.hs`** — add `TVar` counters and wire them up:

```haskell
-- In batchExporter construction, after queue creation:
exportedTotal  <- newTVarIO (0 :: Int)
droppedTotal   <- newTVarIO (0 :: Int)  -- replaces dropChan (see H1)
exportErrors   <- newTVarIO (0 :: Int)
exportTimeouts <- newTVarIO (0 :: Int)

-- In enqueue, on drop:
modifyTVar' droppedTotal (+ nDrop)

-- In worker, on ExportSuccess:
atomically $ modifyTVar' exportedTotal (+ NE.length ne)

-- On ExportFailure or exception:
atomically $ modifyTVar' exportErrors (+ 1)

-- On timeout:
atomically $ modifyTVar' exportTimeouts (+ 1)

-- Construct the metrics record:
let metrics = TelemetryMetrics
      { metricsQueueDepth     = atomically (lengthTBQueue queue)
                                  >>= \n -> pure (fromIntegral n)
      , metricsExportedTotal  = readTVarIO exportedTotal
      , metricsDroppedTotal   = readTVarIO droppedTotal
      , metricsExportErrors   = readTVarIO exportErrors
      , metricsExportTimeouts = readTVarIO exportTimeouts
      , metricsWorkerLive     = do
          h <- readTVarIO workerHealth
          pure (h /= WorkerDead)
      }
```

Extend `BatchExporter` (see C3) with `batchMetrics :: TelemetryMetrics` and expose it from `withTracing`.

---

<a name="h5"></a>
## H5 — Double-timeout layers interact silently

**Severity:** High  
**Module:** `Trace.Export.Otlp` + `Trace.Export.Batch`

### Risk

There are two independent timeout layers: `otlpTimeout` (HTTP-level, default 10s) and `exportTimeout` (batch-level safety net, default 30s). The correct invariant is `otlpTimeout < exportTimeout`. If an operator sets `otlpTimeout = 60` and leaves `exportTimeout = 30`, the HTTP request is killed by the batch layer first, and `http-client`'s own timeout never fires. This is the broken configuration described in C1. The invariant is nowhere documented, nowhere validated.

### Fix

Validate the relationship at `withTracing` construction time. Log a warning (not an error — the configuration may be intentional) when the invariant is violated. Document it in the types.

**`src/Trace/Monad.hs`** — add a check in `withTracing`:

```haskell
withTracing cfg action = do
  innerR <- case configExporter cfg of
    NoopExporter   -> pure (Right noopExporter)
    OtlpExporter c -> do
      -- Validate timeout relationship before constructing anything.
      let batchCfg = defaultBatchConfig
            { onDroppedSpans = defaultOnDroppedSpans (configLogger cfg)
            , batchLogger    = configLogger cfg
            }
      when (otlpTimeout c >= exportTimeout batchCfg) $
        logWarn (configLogger cfg)
          ("htrace: otlpTimeout (" <> Text.pack (show (otlpTimeout c))
            <> "s) >= exportTimeout ("
            <> Text.pack (show (exportTimeout batchCfg))
            <> "s). The HTTP-level timeout will never fire before the \
               \batch timeout. Set otlpTimeout strictly less than \
               \exportTimeout to ensure the HTTP layer cancels first.")
      otlpExporter c
  -- ... rest unchanged ...
```

Also add a note to the `OtlpConfig` type:

```haskell
data OtlpConfig = OtlpConfig
  { otlpEndpoint    :: !Endpoint
  , otlpHeaders     :: ![(Text, Text)]
  , otlpTimeout     :: !NominalDiffTime
    -- ^ HTTP-level timeout for a single export request.
    -- MUST be strictly less than 'exportTimeout' in 'BatchConfig'.
    -- If otlpTimeout >= exportTimeout, the batch-level timeout fires first
    -- and the HTTP request is abandoned via AsyncCancelled, which may not
    -- interrupt non-interruptible socket operations. Default: 10s.
  , otlpCompression :: !Compression
  }
```

---

<a name="m2"></a>
## M2 — `modifySpan` in the public API

**Severity:** Medium  
**Module:** `Trace.Monad`

### Risk

`modifySpan` is exported from `Trace.Monad` and re-exported from the facade `Trace`. Any code holding a `Span` reference can call `modifySpan` with an arbitrary function, mutating span internals from any thread at any time. There is no ownership discipline. This is a footgun that grows more dangerous as the codebase scales: a library consumer can accidentally corrupt a span that is being concurrently read for export.

### Fix

Remove `modifySpan` from the public module export list. It remains available internally. Only the named mutators (`setSpanAttr`, `setSpanAttrs`, `setSpanStatus`, `setStatusError`, `addEvent`, `recordException`) are public.

**`src/Trace/Monad.hs`** — update the export list:

```haskell
module Trace.Monad
  ( -- * Tracer
    Tracer (..)
    -- * Context and monad
  , TraceContext (..)
  , TraceM
    -- * Span creation
  , inSpan
  , inSpanM
  , inSpanCore
    -- * Mutators (named operations only; modifySpan is internal)
  , setSpanAttr
  , setSpanAttrs
  , setSpanStatus
  , setStatusError
  , addEvent
  , recordException
  , withTracing
  , samplerFromConfig
    -- * Utilities
  , getCurrentSpanContext
  , flush
  -- NOTE: modifySpan is intentionally not exported. Use the named mutators.
  ) where
```

**`src/Trace.hs`** (the facade) — ensure `modifySpan` is absent from its export list.

---

<a name="m3"></a>
## M3 — `tracestate` silently dropped

**Severity:** Medium  
**Module:** `Trace.Propagation`

### Risk

`injectHeaders` propagates only `traceparent`. Any incoming `tracestate` header (carrying vendor-specific sampling decisions, baggage, or routing metadata from upstream systems like Datadog, Honeycomb, or AWS X-Ray) is silently discarded. In a proxy or middleware that receives a distributed trace from an upstream service, htrace will destroy that vendor state on every outbound hop. This is a silent interoperability regression.

### Fix

At minimum, preserve and forward `tracestate` as an opaque value. Parse it as an optional `Text` and carry it through `SpanContext` or pass it through `injectHeaders` as a parameter.

**`src/Trace/Propagation.hs`** — add `tracestate` passthrough:

```haskell
tracestateHeader :: CI.CI ByteString
tracestateHeader = CI.mk "tracestate"

-- | Inject traceparent (and tracestate if present) into a header list.
-- The 'Maybe Text' argument is the raw tracestate value to forward.
-- Pass 'Nothing' for root spans; pass the extracted tracestate for
-- child spans so upstream vendor state is preserved.
injectHeaders :: SpanContext -> Maybe Text -> [Header] -> [Header]
injectHeaders ctx mTracestate hs =
  let base = (traceparentHeader, TE.encodeUtf8 (emitTraceparent ctx))
           : filter ((/= traceparentHeader) . fst)
               (filter ((/= tracestateHeader) . fst) hs)
  in case mTracestate of
       Nothing -> base
       Just ts -> (tracestateHeader, TE.encodeUtf8 ts) : base

-- | Extract SpanContext AND raw tracestate from a header list.
-- Returns '(PropagationResult, Maybe Text)'.
extractContextWithTracestate
  :: [Header]
  -> (PropagationResult, Maybe Text)
extractContextWithTracestate hs =
  let result     = extractContext hs
      mTracestate = do
        bs <- lookup tracestateHeader hs
        case TE.decodeUtf8' bs of
          Left  _ -> Nothing
          Right t -> Just t
  in (result, mTracestate)
```

Update `injectHeaders` callers accordingly. The old `injectHeaders :: SpanContext -> [Header] -> [Header]` signature can be preserved as a compatibility shim defaulting to `Nothing` tracestate.

---

<a name="m4"></a>
## M4 — Future-version `traceparent` parsing emits no warning

**Severity:** Medium  
**Module:** `Trace.Propagation` — `parseTraceparent`

### Risk

When a `traceparent` header with a version other than `00` is received, the current code silently parses the first four fields with v00 semantics. Per the W3C Trace Context spec this is correct behaviour for forward compatibility, but if a future version changes the meaning of existing fields the parser will misinterpret them without any signal. Operators have no way to know their services are receiving newer-format headers.

### Fix

Add a `PropagationFutureVersion` variant to `PropagationResult` so callers can distinguish and log this case. Keep the parsing semantics identical — this is a classification change only, not a behaviour change.

**`src/Trace/Propagation.hs`**:

```haskell
data PropagationResult
  = PropagationSuccess      !SpanContext
    -- ^ A valid v00 context was parsed.
  | PropagationFutureVersion !Text !SpanContext
    -- ^ A future-version header was parsed using v00 field semantics.
    -- The 'Text' is the raw version string. The 'SpanContext' is
    -- best-effort. Callers should log this for operator awareness.
  | PropagationAbsent
    -- ^ No traceparent header was present.
  | PropagationInvalid      !PropagationError
    -- ^ A traceparent header was present but could not be parsed.
  deriving stock (Show, Eq)

-- In parseTraceparent, replace the success branch for non-"00" versions:
parseTraceparent t =
  case Text.splitOn "-" t of
    (v : tid : sid : flgs : _) ->
      if not (validVersion v)
        then PropagationInvalid (InvalidVersion (Text.toLower v))
        else case {- ... decode tid, sid, flgs as before ... -} of
          -- ...
          Just f ->
            let ctx = SpanContext traceId spanId Nothing f
            in if Text.toLower v == "00"
               then PropagationSuccess ctx
               else PropagationFutureVersion v ctx
    _ -> PropagationInvalid (MalformedHeader t)
```

Update `extractContext` to propagate the new variant, and update callers to log a warning when `PropagationFutureVersion` is received.

---

<a name="m5"></a>
## M5 — Post-shutdown exporter use is undefined behaviour

**Severity:** Medium  
**Module:** `Trace.Export.Types`

### Risk

The `exporterShutdown` contract says "subsequent calls to `exporterExport` are undefined behaviour." `withTracing` uses `bracket` correctly, but any code that captures a `Tracer` reference (stores it in an `IORef`, passes it to a long-lived thread, etc.) and calls `flush` or `inSpan` after `withTracing` returns will exhibit undefined behaviour — the exporter may panic, corrupt internal state, or silently do nothing.

### Fix

Add a `TVar Bool` shutdown flag to every exporter. Check it at the start of `exporterExport` and `exporterFlush`. Return a clear `ExportFailure ExporterShutDown` error rather than undefined behaviour. This is a one-line change per operation.

**`src/Trace/Export/Types.hs`** — add `ExporterShutDown` to `ExportError`:

```haskell
data ExportError
  = EndpointUnreachable    !Text
  | MalformedResponse      !HttpStatus !Text
  | ExportTimeout          !NominalDiffTime
  | SerializationFailed    !Text
  | ExporterShutDown
    -- ^ The exporter has been shut down; this call is a no-op.
    --   This is a programming error: callers should not use a Tracer
    --   after 'withTracing' has returned.
  | ExporterCompressionUnsupported !Text
  deriving stock (Show, Eq)
```

**`src/Trace/Export/Batch.hs`** — add the flag to `batchExporter`:

```haskell
-- In batchExporter construction:
isShutDown <- newTVarIO False

-- Guard exporterExport:
exporterExport = \ne -> do
  sd <- readTVarIO isShutDown
  if sd
    then pure (ExportFailure ExporterShutDown)
    else enqueue queue totalDropped ne

-- Guard exporterFlush:
exporterFlush = do
  sd <- readTVarIO isShutDown
  if sd
    then pure (Left ExporterShutDown)
    else doFlush queue

-- Set the flag in doShutdown before returning:
doShutdown _queue shutdownVar workerDone notifierDone = do
  atomically (writeTVar shutdownVar True)
  -- ... timeout logic from C2 fix ...
  atomically (writeTVar isShutDown True)
```

---

<a name="m6"></a>
## M6 — Orphan span detection absent

**Severity:** Medium  
**Module:** `Trace.Monad` — `inSpanCore`

### Risk

A span started on a thread that subsequently deadlocks or runs indefinitely will remain in `SpanActive` state forever, holding a live `TVar` in memory and consuming heap. In `RecordOnly` mode (not sampled), these spans accumulate with no cleanup path. For a long-running service with many short-lived worker threads, orphan spans become a memory leak.

### Fix

Add an optional `maxSpanDuration :: Maybe NominalDiffTime` to `BatchConfig`. The existing worker thread doubles as a watchdog: on each wake cycle, it scans for `SpanActive` spans whose start time exceeds the maximum and marks them `SpanEnded` with a synthetic end time and a warning event.

This requires the batch layer to have visibility into active spans — either via a shared registry `TVar (Set Span)` or by making spans self-register. The cleanest approach is a weak-reference registry.

**`src/Trace/Core.hs`** — add a span registry (sketch):

```haskell
import System.Mem.Weak (Weak, mkWeakTVar, deRefWeak)

-- | A registry of active spans, held as weak references so the registry
-- does not prevent GC of finished spans.
newtype SpanRegistry = SpanRegistry
  { registryWeaks :: TVar [Weak (TVar SpanInternals)] }

newSpanRegistry :: IO SpanRegistry
newSpanRegistry = SpanRegistry <$> newTVarIO []

registerSpan :: SpanRegistry -> Span -> IO ()
registerSpan reg sp = do
  w <- mkWeakTVar (spanInternals sp) (pure ())
  atomically $ modifyTVar' (registryWeaks reg) (w :)

-- Watchdog: called periodically by the batch worker.
reapOrphanSpans
  :: SpanRegistry
  -> NominalDiffTime   -- ^ maxSpanDuration
  -> InternalLogger
  -> IO ()
reapOrphanSpans reg maxDur logger = do
  now   <- getCurrentTime
  weaks <- readTVarIO (registryWeaks reg)
  alive <- fmap concat $ forM weaks $ \w -> do
    mTVar <- deRefWeak w
    case mTVar of
      Nothing   -> pure []  -- GC'd; remove from list.
      Just tvar -> do
        si <- readTVarIO tvar
        case siState si of
          SpanActive start
            | diffUTCTime now start > maxDur -> do
                atomically $ writeTVar tvar $
                  si { siState  = SpanEnded start now
                     , siEvents = SpanEvent
                         "htrace.orphan_span_reaped" now mempty
                         : siEvents si
                     }
                logWarn logger
                  ("htrace: orphan span reaped after "
                    <> Text.pack (show maxDur) <> "s")
                pure []
          _ -> pure [w]
  atomically $ writeTVar (registryWeaks reg) alive
```

Add `maxSpanDuration :: Maybe NominalDiffTime` to `BatchConfig` (default `Nothing`). Call `reapOrphanSpans` in the worker loop when `maxSpanDuration` is `Just`.

---

<a name="m7"></a>
## M7 — `SpanAttrs` `Semigroup` instance is subtly fragile

**Severity:** Medium  
**Module:** `Trace.Attributes`

### Risk

The `Semigroup` instance achieves right-bias via `Map.union b a` (arguments reversed). This is correct but a latent trap: any maintainer reading `Map.union a b` in a future diff will not notice the argument reversal is intentional and the bias is right, not left. A future refactor that "fixes" it to `Map.union a b` silently inverts the merge semantics and breaks all attribute overrides.

### Fix

Replace `Map.union b a` with `Map.unionWith (\_ new -> new) a b`. The combinator makes the right-bias explicit and self-documenting. Add a property test.

**`src/Trace/Attributes.hs`**:

```haskell
instance Semigroup SpanAttrs where
  -- Right-biased: keys in the right operand ('b') override the left ('a').
  -- 'Map.unionWith (\_ new -> new)' is used rather than 'Map.union b a'
  -- to make the right-bias self-evident and resistant to accidental reversal.
  SpanAttrs a <> SpanAttrs b =
    SpanAttrs (Map.unionWith (\_ new -> new) a b)
```

**`test/Trace/AttributesSpec.hs`** — add a property test:

```haskell
describe "SpanAttrs Semigroup" $ do
  it "right operand overrides left for duplicate keys" $
    property $ \(k :: AttrKey) (vL :: AttrValue) (vR :: AttrValue) ->
      let l = SpanAttrs (Map.singleton k vL)
          r = SpanAttrs (Map.singleton k vR)
      in lookupAttr k (l <> r) == Right vR

  it "preserves keys present only in the left operand" $
    property $ \(k :: AttrKey) (v :: AttrValue) ->
      let l = SpanAttrs (Map.singleton k v)
          r = SpanAttrs Map.empty
      in lookupAttr k (l <> r) == Right v

  it "satisfies associativity" $
    property $ \(a :: SpanAttrs) (b :: SpanAttrs) (c :: SpanAttrs) ->
      (a <> b) <> c == a <> (b <> c)
```

---

<a name="m8"></a>
## M8 — `inSpanCore` can export synchronously on the user thread

**Severity:** Medium  
**Module:** `Trace.Monad` — `inSpanCore`, `finalize`

### Risk

`finalize` calls `exporterExport (tracerExporter tracer)` directly. In the standard `withTracing` setup, `tracerExporter` is the batched exporter so this just enqueues. But `Tracer` is a plain record and nothing prevents constructing one with a raw (non-batched) exporter. In that case span finalization calls the inner exporter synchronously on the user's thread from inside a `bracket` cleanup, blocking for up to `otlpTimeout` (default 10s) before the user's function returns.

### Fix

Document the contract explicitly in the `Tracer` type. Additionally, restrict public `Tracer` construction to `withTracing` only by hiding the record constructor.

**`src/Trace/Monad.hs`** — hide the constructor and add a note:

```haskell
module Trace.Monad
  ( -- * Tracer
    Tracer   -- Opaque: do not construct directly. Use 'withTracing'.
    -- ... rest of exports ...
  ) where

-- In the Tracer data declaration, add a comment:
-- | A configured tracing handle.
--
-- IMPORTANT: Always construct via 'withTracing'. Do not build a 'Tracer'
-- manually with a non-batched 'SpanExporter' — span finalization calls
-- 'exporterExport' in the 'bracket' cleanup, which would block the user
-- thread for the full export timeout if the exporter is synchronous.
data Tracer = Tracer
  { tracerScope    :: !InstrumentationScope
  , tracerSampler  :: !Sampler
  , tracerExporter :: !SpanExporter
  , tracerClock    :: !Clock
  , tracerLogger   :: !InternalLogger
  }
```

By not exporting the `Tracer {..}` record constructor (only the type name `Tracer`), external code cannot construct a `Tracer` with an arbitrary exporter. The fix to `src/Trace.hs` is to export `Tracer` without `(..)`.

---

<a name="m9"></a>
## M9 — Hand-rolled `base16` encoder

**Severity:** Medium  
**Module:** `Trace.Export.Otlp`

### Risk

The custom `encodeBase16` function routes through `LazyByteString`, performs multiple full copies, and processes one byte at a time. The `base16-bytestring` package already in the Haskell ecosystem is SIMD-accelerated and zero-copy. More critically, hand-rolled crypto/encoding primitives in production code carry maintenance risk: they are not fuzz-tested, are not part of the package's published surface, and will diverge from community-audited implementations over time.

Note: `Trace.Propagation` already imports and uses `Data.ByteString.Base16` correctly. The OTLP exporter should do the same.

### Fix

Add `base16-bytestring` to `htrace.cabal` dependencies and replace the hand-rolled function.

**`htrace.cabal`** — in the `library` stanza:

```cabal
build-depends:
    base              >= 4.14 && < 5
  , ...
  , base16-bytestring >= 1.0  && < 2
```

**`src/Trace/Export/Otlp.hs`** — replace the encoder:

```haskell
-- Remove the entire encodeBase16 function and its import of
-- Data.ByteString.Lazy. Add:
import Data.ByteString.Base16 qualified as Base16

-- In encodeSpan:
hexText :: ByteString -> Text
hexText = TE.decodeUtf8 . Base16.encode
```

The `LBS` import can be removed from `Otlp.hs` entirely if it was only used by `encodeBase16`.

---

<a name="m10"></a>
## M10 — `OTEL_TRACES_SAMPLER_ARG=0` gives misleading error

**Severity:** Medium  
**Module:** `Trace.Config` — `loadSamplerConfig`

### Risk

`readPositiveDouble` rejects `0.0` (it requires `d > 0` implicitly via the pattern match — actually it accepts any value the `TR.double` parser returns, but then `mkSampleRate` accepts `[0, 1]`, so `0` is valid). Looking at the code: `readPositiveDouble` has no lower-bound check — it will return `Just 0.0`. However the function name says "positive" which means strictly greater than zero. The audit finding is that setting `OTEL_TRACES_SAMPLER_ARG=0` produces `MissingRequiredVar` rather than a sensible error. This is because the `Nothing` branch from the `case arg >>= readPositiveDouble` pattern is the same branch hit when the env var is actually absent.

### Fix

Rename the predicate to accurately reflect its range. Separate the "variable absent" error from the "variable present but invalid" error.

**`src/Trace/Config.hs`** — fix `loadSamplerConfig`:

```haskell
loadSamplerConfig
  :: IO (Validation (NonEmpty ConfigError) SamplerConfig)
loadSamplerConfig = do
  s <- lookupEnv "OTEL_TRACES_SAMPLER"
  case s of
    Nothing           -> pure (Success AlwaysSample)
    Just "always_on"  -> pure (Success AlwaysSample)
    Just "always_off" -> pure (Success NeverSample)
    Just sampler | sampler `elem` ["traceidratio", "parentbased_traceidratio"] -> do
      arg <- lookupEnv "OTEL_TRACES_SAMPLER_ARG"
      case arg of
        Nothing ->
          pure $ Failure $ NE.singleton $
            MissingRequiredVar (EnvVarName "OTEL_TRACES_SAMPLER_ARG")
        Just str ->
          case readNonNegativeDouble str of
            Nothing ->
              pure $ Failure $ NE.singleton $
                InvalidVarValue
                  (EnvVarName "OTEL_TRACES_SAMPLER_ARG")
                  (Text.pack str)
                  "expected a non-negative number in [0, 1]"
            Just d ->
              case mkSampleRate d of
                Left e   -> pure $ Failure $ NE.singleton e
                Right sr ->
                  pure $ Success $
                    if sampler == "traceidratio"
                    then TraceIdRatio sr
                    else ParentBased (TraceIdRatio sr)
    Just "parentbased_always_on"  ->
      pure (Success (ParentBased AlwaysSample))
    Just "parentbased_always_off" ->
      pure (Success (ParentBased NeverSample))
    Just other ->
      pure $ Failure $ NE.singleton $
        InvalidVarValue
          (EnvVarName "OTEL_TRACES_SAMPLER")
          (Text.pack other)
          "expected 'always_on', 'always_off', 'traceidratio', or 'parentbased_*'"
  where
    -- Accepts zero and positive doubles. Rejects negatives and non-numerics.
    readNonNegativeDouble str = case TR.double (Text.pack str) of
      Right (d, rest) | Text.null rest && d >= 0 -> Just d
      _                                           -> Nothing
```

---

<a name="l1"></a>
## L1 — Custom `Validation` applicative has no property tests

**Severity:** Mild  
**Module:** `Trace.Config`

### Risk

The custom `Validation` applicative is small and currently correct. Without property tests, future modifications (adding a `Monad` instance, changing error accumulation semantics) can silently break the core invariant that all errors are accumulated rather than short-circuited.

### Fix

Add property tests verifying the accumulation laws.

**`test/Trace/ConfigSpec.hs`** — add tests (internal module access required, or expose via a test-only module):

```haskell
-- Assuming Validation is accessible in tests via an Internal module:
describe "Validation applicative" $ do
  it "accumulates both errors in Failure <*> Failure" $ do
    let v1 = Failure (NE.singleton (MissingRequiredVar (EnvVarName "A")))
        v2 = Failure (NE.singleton (MissingRequiredVar (EnvVarName "B")))
        result = (id <$> v1) <*> v2
    validationToEither result `shouldSatisfy` \case
      Left errs -> NE.length errs == 2
      Right _   -> False

  it "returns Success when both sides succeed" $
    property $ \(x :: Int) (y :: Int) ->
      let v1 = Success x :: Validation (NonEmpty ConfigError) Int
          v2 = Success y
      in validationToEither ((+) <$> v1 <*> v2) == Right (x + y)

  it "short-circuits to Failure when left fails, right succeeds" $
    property $ \(x :: Int) ->
      let v1 = Failure (NE.singleton (InvalidSampleRate 2.0))
          v2 = Success x :: Validation (NonEmpty ConfigError) Int
      in case validationToEither (const <$> v1 <*> v2) of
           Left _ -> True
           Right _ -> False
```

---

<a name="l2"></a>
## L2 — Benchmarks are empty stubs

**Severity:** Mild  
**Module:** `bench/BatchBench.hs`, `bench/Main.hs`

### Risk

There is no performance baseline. Without benchmarks, regressions in enqueue throughput or flush latency are invisible until they appear as production incidents. For a batching telemetry exporter, performance is a correctness property: if enqueue falls below application throughput, spans are dropped (see H1).

### Fix

Implement the four critical benchmarks using `tasty-bench`. Store the baseline CSV in CI and fail builds on regression.

**`bench/BatchBench.hs`**:

```haskell
module BatchBench (benchmarks) where

import Test.Tasty.Bench
import Control.Concurrent.STM
import Data.List.NonEmpty qualified as NE
import Trace.Core
import Trace.Export.Batch
import Trace.Export.Types

-- | A minimal finished span for benchmarking overhead, not semantics.
dummySpan :: FinishedSpan
dummySpan = FinishedSpan
  { fsContext    = SpanContext dummyTraceId dummySpanId Nothing defaultTraceFlags
  , fsName       = SpanName "bench.span"
  , fsKind       = Internal
  , fsStartTime  = epoch
  , fsEndTime    = epoch
  , fsStatus     = StatusUnset
  , fsAttributes = mempty
  , fsEvents     = []
  }
  where
    epoch = read "1970-01-01 00:00:00 UTC"
    -- Use actual constructors from Trace.Core

benchmarks :: [Benchmark]
benchmarks =
  [ bench "enqueue/single-span" $
      whnfIO (withNoop $ \exporter ->
        exporterExport exporter (NE.singleton dummySpan))

  , bench "enqueue/100-spans" $
      whnfIO (withNoop $ \exporter ->
        exporterExport exporter
          (NE.fromList (replicate 100 dummySpan)))

  , bench "enqueue/queue-full-drop" $
      whnfIO (withSaturated $ \exporter ->
        exporterExport exporter (NE.singleton dummySpan))

  , bench "flush/empty-queue" $
      whnfIO (withNoop $ \exporter ->
        exporterFlush exporter)
  ]

withNoop :: (SpanExporter -> IO a) -> IO a
withNoop f = do
  Right be <- batchExporter cfg noopExporter
  f (batchSpanExporter be)
  where
    cfg = defaultBatchConfig { batchLogger = silentLogger }

withSaturated :: (SpanExporter -> IO a) -> IO a
withSaturated f = do
  Right be <- batchExporter cfg noopExporter
  let exporter = batchSpanExporter be
  -- Fill the queue to capacity first.
  mapM_ (\_ -> exporterExport exporter (NE.singleton dummySpan))
        [1 .. maxQueueSize defaultBatchConfig]
  f exporter
  where
    cfg = defaultBatchConfig { batchLogger = silentLogger }
```

**`bench/Main.hs`**:

```haskell
module Main where

import Test.Tasty.Bench
import BatchBench (benchmarks)

main :: IO ()
main = defaultMain benchmarks
```

Add to `htrace.cabal`:

```cabal
benchmark htrace-bench
  type:             exitcode-stdio-1.0
  main-is:          Main.hs
  other-modules:    BatchBench
  hs-source-dirs:   bench
  build-depends:
      base
    , htrace
    , tasty-bench >= 0.3
    , stm
```

---

<a name="l3"></a>
## L3 — Timing-dependent tests; no property tests

**Severity:** Mild  
**Module:** `test/`

### Risk

Several tests use `threadDelay` as a synchronisation primitive (e.g. `threadDelay 300_000` after enqueuing a span to "let the worker fire"). On a slow or loaded CI machine this delay may not be sufficient, producing flaky failures. Flaky tests erode confidence in the entire test suite. Additionally, the `Generators` module provides span generators but they are only used in non-property `hspec` tests — the concurrency invariants that most need fuzz-testing are entirely untested.

### Fix

Replace all timing-dependent synchronisation with explicit `MVar`/`TMVar` synchronisation. Add property tests for the roundtrip, semigroup, and config invariants.

**Replacing `threadDelay` with explicit synchronisation** (pattern):

```haskell
-- BEFORE (flaky):
enqueueSpan exporter span
threadDelay 300_000  -- hope the worker has fired by now
spans <- readAllSpans

-- AFTER (deterministic):
-- Use a custom in-test exporter that signals an MVar when a batch is received.
makeSignallingExporter :: IO (SpanExporter, MVar [FinishedSpan])
makeSignallingExporter = do
  received <- newEmptyMVar
  let exporter = SpanExporter
        { exporterExport   = \ne -> do
            putMVar received (NE.toList ne)
            pure (ExportSuccess (NE.length ne))
        , exporterFlush    = pure (Right ())
        , exporterShutdown = pure ()
        , exporterIsInterruptible = True
        }
  pure (exporter, received)

-- In the test:
(inner, received) <- makeSignallingExporter
Right batchExp <- batchExporter cfg inner
exporterExport (batchSpanExporter batchExp) (NE.singleton span)
spans <- takeMVar received  -- blocks until the worker delivers
spans `shouldBe` [span]
```

**Property tests to add** (`test/Trace/PropagationSpec.hs`):

```haskell
import Test.QuickCheck

-- Roundtrip: emit then parse is identity on valid contexts.
prop_traceparentRoundtrip :: SpanContext -> Property
prop_traceparentRoundtrip ctx =
  -- Null out parentId since traceparent doesn't carry it.
  let ctx' = ctx { scParentId = Nothing }
  in parseTraceparent (emitTraceparent ctx') ===
       PropagationSuccess ctx'
```

**Property tests to add** (`test/Trace/AttributesSpec.hs`):

```haskell
-- Right-bias law
prop_rightBias :: AttrKey -> AttrValue -> AttrValue -> Bool
prop_rightBias k vL vR =
  let l = SpanAttrs (Map.singleton k vL)
      r = SpanAttrs (Map.singleton k vR)
  in lookupAttr k (l <> r) == Right vR

-- Associativity law
prop_associativity :: SpanAttrs -> SpanAttrs -> SpanAttrs -> Bool
prop_associativity a b c = (a <> b) <> c == a <> (b <> c)
```

---

<a name="l4"></a>
## L4 — Golden OTLP snapshot file is empty

**Severity:** Mild  
**Module:** `Trace.Export.Otlp`; `test/snapshots/otlp-single-span.json`

### Risk

The golden test file `test/snapshots/otlp-single-span.json` is empty. Without a known-good payload fixture, the OTLP encoding is never validated end-to-end. A future refactor could silently change the JSON structure in a way that is rejected by all real collectors.

### Fix

Populate the snapshot file with a deterministic OTLP payload and use it as a golden test.

**`test/snapshots/otlp-single-span.json`** — populate with a canonical fixture:

```json
{
  "resourceSpans": [
    {
      "resource": {
        "attributes": [
          {"key": "service.name", "value": {"stringValue": "test-service"}},
          {"key": "telemetry.sdk.name", "value": {"stringValue": "htrace"}},
          {"key": "telemetry.sdk.version", "value": {"stringValue": "0.1.0.0"}},
          {"key": "telemetry.sdk.language", "value": {"stringValue": "haskell"}}
        ]
      },
      "scopeSpans": [
        {
          "scope": {
            "name": "htrace",
            "version": "0.1.0.0"
          },
          "spans": [
            {
              "traceId": "0af7651916cd43dd8448eb211c80319c",
              "spanId": "b7ad6b7169203331",
              "parentSpanId": "",
              "name": "test-span",
              "kind": 1,
              "startTimeUnixNano": 1000000000000000000,
              "endTimeUnixNano": 2000000000000000000,
              "attributes": [],
              "events": [],
              "status": {"code": 0}
            }
          ]
        }
      ]
    }
  ]
}
```

**`test/Trace/Export/OtlpSpec.hs`** — add golden test:

```haskell
it "encodeOtlp matches the golden snapshot" $ do
  goldenBytes <- BS.readFile "test/snapshots/otlp-single-span.json"
  let expected = Aeson.decode goldenBytes :: Maybe Aeson.Value
  let actual   = encodeOtlp testResource testScope [canonicalSpan]
  Just actual `shouldBe` expected
```

This test will catch any regression in the JSON structure, field names, or numeric encoding of timestamps.

---

<a name="l5"></a>
## L5 — `Resource` config is never attached to OTLP output

**Severity:** Mild  
**Module:** `Trace.Config`, `Trace.Monad`, `Trace.Export.Otlp`

### Risk

`TracingConfig` has a `configResource :: Resource` field. `withTracing` constructs a `Tracer` but does not thread `configResource` into the tracer or the exporter. `encodeOtlp` builds `resourceSpans` but omits the `resource` key entirely. All spans from htrace appear in the collector with no service name, no SDK identification, and no deployment attributes. This makes spans impossible to filter, attribute to a service, or correlate in Jaeger, Grafana Tempo, or any OTLP backend. The `Resource` configuration is currently a silent no-op.

### Fix

Thread `Resource` through `Tracer` and include it in `encodeOtlp`.

**`src/Trace/Monad.hs`** — add resource to `Tracer`:

```haskell
data Tracer = Tracer
  { tracerScope    :: !InstrumentationScope
  , tracerSampler  :: !Sampler
  , tracerExporter :: !SpanExporter
  , tracerClock    :: !Clock
  , tracerLogger   :: !InternalLogger
  , tracerResource :: !Resource   -- NEW
  }
```

Update `withTracing` to populate it:

```haskell
let tracer = Tracer
      { tracerScope    = InstrumentationScope "htrace" (Just "0.1.0.0")
      , tracerSampler  = samplerFromConfig (configSampler cfg)
      , tracerExporter = batched
      , tracerClock    = systemClock
      , tracerLogger   = configLogger cfg
      , tracerResource = configResource cfg   -- NEW
      }
```

Update `finalize` in `inSpanCore` to pass the resource to the exporter (or, better, pass it at exporter construction time):

**`src/Trace/Export/Otlp.hs`** — add resource to `encodeOtlp`:

```haskell
-- | Encode a list of 'FinishedSpan's as an OTLP ExportTraceServiceRequest,
-- including the resource and instrumentation scope.
encodeOtlp :: Resource -> InstrumentationScope -> [FinishedSpan] -> Value
encodeOtlp resource scope spans = object
  [ "resourceSpans" .= [ object
      [ "resource"   .= encodeResource resource
      , "scopeSpans" .= [ object
          [ "scope"  .= encodeScope scope
          , "spans"  .= map encodeSpan spans
          ]
        ]
      ]
    ]
  ]

encodeResource :: Resource -> Value
encodeResource r = object
  [ "attributes" .= map encodeKv (Map.toList (unSpanAttrs (unResource r))) ]

encodeScope :: InstrumentationScope -> Value
encodeScope s = object $
  [ "name" .= isName s ]
  <> maybe [] (\v -> ["version" .= v]) (isVersion s)
```

Update `doExport` to accept and forward `resource` and `scope`:

```haskell
doExport :: Resource -> InstrumentationScope -> Manager -> Request
         -> NonEmpty FinishedSpan -> IO ExportResult
doExport resource scope mgr req spans = do
  let body = encode (encodeOtlp resource scope (NE.toList spans))
  -- ... rest unchanged ...
```

Pass `tracerResource` and `tracerScope` from the tracer down to `exporterExport` — either by partially applying them at exporter construction time in `withTracing`, or by adding them to the `SpanExporter` record.

---

<a name="l6"></a>
## L6 — `InstrumentationScope` is hardcoded

**Severity:** Mild  
**Module:** `Trace.Monad` — `withTracing`

### Risk

The instrumentation scope is hardcoded to `InstrumentationScope "htrace" (Just "0.1.0.0")`. Users who instrument their own libraries or services via htrace cannot identify their spans with their own library name and version — a standard OTel practice. Additionally, `scopeSpans` in `encodeOtlp` currently omits the `scope` field entirely (fixed as part of L5), meaning even the hardcoded value is never emitted.

### Fix

Add `configScope :: InstrumentationScope` to `TracingConfig` with a sensible default. Use it in `withTracing`. Ensure `encodeOtlp` emits the `scope` field (covered by L5 fix above).

**`src/Trace/Core.hs`** — ensure `InstrumentationScope` is exported (it likely already is).

**`src/Trace/Config.hs`** — add to `TracingConfig` and `defaultConfig`:

```haskell
data TracingConfig = TracingConfig
  { configExporter    :: !ExporterConfig
  , configSampler     :: !SamplerConfig
  , configResource    :: !Resource
  , configPropagators :: ![Propagator]
  , configLogger      :: !InternalLogger
  , configScope       :: !InstrumentationScope
    -- ^ The instrumentation scope attached to all spans.
    --   Default: 'InstrumentationScope "htrace" (Just "0.1.0.0")'.
    --   Override with your own library name and version.
  }

defaultConfig :: TracingConfig
defaultConfig = TracingConfig
  { configExporter    = NoopExporter
  , configSampler     = AlwaysSample
  , configResource    = defaultResource
  , configPropagators = [W3CTraceContextPropagator]
  , configLogger      = stderrLogger
  , configScope       = InstrumentationScope "htrace" (Just "0.1.0.0")
  }
```

**`src/Trace/Monad.hs`** — use `configScope` in `withTracing`:

```haskell
let tracer = Tracer
      { tracerScope    = configScope cfg   -- was hardcoded
      , tracerSampler  = samplerFromConfig (configSampler cfg)
      , tracerExporter = batched
      , tracerClock    = systemClock
      , tracerLogger   = configLogger cfg
      , tracerResource = configResource cfg
      }
```

---

## Remediation Priority Order

The following table consolidates every fix in the order that maximises safety impact per unit of effort. Items marked **blocking** must be complete before any mission-critical deployment.

| Priority | ID | Description | Effort | Status |
|---|---|---|---|---|
| 1 | C4 | `recordException` atomicity — single STM transaction | Minutes | **Blocking** |
| 2 | C2 | Shutdown deadline — prevent process hang at exit | Small | **Blocking** |
| 3 | C1 | Replace `race`-timeout with `System.Timeout.timeout` | Small | **Blocking** |
| 4 | C3 | Supervised workers with restart and health TVar | Medium | **Blocking** |
| 5 | M5 | Post-shutdown guard with `TVar Bool` isShutDown flag | Minutes | **Blocking** |
| 6 | L5 | Thread `Resource` through tracer and into OTLP output | Small | **Blocking** |
| 7 | H5 | Validate and document `otlpTimeout < exportTimeout` | Minutes | **Blocking** |
| 8 | H3 | Return error for `GzipCompression` at construction time | Minutes | Strongly recommended |
| 9 | H1 | `ExportPartial` result; replace `dropChan` with `TVar` | Small | Strongly recommended |
| 10 | H2 | Retry pipeline with exponential backoff | Medium | Strongly recommended |
| 11 | H4 | `TelemetryMetrics` record wired to `TVar` counters | Medium | Strongly recommended |
| 12 | M3 | Preserve and forward `tracestate` header | Small | Strongly recommended |
| 13 | L6 | Configurable `InstrumentationScope` | Small | Strongly recommended |
| 14 | M10 | Fix `OTEL_TRACES_SAMPLER_ARG=0` error message | Minutes | Recommended |
| 15 | M9 | Replace hand-rolled base16 with `base16-bytestring` | Minutes | Recommended |
| 16 | M7 | `Map.unionWith` to make right-bias explicit | Minutes | Recommended |
| 17 | M4 | `PropagationFutureVersion` variant with logged warning | Small | Recommended |
| 18 | M8 | Hide `Tracer` constructor; document sync-export risk | Minutes | Recommended |
| 19 | M2 | Remove `modifySpan` from public export list | Minutes | Recommended |
| 20 | L4 | Populate and use golden OTLP snapshot test | Small | Recommended |
| 21 | L3 | Replace `threadDelay` sync; add property tests | Medium | Recommended |
| 22 | L2 | Implement benchmark suite with `tasty-bench` | Medium | Recommended |
| 23 | M6 | Orphan span watchdog with weak-ref registry | Large | Optional/configurable |
| 24 | L1 | Property tests for `Validation` applicative | Small | Recommended |

---

*End of htrace Remediation Guide.*
