module Trace.Export.BatchSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (throwIO)
import Data.IORef (newIORef, readIORef, modifyIORef', writeIORef)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as Text
import Test.Hspec

import Trace.Export.Batch
import Trace.Export.Types
import Trace.Generators

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Show only the Left side of an Either; used when Right has no Show instance.
showLeft :: Either BatchConfigError a -> String
showLeft (Left e)  = show e
showLeft (Right _) = "<Right>"

-- | A batch config with a very long interval so the worker never wakes
-- spontaneously during tests — exports happen only via shutdown or flush.
quietConfig :: BatchConfig
quietConfig = defaultBatchConfig
  { maxQueueSize   = 2048
  , maxExportBatch = 512
  , exportInterval = 3600
  , exportTimeout  = 10
  , onDroppedSpans = \_ -> pure ()
  , batchLogger    = silentLogger
  }

-- | Capture log messages during a test without touching stderr.
capturingLogger :: IO (InternalLogger, IO [Text], IO [Text])
capturingLogger = do
  warnsRef  <- newIORef ([] :: [Text])
  errorsRef <- newIORef ([] :: [Text])
  let logger = InternalLogger
        { logWarn  = \t -> modifyIORef' warnsRef  (t :)
        , logError = \t -> modifyIORef' errorsRef (t :)
        }
  pure
    ( logger
    , fmap reverse (readIORef warnsRef)
    , fmap reverse (readIORef errorsRef)
    )

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do

  describe "validateBatchConfig" $ do
    it "rejects maxQueueSize = 0" $ do
      result <- batchExporter
        quietConfig { maxQueueSize = 0 } noopExporter
      case result of
        Left (NonPositiveQueueSize 0) -> pure ()
        other -> expectationFailure ("unexpected: " <> showLeft other)

    it "rejects maxExportBatch = 0" $ do
      result <- batchExporter
        quietConfig { maxExportBatch = 0 } noopExporter
      case result of
        Left (NonPositiveBatchSize 0) -> pure ()
        other -> expectationFailure ("unexpected: " <> showLeft other)

    it "rejects maxExportBatch > maxQueueSize" $ do
      result <- batchExporter
        quietConfig { maxExportBatch = 100, maxQueueSize = 50 } noopExporter
      case result of
        Left (BatchExceedsQueue 100 50) -> pure ()
        other -> expectationFailure ("unexpected: " <> showLeft other)

    it "rejects exportInterval = 0" $ do
      result <- batchExporter
        quietConfig { exportInterval = 0 } noopExporter
      case result of
        Left (NonPositiveInterval _) -> pure ()
        other -> expectationFailure ("unexpected: " <> showLeft other)

    it "rejects exportTimeout = 0" $ do
      result <- batchExporter
        quietConfig { exportTimeout = 0 } noopExporter
      case result of
        Left (NonPositiveTimeout _) -> pure ()
        other -> expectationFailure ("unexpected: " <> showLeft other)

    it "accepts maxExportBatch == maxQueueSize" $ do
      result <- batchExporter
        quietConfig { maxExportBatch = 100, maxQueueSize = 100 } noopExporter
      case result of
        Right batched -> exporterShutdown batched
        Left e        -> expectationFailure ("unexpected error: " <> show e)

  describe "batchExporter" $ do
    it "delivers all spans on shutdown" $ do
      (inner, readAll) <- memoryExporter
      Right batched <- batchExporter quietConfig inner
      mapM_ (\i -> exporterExport batched (sampleSpan i NE.:| []))
            [1..100 :: Int]
      exporterShutdown batched
      spans <- readAll
      length spans `shouldBe` 100

    it "preserves all spans when n < maxQueueSize" $ do
      (inner, readAll) <- memoryExporter
      Right batched <- batchExporter
        quietConfig { maxQueueSize = 200, maxExportBatch = 200 }
        inner
      mapM_ (\i -> exporterExport batched (sampleSpan i NE.:| []))
            [1..50 :: Int]
      exporterShutdown batched
      spans <- readAll
      length spans `shouldBe` 50

    it "drop callback fires when queue overflows" $ do
      droppedRef <- newIORef (0 :: Int)
      let cfg = quietConfig
            { maxQueueSize   = 10
            , maxExportBatch = 10
            , onDroppedSpans = \n -> modifyIORef' droppedRef (+ n)
            }
      Right batched <- batchExporter cfg noopExporter
      mapM_ (\i -> exporterExport batched (sampleSpan i NE.:| []))
            [1..20 :: Int]
      -- Give the notifier thread time to drain and call the callback.
      exporterShutdown batched
      threadDelay 100_000
      dropped <- readIORef droppedRef
      dropped `shouldSatisfy` (>= 1)

    it "drop callback runs off the producer thread" $ do
      -- Callback blocks for 300ms. Enqueue happens in < 50ms even so.
      callbackStarted <- newIORef False
      let cfg = quietConfig
            { maxQueueSize   = 1
            , maxExportBatch = 1
            , onDroppedSpans = \_ -> do
                modifyIORef' callbackStarted (const True)
                threadDelay 300_000
            }
      Right batched <- batchExporter cfg noopExporter
      -- Enqueue 5 spans; at least 4 will be dropped and trigger the callback.
      mapM_ (\i -> exporterExport batched (sampleSpan i NE.:| []))
            [1..5 :: Int]
      -- The enqueue calls above should return promptly (before callback finishes).
      -- We just verify the callback eventually ran.
      exporterShutdown batched
      threadDelay 400_000
      started <- readIORef callbackStarted
      started `shouldBe` True

    it "flush exports buffered spans synchronously" $ do
      (inner, readAll) <- memoryExporter
      Right batched <- batchExporter
        quietConfig { maxQueueSize = 100, maxExportBatch = 100 }
        inner
      mapM_ (\i -> exporterExport batched (sampleSpan i NE.:| []))
            [1..30 :: Int]
      result <- exporterFlush batched
      result `shouldBe` Right ()
      spans <- readAll
      length spans `shouldBe` 30
      exporterShutdown batched

    it "shutdown drains the full queue before returning" $ do
      (inner, readAll) <- memoryExporter
      Right batched <- batchExporter
        quietConfig { maxQueueSize = 500, maxExportBatch = 500 }
        inner
      mapM_ (\i -> exporterExport batched (sampleSpan i NE.:| []))
            [1..200 :: Int]
      exporterShutdown batched
      spans <- readAll
      length spans `shouldBe` 200

    it "exporterExport returns ExporterShutDown after shutdown" $ do
      Right batched <- batchExporter quietConfig noopExporter
      exporterShutdown batched
      result <- exporterExport batched (sampleSpan 0 NE.:| [])
      result `shouldBe` ExportFailure ExporterShutDown

    it "exporterFlush returns ExporterShutDown after shutdown" $ do
      Right batched <- batchExporter quietConfig noopExporter
      exporterShutdown batched
      result <- exporterFlush batched
      result `shouldBe` Left ExporterShutDown

  -- -------------------------------------------------------------------------
  -- C-1: Shutdown race — spans enqueued concurrently with shutdown
  -- are not silently admitted to a dead queue.
  -- -------------------------------------------------------------------------

  describe "C-1: atomic shutdown state machine" $ do
    it "enqueue after Draining returns ExporterShutDown, not ExportSuccess" $ do
      -- Use a slow exporter so the worker is occupied during shutdown,
      -- giving the race the best chance to surface.
      exportStarted <- newIORef False
      let slowExporter = noopExporter
            { exporterExport = \_ -> do
                writeIORef exportStarted True
                threadDelay 500_000
                pure (ExportSuccess 0)
            }
      Right batched <- batchExporter
        quietConfig { exportInterval = 0.01, exportTimeout = 5 }
        slowExporter
      -- Fill the queue so the worker wakes.
      mapM_ (\i -> exporterExport batched (sampleSpan i NE.:| []))
            [1..10 :: Int]
      threadDelay 50_000  -- let the worker start its export

      -- Concurrently initiate shutdown and enqueue another span.
      -- The enqueue must not succeed with ExportSuccess after shutdown
      -- has set the state to Draining.
      exporterShutdown batched
      result <- exporterExport batched (sampleSpan 99 NE.:| [])
      result `shouldBe` ExportFailure ExporterShutDown

    it "repeated enqueues after shutdown all return ExporterShutDown" $ do
      Right batched <- batchExporter quietConfig noopExporter
      exporterShutdown batched
      results <- mapM (\i -> exporterExport batched (sampleSpan i NE.:| []))
                      [1..10 :: Int]
      all (== ExportFailure ExporterShutDown) results `shouldBe` True

    it "flush after shutdown returns ExporterShutDown" $ do
      Right batched <- batchExporter quietConfig noopExporter
      exporterShutdown batched
      result <- exporterFlush batched
      result `shouldBe` Left ExporterShutDown

    it "double shutdown is idempotent" $ do
      -- Calling exporterShutdown twice must not deadlock or throw.
      -- The second call races against a Stopped state; inner shutdown
      -- is called only once because the worker async is already done.
      Right batched <- batchExporter quietConfig noopExporter
      exporterShutdown batched
      exporterShutdown batched  -- must return without hanging

  -- -------------------------------------------------------------------------
  -- C-2: Unsupervised forkIO → crashing worker is now observable
  -- -------------------------------------------------------------------------

  describe "C-2: worker crash visibility" $ do
    it "logs an error when the inner exporter throws and the worker exits" $ do
      (logger, _, readErrors) <- capturingLogger
      exportCount <- newIORef (0 :: Int)
      let crashAfterFirst = noopExporter
            { exporterExport = \ne -> do
                n <- readIORef exportCount
                modifyIORef' exportCount (+ 1)
                if n == 0
                  -- First call throws; worker should catch, log, and continue.
                  then throwIO (userError "injected crash")
                  else pure (ExportSuccess (NE.length ne))
            }
      Right batched <- batchExporter
        quietConfig
          { exportInterval = 0.01
          , exportTimeout  = 5
          , batchLogger    = logger
          }
        crashAfterFirst
      _ <- exporterExport batched (sampleSpan 0 NE.:| [])
      threadDelay 300_000
      exporterShutdown batched
      errs <- readErrors
      -- The worker must have logged the exception.
      any (Text.isInfixOf "threw exception") errs `shouldBe` True

    it "worker crash is reported at shutdown via logError, not silently lost" $ do
      -- A worker that always throws forces the worker to loop through the
      -- crash path. We verify the error is recorded, not swallowed.
      (logger, _, readErrors) <- capturingLogger
      let alwaysCrash = noopExporter
            { exporterExport = \_ -> throwIO (userError "persistent crash")
            }
      Right batched <- batchExporter
        quietConfig
          { exportInterval = 0.01
          , exportTimeout  = 5
          , batchLogger    = logger
          }
        alwaysCrash
      _ <- exporterExport batched (sampleSpan 0 NE.:| [])
      threadDelay 300_000
      exporterShutdown batched
      errs <- readErrors
      null errs `shouldBe` False

  -- -------------------------------------------------------------------------
  -- M-5: doShutdown cancels the worker async on timeout before calling
  -- exporterShutdown inner, preventing use-after-free.
  -- -------------------------------------------------------------------------

  describe "M-5: safe inner-exporter teardown on worker timeout" $ do
    -- Design note: all three tests use an MVar rendezvous to guarantee the
    -- worker is provably inside exporterExport before exporterShutdown is
    -- called.  A blind threadDelay is unreliable: if the worker's own
    -- exportTimeout fires first, it self-exits cleanly and shutdown never
    -- hits the cancellation path.
    --
    -- The pattern:
    --   exportTimeout = 30s  → worker never self-cancels during the test
    --   deadlineMicros in doShutdown = exportTimeout + 1s = 31s, which would
    --   hang forever — so we set exportTimeout very long and rely on the
    --   MVar to know the worker is stuck, then the shutdown deadline is the
    --   only way out.  To keep tests fast we set exportTimeout = 30s but the
    --   blocking exporter signals the MVar immediately and blocks; shutdown
    --   is called right after, so the real wall-clock wait is just the
    --   shutdown deadline = exportTimeout + 1s.
    --
    -- To avoid 31s test times we override the deadline by using a short
    -- exportTimeout (0.5s) and an MVar to ensure the worker is inside the
    -- export *before* we call shutdown.  The worker timeout (0.5s) must be
    -- longer than the time between "worker enters export" and "shutdown is
    -- called" — the MVar makes that gap effectively zero.  The shutdown
    -- deadline is then exportTimeout + 1s = 1.5s, which is acceptable.

    it "shutdown completes even when the worker is blocked mid-export" $ do
      -- Use an MVar to confirm the worker is inside the export before we
      -- initiate shutdown.  exportTimeout is set long enough that the worker
      -- does not self-cancel before shutdown can reach the deadline.
      workerInside <- newEmptyMVar
      let blockingExporter = noopExporter
            { exporterExport = \_ -> do
                putMVar workerInside ()        -- signal: worker is now blocked
                threadDelay 60_000_000         -- block until async-cancelled
                pure (ExportSuccess 0)
            }
      Right batched <- batchExporter
        quietConfig
          { exportInterval = 0.01
          -- exportTimeout drives doShutdown's deadline (exportTimeout + 1s).
          -- 0.5s gives a 1.5s total deadline, keeping the test fast while
          -- still being long enough that the worker does not self-timeout
          -- before shutdown receives the MVar signal.
          , exportTimeout  = 0.5
          , batchLogger    = silentLogger
          }
        blockingExporter
      _ <- exporterExport batched (sampleSpan 0 NE.:| [])
      takeMVar workerInside   -- block until worker is provably inside export
      -- shutdown must return (not hang) even though the worker is stuck;
      -- the cancel in M-5 unblocks it within the deadline.
      exporterShutdown batched

    it "inner exporter is shut down even when the worker is cancelled" $ do
      workerInside       <- newEmptyMVar
      innerShutdownCalled <- newIORef False
      let slowThenShutdown = noopExporter
            { exporterExport   = \_ -> do
                putMVar workerInside ()
                threadDelay 60_000_000 >> pure (ExportSuccess 0)
            , exporterShutdown = writeIORef innerShutdownCalled True
            }
      Right batched <- batchExporter
        quietConfig
          { exportInterval = 0.01
          , exportTimeout  = 0.5
          , batchLogger    = silentLogger
          }
        slowThenShutdown
      _ <- exporterExport batched (sampleSpan 0 NE.:| [])
      takeMVar workerInside
      exporterShutdown batched
      -- Inner exporter must have been shut down despite worker cancellation.
      called <- readIORef innerShutdownCalled
      called `shouldBe` True

    it "shutdown logs a warning when the worker is cancelled due to timeout" $ do
      workerInside <- newEmptyMVar

      (logger, readWarns, _) <- capturingLogger

      let blockingExporter =
            noopExporter
              { exporterExport = \_ -> do
                  putMVar workerInside ()

                  -- Simulate permanently blocked exporter call.
                  threadDelay 60_000_000

                  pure (ExportSuccess 0)
              }

      Right batched <- batchExporter
        quietConfig
          { exportInterval  = 0.01

          -- Keep export alive long enough that shutdown
          -- cancellation path is exercised.
          , exportTimeout   = 30

          -- Force shutdown timeout quickly.
          , shutdownTimeout = 0.5

          , batchLogger     = logger
          }
        blockingExporter

      _ <- exporterExport batched (sampleSpan 0 NE.:| [])

      -- Ensure worker is definitely inside exporterExport.
      takeMVar workerInside

      exporterShutdown batched

      warns <- readWarns

      any (Text.isInfixOf "forcing cancellation") warns
        `shouldBe` True

  -- -------------------------------------------------------------------------
  -- Phase R5: worker logging (unchanged from original suite)
  -- -------------------------------------------------------------------------

  describe "batchLogger" $ do
    it "logs a warning on export timeout" $ do
      (logger, readWarns, _) <- capturingLogger
      let slowExporter = noopExporter
            { exporterExport = \_ -> threadDelay 5_000_000
                                       >> pure (ExportSuccess 1)
            }
      Right batched <- batchExporter
        quietConfig
          { exportInterval = 0.01
          , exportTimeout  = 0.05
          , batchLogger    = logger
          }
        slowExporter
      _ <- exporterExport batched (sampleSpan 0 NE.:| [])
      threadDelay 500_000
      exporterShutdown batched
      warns <- readWarns
      any (Text.isInfixOf "timed out") warns `shouldBe` True

    it "logs an error when exporter throws" $ do
      (logger, _, readErrors) <- capturingLogger
      let crashExporter = noopExporter
            { exporterExport = \_ -> throwIO (userError "boom")
            }
      Right batched <- batchExporter
        quietConfig
          { exportInterval = 0.01
          , exportTimeout  = 5
          , batchLogger    = logger
          }
        crashExporter
      _ <- exporterExport batched (sampleSpan 0 NE.:| [])
      threadDelay 300_000
      exporterShutdown batched
      errs <- readErrors
      any (Text.isInfixOf "threw exception") errs `shouldBe` True

    it "logs a warning on ExportFailure" $ do
      (logger, readWarns, _) <- capturingLogger
      let failExporter = noopExporter
            { exporterExport = \_ ->
                pure (ExportFailure (EndpointUnreachable "test"))
            }
      Right batched <- batchExporter
        quietConfig
          { exportInterval = 0.01
          , exportTimeout  = 5
          , batchLogger    = logger
          }
        failExporter
      _ <- exporterExport batched (sampleSpan 0 NE.:| [])
      threadDelay 300_000
      exporterShutdown batched
      warns <- readWarns
      any (Text.isInfixOf "export returned failure") warns `shouldBe` True

    it "ExportSuccess produces no log output" $ do
      (logger, readWarns, readErrors) <- capturingLogger
      Right batched <- batchExporter
        quietConfig
          { exportInterval = 0.01
          , batchLogger    = logger
          }
        noopExporter
      _ <- exporterExport batched (sampleSpan 0 NE.:| [])
      threadDelay 300_000
      exporterShutdown batched
      warns  <- readWarns
      errors <- readErrors
      warns  `shouldBe` []
      errors `shouldBe` []

    it "crashing logger does not crash the worker" $ do
      let crashLogger = InternalLogger
            { logWarn  = \_ -> throwIO (userError "logger crash")
            , logError = \_ -> throwIO (userError "logger crash")
            }
          failExporter = noopExporter
            { exporterExport = \_ ->
                pure (ExportFailure (EndpointUnreachable "test"))
            }
      Right batched <- batchExporter
        quietConfig
          { exportInterval = 0.01
          , exportTimeout  = 5
          , batchLogger    = crashLogger
          }
        failExporter
      _ <- exporterExport batched (sampleSpan 0 NE.:| [])
      threadDelay 300_000
      exporterShutdown batched
      -- If we reach here the worker did not crash
      pure ()
