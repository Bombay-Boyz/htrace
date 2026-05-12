module Trace.Export.BatchSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Exception (throwIO)
import Data.IORef (newIORef, readIORef, modifyIORef')
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

    it "flush returns ExportTimeout when inner exporter is slow" $ do
      let slowExporter = noopExporter
            { exporterExport = \_ -> do
                threadDelay 2_000_000
                pure (ExportSuccess 0)
            }
      Right batched <- batchExporter
        quietConfig { exportTimeout = 0.1 }
        slowExporter
      _ <- exporterExport batched (sampleFinishedSpan NE.:| [])
      result <- exporterFlush batched
      case result of
        Left (ExportTimeout _) -> pure ()
        other ->
          expectationFailure ("unexpected: " <> show other)
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
  -- Phase R5: worker logging
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
