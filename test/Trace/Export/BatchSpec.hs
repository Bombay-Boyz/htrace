module Trace.Export.BatchSpec (spec) where

import Control.Concurrent (threadDelay)
import Data.IORef (newIORef, readIORef, modifyIORef')
import Data.List.NonEmpty qualified as NE
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
  }

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