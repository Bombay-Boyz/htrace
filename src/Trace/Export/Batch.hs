module Trace.Export.Batch
  ( -- * Configuration
    BatchConfig (..)
  , defaultBatchConfig
  , defaultOnDroppedSpans
    -- * Constructor
  , batchExporter
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Monad (when)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as Text
import Data.Time (NominalDiffTime)
import UnliftIO.Async (race)
import Control.Exception (SomeException)
import UnliftIO.Exception (try)
import Trace.Core (FinishedSpan)
import Trace.Export.Types

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

-- | Configuration for the batching exporter wrapper.
data BatchConfig = BatchConfig
  { maxQueueSize   :: !Int
    -- ^ Maximum number of spans held in the queue at once.
  , maxExportBatch :: !Int
    -- ^ Maximum number of spans sent to the inner exporter per flush.
  , exportInterval :: !NominalDiffTime
    -- ^ How often the background worker wakes to flush, in seconds.
  , exportTimeout  :: !NominalDiffTime
    -- ^ How long a single export call may run before being abandoned.
  , onDroppedSpans :: !(Int -> IO ())
    -- ^ Callback invoked (on a background thread) when spans are dropped
    --   because the queue is full.
  , batchLogger    :: !InternalLogger
    -- ^ Receives warnings for export failures and timeouts.
    --   Use 'silentLogger' to suppress (e.g. in tests that expect failures).
  }

-- | Warn to stderr when spans are dropped.
defaultOnDroppedSpans :: InternalLogger -> Int -> IO ()
defaultOnDroppedSpans logger n =
  logWarn logger
    ("htrace: dropped " <> Text.pack (show n) <> " spans (queue full)")

-- | Sensible production defaults.
defaultBatchConfig :: BatchConfig
defaultBatchConfig = BatchConfig
  { maxQueueSize   = 2048
  , maxExportBatch = 512
  , exportInterval = 5
  , exportTimeout  = 30
  , onDroppedSpans = defaultOnDroppedSpans stderrLogger
  , batchLogger    = stderrLogger
  }

-- ---------------------------------------------------------------------------
-- Validation
-- ---------------------------------------------------------------------------

validateBatchConfig :: BatchConfig -> Maybe BatchConfigError
validateBatchConfig c
  | maxQueueSize c   <= 0 =
      Just (NonPositiveQueueSize (maxQueueSize c))
  | maxExportBatch c <= 0 =
      Just (NonPositiveBatchSize (maxExportBatch c))
  | maxExportBatch c > maxQueueSize c =
      Just (BatchExceedsQueue (maxExportBatch c) (maxQueueSize c))
  | exportInterval c <= 0 =
      Just (NonPositiveInterval (exportInterval c))
  | exportTimeout c  <= 0 =
      Just (NonPositiveTimeout (exportTimeout c))
  | otherwise = Nothing

-- ---------------------------------------------------------------------------
-- Constructor
-- ---------------------------------------------------------------------------

-- | Wrap any 'SpanExporter' with a bounded async queue and background
-- flush worker. Returns 'Left' if the config is invalid.
batchExporter
  :: BatchConfig
  -> SpanExporter
  -> IO (Either BatchConfigError SpanExporter)
batchExporter cfg inner =
  case validateBatchConfig cfg of
    Just e  -> pure (Left e)
    Nothing -> do
      queue        <- newTBQueueIO (fromIntegral (maxQueueSize cfg))
      dropChan     <- newTBQueueIO 64
      shutdownVar  <- newTVarIO False
      workerDone   <- newEmptyTMVarIO
      notifierDone <- newEmptyTMVarIO
      _ <- forkIO (worker queue shutdownVar workerDone)
      _ <- forkIO (notifier dropChan shutdownVar notifierDone)
      pure $ Right $ SpanExporter
        { exporterExport   = enqueue queue dropChan
        , exporterFlush    = doFlush queue
        , exporterShutdown = doShutdown queue shutdownVar
                               workerDone notifierDone
        }
  where
    -- -----------------------------------------------------------------------
    -- Enqueue
    -- -----------------------------------------------------------------------

    enqueue queue dropChan ne = do
      let xs = NE.toList ne
          n  = length xs
      dropped <- atomically $ do
        occupied <- lengthTBQueue queue
        let space      = fromIntegral (maxQueueSize cfg) - fromIntegral occupied
            canEnqueue = min n space
            nDropped   = n - canEnqueue
        mapM_ (writeTBQueue queue) (take canEnqueue xs)
        when (nDropped > 0) $ do
          full <- isFullTBQueue dropChan
          when (not full) $ writeTBQueue dropChan nDropped
        pure nDropped
      pure (ExportSuccess (n - dropped))

    -- -----------------------------------------------------------------------
    -- Notifier thread: drains dropChan and calls onDroppedSpans
    -- -----------------------------------------------------------------------

    notifier dropChan shutdownVar done = loop
      where
        loop = do
          mn <- atomically $
            (Just <$> readTBQueue dropChan) `orElse` do
              shutting <- readTVar shutdownVar
              empty    <- isEmptyTBQueue dropChan
              if shutting && empty
                then pure Nothing
                else retry
          case mn of
            Just n  -> onDroppedSpans cfg n >> loop
            Nothing -> atomically (putTMVar done ())

    -- -----------------------------------------------------------------------
    -- Safe log: logger failures must never crash the worker
    -- -----------------------------------------------------------------------

    safeLog :: (InternalLogger -> Text.Text -> IO ()) -> Text.Text -> IO ()
    safeLog f msg = do
      result <- try (f (batchLogger cfg) msg) :: IO (Either SomeException ())
      case result of
        Right () -> pure ()
        Left  _  -> pure ()

    -- -----------------------------------------------------------------------
    -- Worker thread: periodically drains queue and exports
    -- -----------------------------------------------------------------------

    worker queue shutdownVar done = loop
      where
        intervalMicros = round (realToFrac (exportInterval cfg) * 1_000_000 :: Double)
        timeoutMicros  = round (realToFrac (exportTimeout  cfg) * 1_000_000 :: Double)

        loop = do
          -- Wake on: interval elapsed OR queue has a full batch OR shutdown.
          _ <- race
            (threadDelay intervalMicros)
            (atomically $ do
              shutting <- readTVar shutdownVar
              if shutting
                then pure ()
                else do
                  len <- lengthTBQueue queue
                  if fromIntegral len >= maxExportBatch cfg
                    then pure ()
                    else retry)

          -- Drain up to maxExportBatch spans.
          batch <- atomically (drainBatch queue (maxExportBatch cfg))
          case NE.nonEmpty batch of
            Nothing -> pure ()
            Just ne -> do
              raceResult <- race
                (threadDelay timeoutMicros)
                (try (exporterExport inner ne) :: IO (Either SomeException ExportResult))
              case raceResult of
                Left () ->
                  safeLog logWarn
                    (  "htrace: export timed out after "
                    <> Text.pack (show (exportTimeout cfg))
                    <> "s; "
                    <> Text.pack (show (NE.length ne))
                    <> " spans abandoned"
                    )
                Right (Left ex) ->
                  safeLog logError
                    (  "htrace: exporter threw exception: "
                    <> Text.pack (show ex)
                    )
                Right (Right (ExportFailure err)) ->
                  safeLog logWarn
                    (  "htrace: export returned failure: "
                    <> Text.pack (show err)
                    )
                Right (Right (ExportSuccess _)) ->
                  pure ()

          -- Exit only when shutdown is set AND the queue is empty.
          shouldExit <- atomically $ do
            shutting <- readTVar shutdownVar
            empty    <- isEmptyTBQueue queue
            pure (shutting && empty)
          if shouldExit
            then atomically (putTMVar done ())
            else loop

    -- -----------------------------------------------------------------------
    -- Flush: drain and export synchronously, bounded by exportTimeout
    -- -----------------------------------------------------------------------

    doFlush queue = do
      batch <- atomically (drainBatch queue (maxQueueSize cfg))
      case NE.nonEmpty batch of
        Nothing -> pure (Right ())
        Just ne -> do
          result <- race
            (threadDelay (round (realToFrac (exportTimeout cfg) * 1_000_000 :: Double)))
            (exporterExport inner ne)
          case result of
            Left  ()                -> pure (Left (ExportTimeout (exportTimeout cfg)))
            Right (ExportSuccess _) -> pure (Right ())
            Right (ExportFailure e) -> pure (Left e)

    -- -----------------------------------------------------------------------
    -- Shutdown: signal threads and wait for clean exit
    -- -----------------------------------------------------------------------

    doShutdown _queue shutdownVar workerDone notifierDone = do
      atomically (writeTVar shutdownVar True)
      _ <- atomically (takeTMVar workerDone)
      _ <- atomically (takeTMVar notifierDone)
      exporterShutdown inner

    -- -----------------------------------------------------------------------
    -- Helper: atomically dequeue up to n spans
    -- -----------------------------------------------------------------------

    drainBatch queue maxN = go [] 0
      where
        go acc i
          | i >= maxN = pure (reverse acc)
          | otherwise =
              tryReadTBQueue queue >>= \case
                Nothing -> pure (reverse acc)
                Just s  -> go (s : acc) (i + 1)
