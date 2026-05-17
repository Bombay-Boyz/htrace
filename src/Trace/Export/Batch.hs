module Trace.Export.Batch
  ( -- * Configuration
    BatchConfig (..)
  , defaultBatchConfig
  , defaultOnDroppedSpans
    -- * Constructor
  , batchExporter
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel, waitCatch)
import Control.Concurrent.STM
import Control.Monad (void, when)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as Text
import Data.Time (NominalDiffTime)
import System.Timeout (timeout)
import UnliftIO.Async (race)
import Control.Exception (SomeException)
import Control.Exception qualified as CE

import Trace.Core (FinishedSpan)
import Trace.Export.Types

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

-- NEW
data BatchConfig = BatchConfig
  { maxQueueSize      :: !Int
  , maxExportBatch    :: !Int
  , exportInterval    :: !NominalDiffTime
  , exportTimeout     :: !NominalDiffTime
  , shutdownTimeout   :: !NominalDiffTime
  , overflowStrategy  :: !OverflowStrategy
  , onDroppedSpans    :: !(Int -> IO ())
  , batchLogger       :: !InternalLogger
  }

defaultOnDroppedSpans :: InternalLogger -> Int -> IO ()
defaultOnDroppedSpans logger n =
  logWarn logger
    ("htrace: dropped "
      <> Text.pack (show n)
      <> " spans (queue full)")


defaultBatchConfig = BatchConfig
  { maxQueueSize     = 2048
  , maxExportBatch   = 512
  , exportInterval   = 5
  , exportTimeout    = 30
  , shutdownTimeout  = 5
  , overflowStrategy = DropNewest
  , onDroppedSpans   = defaultOnDroppedSpans stderrLogger
  , batchLogger      = stderrLogger
  }

-- ---------------------------------------------------------------------------
-- Validation
-- ---------------------------------------------------------------------------

validateBatchConfig :: BatchConfig -> Maybe BatchConfigError
validateBatchConfig c
  | maxQueueSize c <= 0   = Just (NonPositiveQueueSize (maxQueueSize c))
  | maxExportBatch c <= 0 = Just (NonPositiveBatchSize (maxExportBatch c))
  | maxExportBatch c > maxQueueSize c =
      Just (BatchExceedsQueue (maxExportBatch c) (maxQueueSize c))
  | exportInterval c <= 0  = Just (NonPositiveInterval (exportInterval c))
  | exportTimeout c <= 0   = Just (NonPositiveTimeout (exportTimeout c))
  | shutdownTimeout c <= 0 = Just (NonPositiveShutdownTimeout (shutdownTimeout c))
  | otherwise              = Nothing

-- ---------------------------------------------------------------------------
-- Internal types
-- ---------------------------------------------------------------------------

-- | Items placed on the internal worker queue.
-- 'FlushBarrier' is a sentinel: when the worker encounters it, every span
-- ahead of it has already been exported, so it signals the TMVar to unblock
-- the flush caller.
data WorkItem
  = SpanItem     !(NonEmpty FinishedSpan)
  | FlushBarrier !(TMVar (Either ExportError ()))

data ExporterState
  = Running
  | Draining
  | Stopped
  deriving stock (Eq, Show)


data EnqueueResult
  = Enqueued    !Int        -- ^ all spans accepted
  | PartialDrop !Int !Int   -- ^ (accepted, dropped)
  | QueueClosed             -- ^ exporter is shutting down
  deriving stock (Show, Eq)

enqueueResultToExport :: EnqueueResult -> ExportResult
enqueueResultToExport (Enqueued n)       = ExportSuccess n
enqueueResultToExport (PartialDrop n _)  = ExportSuccess n
enqueueResultToExport QueueClosed        = ExportFailure ExporterShutDown

-- ---------------------------------------------------------------------------
-- Constructor
-- ---------------------------------------------------------------------------

batchExporter
  :: BatchConfig
  -> SpanExporter
  -> IO (Either BatchConfigError SpanExporter)
batchExporter cfg inner =
  case validateBatchConfig cfg of
    Just e  -> pure (Left e)
    Nothing -> do
      queue          <- newTBQueueIO (fromIntegral (maxQueueSize cfg))
      barrierPending <- newTVarIO False
      dropCounter    <- newTVarIO (0 :: Int)
      stateVar       <- newTVarIO Running
      workerDone     <- newEmptyTMVarIO
      notifierDone   <- newEmptyTMVarIO

      workerAsync   <- async (worker   barrierPending queue stateVar workerDone)
      notifierAsync <- async (notifier dropCounter stateVar notifierDone)

      pure $ Right $ SpanExporter
        { exporterExport   = guardedEnqueue stateVar queue dropCounter
        , exporterFlush    = guardedFlush   barrierPending stateVar queue
        , exporterShutdown =
            doShutdown stateVar workerDone notifierDone
                       workerAsync notifierAsync
        }

  where

    -- | Convert a NominalDiffTime to microseconds for threadDelay / timeout.
    -- Positivity is guaranteed by validateBatchConfig.
    toMicros :: NominalDiffTime -> Int
    toMicros d = round (realToFrac d * 1_000_000 :: Double)

    isBarrier :: WorkItem -> Bool
    isBarrier (FlushBarrier _) = True
    isBarrier _                = False

    -- | Read up to maxN items from the queue without blocking.
    drainItems :: TBQueue WorkItem -> Int -> STM [WorkItem]
    drainItems queue maxN = go [] 0
      where
        go acc i
          | i >= maxN = pure (reverse acc)
          | otherwise =
              tryReadTBQueue queue >>= \case
                Nothing -> pure (reverse acc)
                Just it -> go (it : acc) (i + 1)

    -- -----------------------------------------------------------------------
    -- Safe logging
    -- -----------------------------------------------------------------------

    syncOnly :: SomeException -> Maybe SomeException
    syncOnly ex =
      case CE.fromException ex of
        Just (CE.SomeAsyncException _) -> Nothing
        Nothing                        -> Just ex

    safeLog :: (InternalLogger -> Text.Text -> IO ()) -> Text.Text -> IO ()
    safeLog f msg = do
      result <- CE.tryJust syncOnly (f (batchLogger cfg) msg)
      case result of
        Right () -> pure ()
        Left  _  -> pure ()

    -- -----------------------------------------------------------------------
    -- Enqueue
    -- -----------------------------------------------------------------------

    -- NEW
    guardedEnqueue
      :: TVar ExporterState
      -> TBQueue WorkItem
      -> TVar Int
      -> NonEmpty FinishedSpan
      -> IO ExportResult
    guardedEnqueue stateVar queue dropCounter ne =
      fmap enqueueResultToExport $ atomically $ do
        st <- readTVar stateVar
        case st of
          Running  -> enqueueSTM queue dropCounter ne
          Draining -> pure QueueClosed
          Stopped  -> pure QueueClosed



    -- NEW
    enqueueSTM
      :: TBQueue WorkItem
      -> TVar Int
      -> NonEmpty FinishedSpan
      -> STM EnqueueResult
    enqueueSTM queue dropCounter ne = do
      let xs = NE.toList ne
          n  = length xs
      case overflowStrategy cfg of
        DropNewest -> do
          occupied <- lengthTBQueue queue
          let space      = maxQueueSize cfg - fromIntegral occupied
              canEnqueue = min n space
              nDropped   = n - canEnqueue
          case NE.nonEmpty (take canEnqueue xs) of
            Just batch -> writeTBQueue queue (SpanItem batch)
            Nothing    -> pure ()
          when (nDropped > 0) $
            modifyTVar' dropCounter (+ nDropped)
          if nDropped > 0
            then pure (PartialDrop (n - nDropped) nDropped)
            else pure (Enqueued n)
        
        DropOldest -> do
          full <- isFullTBQueue queue
          when full $ void (tryReadTBQueue queue)
          writeTBQueue queue (SpanItem ne)
          pure (Enqueued n)
        BlockProducer -> do
          writeTBQueue queue (SpanItem ne)
          pure (Enqueued n)

    -- -----------------------------------------------------------------------
    -- Flush
    -- -----------------------------------------------------------------------

    guardedFlush
      :: TVar Bool
      -> TVar ExporterState
      -> TBQueue WorkItem
      -> IO (Either ExportError ())
    guardedFlush barrierPending stateVar queue = do
      st <- readTVarIO stateVar
      case st of
        Draining -> pure (Left ExporterShutDown)
        Stopped  -> pure (Left ExporterShutDown)
        Running  -> do
          barrier <- newEmptyTMVarIO
          atomically $ do
            writeTVar barrierPending True
            writeTBQueue queue (FlushBarrier barrier)
          atomically (takeTMVar barrier)

    -- -----------------------------------------------------------------------
    -- Drop notifier
    -- -----------------------------------------------------------------------

    notifier :: TVar Int -> TVar ExporterState -> TMVar () -> IO ()
    notifier dropCounter stateVar done = loop
      where
        loop = do
          mn <- atomically $ do
            n  <- readTVar dropCounter
            st <- readTVar stateVar
            if n > 0
              then writeTVar dropCounter 0 >> pure (Just n)
              else if st /= Running
                     then pure Nothing
                     else retry
          case mn of
            Just n  -> onDroppedSpans cfg n >> loop
            Nothing -> atomically (putTMVar done ())

    -- -----------------------------------------------------------------------
    -- Worker
    -- -----------------------------------------------------------------------

    worker
      :: TVar Bool
      -> TBQueue WorkItem
      -> TVar ExporterState
      -> TMVar ()
      -> IO ()
    worker barrierPending queue stateVar done = loop
      where
        intervalMicros = toMicros (exportInterval cfg)
        timeoutMicros  = toMicros (exportTimeout  cfg)

        loop = do
          _ <- race
                 (threadDelay intervalMicros)
                 (atomically $ do
                   st <- readTVar stateVar
                   if st /= Running
                     then pure ()
                     else do
                       len     <- lengthTBQueue queue
                       pending <- readTVar barrierPending
                       if fromIntegral len >= maxExportBatch cfg || pending
                         then pure ()
                         else retry)
          processQueue
          shouldExit <- atomically $ do
            st    <- readTVar stateVar
            empty <- isEmptyTBQueue queue
            pure (st /= Running && empty)
          if shouldExit
            then atomically (putTMVar done ())
            else loop

        processQueue :: IO ()
        processQueue = do
          items <- atomically (drainItems queue (maxExportBatch cfg))
          case items of
            [] -> pure ()
            _  -> processItems items

        processItems :: [WorkItem] -> IO ()
        processItems [] = pure ()
        processItems items = do
          let (spanItems, rest) = break isBarrier items
              spans             = [ne | SpanItem ne <- spanItems]
          case NE.nonEmpty (concatMap NE.toList spans) of
            Nothing -> pure ()
            Just ne -> do
              mResult <- CE.tryJust syncOnly $
                           timeout timeoutMicros (exporterExport inner ne)
              case mResult of
                Left ex ->
                  safeLog logError
                    ("htrace: exporter threw exception: "
                      <> Text.pack (show ex))
                Right Nothing ->
                  safeLog logWarn
                    ("htrace: export timed out after "
                      <> Text.pack (show (exportTimeout cfg))
                      <> "s; "
                      <> Text.pack (show (NE.length ne))
                      <> " spans abandoned")
                Right (Just (ExportFailure err)) ->
                  safeLog logWarn
                    ("htrace: export returned failure: "
                      <> Text.pack (show err))
                Right (Just (ExportSuccess _)) ->
                  pure ()
          case rest of
            (FlushBarrier tmv : remainder) -> do
              atomically $ do
                writeTVar barrierPending False
                putTMVar tmv (Right ())
              processItems remainder
            _ ->
              pure ()

    -- -----------------------------------------------------------------------
    -- Shutdown
    -- -----------------------------------------------------------------------

    doShutdown
      :: TVar ExporterState
      -> TMVar ()
      -> TMVar ()
      -> Async ()
      -> Async ()
      -> IO ()
    doShutdown stateVar workerDone notifierDone workerAsync notifierAsync = do
      atomically (writeTVar stateVar Draining)
      let deadlineMicros = toMicros (shutdownTimeout cfg)

      workerExited <- timeout deadlineMicros (atomically (takeTMVar workerDone))
      case workerExited of
        Just () -> do
          result <- waitCatch workerAsync
          case result of
            Left ex ->
              safeLog logError
                ("htrace: worker thread crashed: " <> Text.pack (show ex))
            Right () -> pure ()
        Nothing -> do
          cancel workerAsync
          void (waitCatch workerAsync)
          safeLog logWarn
            "htrace: worker did not exit within shutdown deadline; \
            \forcing cancellation. Some spans may not have been exported."

      notifierExited <- timeout deadlineMicros (atomically (takeTMVar notifierDone))
      case notifierExited of
        Just () -> pure ()
        Nothing -> do
          cancel notifierAsync
          void (waitCatch notifierAsync)
          safeLog logWarn
            "htrace: notifier did not exit within shutdown deadline."

      atomically (writeTVar stateVar Stopped)
      exporterShutdown inner
