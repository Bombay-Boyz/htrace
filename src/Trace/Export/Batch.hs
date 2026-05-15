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

data BatchConfig = BatchConfig
  { maxQueueSize    :: !Int
  , maxExportBatch  :: !Int
  , exportInterval  :: !NominalDiffTime
  , exportTimeout   :: !NominalDiffTime
  , shutdownTimeout :: !NominalDiffTime
  , onDroppedSpans  :: !(Int -> IO ())
  , batchLogger     :: !InternalLogger
  }

defaultOnDroppedSpans :: InternalLogger -> Int -> IO ()
defaultOnDroppedSpans logger n =
  logWarn logger
    ("htrace: dropped "
      <> Text.pack (show n)
      <> " spans (queue full)")

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

-- ---------------------------------------------------------------------------
-- Validation
-- ---------------------------------------------------------------------------

validateBatchConfig :: BatchConfig -> Maybe BatchConfigError
validateBatchConfig c
  | maxQueueSize c <= 0 =
      Just (NonPositiveQueueSize (maxQueueSize c))

  | maxExportBatch c <= 0 =
      Just (NonPositiveBatchSize (maxExportBatch c))

  | maxExportBatch c > maxQueueSize c =
      Just (BatchExceedsQueue
              (maxExportBatch c)
              (maxQueueSize c))

  | exportInterval c <= 0 =
      Just (NonPositiveInterval (exportInterval c))

  | exportTimeout c <= 0 =
      Just (NonPositiveTimeout (exportTimeout c))

  | shutdownTimeout c <= 0 =
      Just (NonPositiveShutdownTimeout
              (shutdownTimeout c))

  | otherwise =
      Nothing

-- ---------------------------------------------------------------------------
-- Worker queue item
-- ---------------------------------------------------------------------------

-- | Items placed on the internal worker queue.
--
-- 'SpanItem' carries one batch of spans to export.
--
-- 'FlushBarrier' is a sentinel injected by 'exporterFlush'.  When the
-- worker encounters it, it has already exported every span that was ahead
-- of it in the queue, so it fills the 'TMVar' to unblock the caller.
-- This routes all flush requests through the single worker thread,
-- eliminating the concurrent-drain race that existed when flush drained
-- the queue directly (H-1).
data WorkItem
  = SpanItem  !(NonEmpty FinishedSpan)
  | FlushBarrier !(TMVar (Either ExportError ()))

-- ---------------------------------------------------------------------------
-- Exporter lifecycle state
-- ---------------------------------------------------------------------------

data ExporterState
  = Running
  | Draining
  | Stopped
  deriving stock (Eq, Show)

-- ---------------------------------------------------------------------------
-- Constructor
-- ---------------------------------------------------------------------------

batchExporter
  :: BatchConfig
  -> SpanExporter
  -> IO (Either BatchConfigError SpanExporter)
batchExporter cfg inner =
  case validateBatchConfig cfg of
    Just e ->
      pure (Left e)

    Nothing -> do
      queue <- newTBQueueIO (fromIntegral (maxQueueSize cfg))

      -- Atomic counter for dropped span counts.
      -- The notifier thread wakes whenever this is non-zero, reports
      -- the accumulated count to the callback, and resets it to zero —
      -- all in one STM transaction.  Using a counter instead of a
      -- bounded channel means overflow events are never silently
      -- discarded (H-4).
      dropCounter <- newTVarIO (0 :: Int)

      stateVar     <- newTVarIO Running
      workerDone   <- newEmptyTMVarIO
      notifierDone <- newEmptyTMVarIO

      workerAsync   <- async (worker   queue       stateVar workerDone)
      notifierAsync <- async (notifier dropCounter stateVar notifierDone)

      pure $ Right $ SpanExporter
        { exporterExport   = guardedEnqueue stateVar queue dropCounter
        , exporterFlush    = guardedFlush   stateVar queue
        , exporterShutdown =
            doShutdown stateVar workerDone notifierDone
                       workerAsync notifierAsync
        }

  where

    -- -----------------------------------------------------------------------
    -- Enqueue
    -- -----------------------------------------------------------------------

    guardedEnqueue
      :: TVar ExporterState
      -> TBQueue WorkItem
      -> TVar Int
      -> NonEmpty FinishedSpan
      -> IO ExportResult

    guardedEnqueue stateVar queue dropCounter ne =
      atomically $ do
        st <- readTVar stateVar
        case st of
          Running  -> enqueueSTM queue dropCounter ne
          Draining -> pure (ExportFailure ExporterShutDown)
          Stopped  -> pure (ExportFailure ExporterShutDown)

    enqueueSTM
      :: TBQueue WorkItem
      -> TVar Int
      -> NonEmpty FinishedSpan
      -> STM ExportResult

    enqueueSTM queue dropCounter ne = do
      let xs         = NE.toList ne
          n          = length xs
      occupied <- lengthTBQueue queue
      let space      = maxQueueSize cfg - fromIntegral occupied
          canEnqueue = min n space
          nDropped   = n - canEnqueue

      -- Write one SpanItem wrapping all admissible spans.
      case NE.nonEmpty (take canEnqueue xs) of
        Just batch -> writeTBQueue queue (SpanItem batch)
        Nothing    -> pure ()

      -- Accumulate drops atomically; the counter is unbounded so no
      -- overflow event is ever silently discarded (H-4).
      when (nDropped > 0) $
        modifyTVar' dropCounter (+ nDropped)

      pure (ExportSuccess (n - nDropped))

    -- -----------------------------------------------------------------------
    -- Flush
    -- -----------------------------------------------------------------------

    -- | Flush by injecting a 'FlushBarrier' into the worker queue and
    -- blocking until the worker signals it (H-1).
    --
    -- The worker exports every span ahead of the barrier before filling
    -- the 'TMVar', so this call returns only after all buffered spans
    -- have been handed to the inner exporter.  No direct queue drain or
    -- inner-exporter call happens on this thread — everything goes
    -- through the single worker path.
    guardedFlush
      :: TVar ExporterState
      -> TBQueue WorkItem
      -> IO (Either ExportError ())

    guardedFlush stateVar queue = do
      st <- readTVarIO stateVar
      case st of
        Draining -> pure (Left ExporterShutDown)
        Stopped  -> pure (Left ExporterShutDown)
        Running  -> do
          barrier <- newEmptyTMVarIO
          atomically (writeTBQueue queue (FlushBarrier barrier))
          atomically (takeTMVar barrier)

    -- -----------------------------------------------------------------------
    -- Drop notifier
    -- -----------------------------------------------------------------------

    -- | Watch the atomic drop counter and call 'onDroppedSpans' whenever
    -- the count is non-zero.  The read and reset happen in one STM
    -- transaction, so no count is ever lost (H-4).
    -- Exits when state is no longer 'Running' and the counter is zero.
    notifier
      :: TVar Int
      -> TVar ExporterState
      -> TMVar ()
      -> IO ()

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
    -- Safe logging
    -- -----------------------------------------------------------------------

    syncOnly :: SomeException -> Maybe SomeException
    syncOnly ex =
      case CE.fromException ex of
        Just (CE.SomeAsyncException _) -> Nothing
        Nothing                        -> Just ex

    safeLog
      :: (InternalLogger -> Text.Text -> IO ())
      -> Text.Text
      -> IO ()
    safeLog f msg = do
      result <- CE.tryJust syncOnly (f (batchLogger cfg) msg)
      case result of
        Right () -> pure ()
        Left  _  -> pure ()

    -- -----------------------------------------------------------------------
    -- Worker
    -- -----------------------------------------------------------------------

    worker
      :: TBQueue WorkItem
      -> TVar ExporterState
      -> TMVar ()
      -> IO ()

    worker queue stateVar done = loop
      where
        intervalMicros :: Int
        intervalMicros =
          round (realToFrac (exportInterval cfg) * 1_000_000 :: Double)

        timeoutMicros :: Int
        timeoutMicros =
          round (realToFrac (exportTimeout cfg) * 1_000_000 :: Double)

        loop = do
          -- Wake when: the timer fires, the queue has a full batch,
          -- a FlushBarrier is anywhere in the queue, or state has
          -- changed away from Running (drain-and-exit signal).
          _ <- race
                 (threadDelay intervalMicros)
                 (atomically $ do
                   st <- readTVar stateVar
                   if st /= Running
                     then pure ()
                     else do
                       len <- lengthTBQueue queue
                       if fromIntegral len >= maxExportBatch cfg
                         then pure ()
                         else do
                           -- Wake immediately when any item in the queue
                           -- is a FlushBarrier, regardless of depth.
                           found <- containsBarrier queue
                           if found then pure () else retry)

          -- Process all available items, honouring flush barriers (H-1).
          processQueue

          shouldExit <- atomically $ do
            st    <- readTVar stateVar
            empty <- isEmptyTBQueue queue
            pure (st /= Running && empty)

          if shouldExit
            then atomically (putTMVar done ())
            else loop

        -- | Drain up to 'maxExportBatch' items from the queue and
        -- process them.  Spans are collected into a batch; a
        -- 'FlushBarrier' causes the accumulated batch (if any) to be
        -- exported immediately and the barrier to be signalled before
        -- processing any further items.
        processQueue :: IO ()
        processQueue = do
          items <- atomically (drainItems (maxExportBatch cfg))
          case items of
            [] -> pure ()
            _  -> processItems items

        processItems :: [WorkItem] -> IO ()
        processItems [] = pure ()
        processItems items = do
          let (spanItems, rest) = break isBarrier items
              spans = [ne | SpanItem ne <- spanItems]

          -- Export any spans collected before the next barrier.
          case NE.nonEmpty (concatMap NE.toList spans) of
            Nothing -> pure ()
            Just ne -> do
              mResult <-
                CE.tryJust syncOnly $
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

          -- Signal the first barrier encountered (if any) and recurse.
          case rest of
            (FlushBarrier tmv : remainder) -> do
              atomically (putTMVar tmv (Right ()))
              processItems remainder
            _ ->
              pure ()

        isBarrier :: WorkItem -> Bool
        isBarrier (FlushBarrier _) = True
        isBarrier _                = False

        -- | Scan the entire queue for a 'FlushBarrier', restoring all
        -- items in their original order afterwards.
        -- 'drainAll' returns items in reverse (last-dequeued first), so
        -- feeding them back via 'unGetTBQueue' (which pushes to the front)
        -- restores the original FIFO order.
        containsBarrier :: TBQueue WorkItem -> STM Bool
        containsBarrier q = do
          items <- drainAll q
          mapM_ (unGetTBQueue q) items
          pure (any isBarrier items)

        -- | Drain every item from the queue without blocking.
        -- Returns items in reverse order (most-recently-dequeued first).
        drainAll :: TBQueue WorkItem -> STM [WorkItem]
        drainAll q = go []
          where
            go acc =
              tryReadTBQueue q >>= \case
                Nothing -> pure acc
                Just it -> go (it : acc)

        -- | Read up to 'maxN' items from the queue without blocking.
        drainItems :: Int -> STM [WorkItem]
        drainItems maxN = go [] 0
          where
            go acc i
              | i >= maxN = pure (reverse acc)
              | otherwise =
                  tryReadTBQueue queue >>= \case
                    Nothing -> pure (reverse acc)
                    Just it -> go (it : acc) (i + 1)

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

      -- Immediately stop accepting new enqueues (C-1).
      atomically (writeTVar stateVar Draining)

      let deadlineMicros :: Int
          deadlineMicros =
            round (realToFrac (shutdownTimeout cfg) * 1_000_000 :: Double)

      -- Wait for the worker to drain and exit.
      workerExited <-
        timeout deadlineMicros (atomically (takeTMVar workerDone))

      case workerExited of
        Just () -> do
          result <- waitCatch workerAsync
          case result of
            Left ex ->
              safeLog logError
                ("htrace: worker thread crashed: " <> Text.pack (show ex))
            Right () -> pure ()

        Nothing -> do
          -- Hard-cancel the worker so the inner exporter is not used
          -- after we tear it down below (M-5).
          cancel workerAsync
          void (waitCatch workerAsync)
          safeLog logWarn
            "htrace: worker did not exit within shutdown deadline; \
            \forcing cancellation. Some spans may not have been exported."

      -- Wait for the notifier to drain and exit.
      notifierExited <-
        timeout deadlineMicros (atomically (takeTMVar notifierDone))

      case notifierExited of
        Just () -> pure ()
        Nothing -> do
          cancel notifierAsync
          void (waitCatch notifierAsync)
          safeLog logWarn
            "htrace: notifier did not exit within shutdown deadline."

      atomically (writeTVar stateVar Stopped)

      exporterShutdown inner