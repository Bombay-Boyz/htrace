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
      queue        <- newTBQueueIO
                        (fromIntegral (maxQueueSize cfg))

      dropChan     <- newTBQueueIO 64

      stateVar     <- newTVarIO Running

      workerDone   <- newEmptyTMVarIO
      notifierDone <- newEmptyTMVarIO

      workerAsync   <- async
                         (worker queue stateVar workerDone)

      notifierAsync <- async
                         (notifier dropChan
                                   stateVar
                                   notifierDone)

      pure $
        Right $
          SpanExporter
            { exporterExport =
                guardedEnqueue
                  stateVar
                  queue
                  dropChan

            , exporterFlush =
                guardedFlush
                  stateVar
                  queue

            , exporterShutdown =
                doShutdown
                  stateVar
                  workerDone
                  notifierDone
                  workerAsync
                  notifierAsync
            }

  where

    -- -----------------------------------------------------------------------
    -- Enqueue
    -- -----------------------------------------------------------------------

    guardedEnqueue
      :: TVar ExporterState
      -> TBQueue FinishedSpan
      -> TBQueue Int
      -> NonEmpty FinishedSpan
      -> IO ExportResult

    guardedEnqueue stateVar queue dropChan ne =
      atomically $ do
        st <- readTVar stateVar

        case st of
          Running ->
            enqueueSTM queue dropChan ne

          Draining ->
            pure (ExportFailure ExporterShutDown)

          Stopped ->
            pure (ExportFailure ExporterShutDown)

    enqueueSTM
      :: TBQueue FinishedSpan
      -> TBQueue Int
      -> NonEmpty FinishedSpan
      -> STM ExportResult

    enqueueSTM queue dropChan ne = do
      let xs = NE.toList ne
          n  = length xs

      occupied <- lengthTBQueue queue

      let space =
            maxQueueSize cfg - fromIntegral occupied

          canEnqueue =
            min n space

          nDropped =
            n - canEnqueue

      mapM_ (writeTBQueue queue)
            (take canEnqueue xs)

      when (nDropped > 0) $ do
        full <- isFullTBQueue dropChan

        when (not full) $
          writeTBQueue dropChan nDropped

      pure (ExportSuccess (n - nDropped))

    -- -----------------------------------------------------------------------
    -- Flush
    -- -----------------------------------------------------------------------

    guardedFlush
      :: TVar ExporterState
      -> TBQueue FinishedSpan
      -> IO (Either ExportError ())

    guardedFlush stateVar queue = do
      st <- readTVarIO stateVar

      case st of
        Running ->
          doFlush queue

        Draining ->
          pure (Left ExporterShutDown)

        Stopped ->
          pure (Left ExporterShutDown)

    -- -----------------------------------------------------------------------
    -- Drop notifier
    -- -----------------------------------------------------------------------

    notifier dropChan stateVar done =
      loop
      where
        loop = do
          mn <- atomically $
            (Just <$> readTBQueue dropChan)
              `orElse`
            do
              st <- readTVar stateVar
              empty <- isEmptyTBQueue dropChan

              if st /= Running && empty
                then pure Nothing
                else retry

          case mn of
            Just n -> do
              onDroppedSpans cfg n
              loop

            Nothing ->
              atomically
                (putTMVar done ())

    -- -----------------------------------------------------------------------
    -- Safe logging
    -- -----------------------------------------------------------------------

    syncOnly :: SomeException -> Maybe SomeException
    syncOnly ex =
      case CE.fromException ex of
        Just (CE.SomeAsyncException _) ->
          Nothing

        Nothing ->
          Just ex

    safeLog
      :: (InternalLogger -> Text.Text -> IO ())
      -> Text.Text
      -> IO ()

    safeLog f msg = do
      result <-
        CE.tryJust syncOnly
          (f (batchLogger cfg) msg)

      case result of
        Right () ->
          pure ()

        Left _ ->
          pure ()

    -- -----------------------------------------------------------------------
    -- Worker
    -- -----------------------------------------------------------------------

    worker queue stateVar done =
      loop
      where

        intervalMicros =
          round
            ( realToFrac (exportInterval cfg)
            * 1_000_000
            :: Double
            )

        timeoutMicros =
          round
            ( realToFrac (exportTimeout cfg)
            * 1_000_000
            :: Double
            )

        loop = do

          _ <- race
                (threadDelay intervalMicros)
                (atomically $ do
                  st <- readTVar stateVar

                  if st /= Running
                    then pure ()
                    else do
                      len <- lengthTBQueue queue

                      if fromIntegral len
                           >= maxExportBatch cfg
                        then pure ()
                        else retry
                )

          batch <-
            atomically $
              drainBatch
                queue
                (maxExportBatch cfg)

          case NE.nonEmpty batch of

            Nothing ->
              pure ()

            Just ne -> do

              mResult <-
                CE.tryJust syncOnly $
                  timeout timeoutMicros
                    (exporterExport inner ne)

              case mResult of

                Left ex ->
                  safeLog logError
                    ( "htrace: exporter threw exception: "
                    <> Text.pack (show ex)
                    )

                Right Nothing ->
                  safeLog logWarn
                    ( "htrace: export timed out after "
                    <> Text.pack
                         (show (exportTimeout cfg))
                    <> "s; "
                    <> Text.pack
                         (show (NE.length ne))
                    <> " spans abandoned"
                    )

                Right (Just (ExportFailure err)) ->
                  safeLog logWarn
                    ( "htrace: export returned failure: "
                    <> Text.pack (show err)
                    )

                Right (Just (ExportSuccess _)) ->
                  pure ()

          shouldExit <- atomically $ do
            st <- readTVar stateVar
            empty <- isEmptyTBQueue queue

            pure
              (st /= Running && empty)

          if shouldExit
            then atomically
                   (putTMVar done ())
            else loop

    -- -----------------------------------------------------------------------
    -- Flush implementation
    -- -----------------------------------------------------------------------

    doFlush queue = do

      batch <-
        atomically $
          drainBatch
            queue
            (maxQueueSize cfg)

      case NE.nonEmpty batch of

        Nothing ->
          pure (Right ())

        Just ne -> do

          let flushTimeoutMicros =
                round
                  ( realToFrac
                      (exportTimeout cfg)
                  * 1_000_000
                  :: Double
                  )

          mResult <-
            timeout flushTimeoutMicros
              (exporterExport inner ne)

          case mResult of

            Nothing ->
              pure
                (Left
                  (ExportTimeout
                    (exportTimeout cfg)
                  )
                )

            Just (ExportSuccess _) ->
              pure (Right ())

            Just (ExportFailure e) ->
              pure (Left e)

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

    doShutdown
      stateVar
      workerDone
      notifierDone
      workerAsync
      notifierAsync = do

      atomically $
        writeTVar stateVar Draining

      let deadlineMicros =
            round
              ( realToFrac
                  (shutdownTimeout cfg)
              * 1_000_000
              :: Double
              )

      workerExited <-
        timeout deadlineMicros
          (atomically
            (takeTMVar workerDone)
          )

      case workerExited of

        Just () -> do

          result <- waitCatch workerAsync

          case result of

            Left ex ->
              safeLog logError
                ( "htrace: worker thread crashed: "
                <> Text.pack (show ex)
                )

            Right () ->
              pure ()

        Nothing -> do

          cancel workerAsync

          -- Important:
          -- ensure cancellation has completed before teardown.
          void (waitCatch workerAsync)

          safeLog logWarn
            "htrace: worker did not exit within shutdown deadline; \
            \forcing cancellation. Some spans may not have been exported."

      notifierExited <-
        timeout deadlineMicros
          (atomically
            (takeTMVar notifierDone)
          )

      case notifierExited of

        Just () ->
          pure ()

        Nothing -> do

          cancel notifierAsync

          void (waitCatch notifierAsync)

          safeLog logWarn
            "htrace: notifier did not exit within shutdown deadline."

      atomically $
        writeTVar stateVar Stopped

      exporterShutdown inner

    -- -----------------------------------------------------------------------
    -- Drain helper
    -- -----------------------------------------------------------------------

    drainBatch queue maxN =
      go [] 0
      where

        go acc i
          | i >= maxN =
              pure (reverse acc)

          | otherwise =
              tryReadTBQueue queue >>= \case

                Nothing ->
                  pure (reverse acc)

                Just s ->
                  go (s : acc) (i + 1)