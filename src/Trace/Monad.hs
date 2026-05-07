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
    -- * Mutators
  , setSpanAttr
  , setSpanAttrs
  , setSpanStatus
  , setStatusError
  , addEvent
  , recordException
    -- * Utilities
  , getCurrentSpanContext
  , flush
  , modifySpan
  ) where

import Control.Concurrent.STM (atomically, readTVar, writeTVar, newTVarIO)
import Control.Exception (Exception, SomeException, displayException)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask, asks, runReaderT)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Typeable (typeOf)
import UnliftIO.Exception (bracket, try)

import Trace.Attributes
import Trace.Core
import Trace.Export.Types
-- ---------------------------------------------------------------------------
-- Tracer
-- ---------------------------------------------------------------------------

-- | A configured tracing handle. Pass this to 'inSpan' and 'inSpanM'.
data Tracer = Tracer
  { tracerScope    :: !InstrumentationScope
  , tracerSampler  :: !Sampler
  , tracerExporter :: !SpanExporter
  , tracerClock    :: !Clock
  , tracerLogger   :: !InternalLogger
  }

-- ---------------------------------------------------------------------------
-- Context and monad
-- ---------------------------------------------------------------------------

-- | The ambient tracing context threaded through 'TraceM'.
data TraceContext = TraceContext
  { tcCurrentSpanContext :: !(Maybe SpanContext)
  , tcTracer             :: !Tracer
  }

-- | A 'ReaderT' monad that carries the current 'TraceContext'.
type TraceM = ReaderT TraceContext IO

-- ---------------------------------------------------------------------------
-- Mutator skeleton
-- ---------------------------------------------------------------------------

-- | Apply a pure modification to a live span's internals.
-- Returns 'Left' if the span has already ended or was dropped.
modifySpan
  :: Span
  -> (SpanInternals -> SpanInternals)
  -> IO (Either SpanError ())
modifySpan sp f = atomically $ do
  si <- readTVar (spanInternals sp)
  case siState si of
    SpanActive _  -> writeTVar (spanInternals sp) (f si) >> pure (Right ())
    SpanEnded _ _ -> pure (Left SpanAlreadyEnded)
    SpanDropped   -> pure (Left SpanWasDropped)

-- ---------------------------------------------------------------------------
-- Mutators
-- ---------------------------------------------------------------------------

-- | Set a single attribute on a live span.
setSpanAttr
  :: Span -> AttrKey -> AttrValue -> IO (Either SpanError ())
setSpanAttr sp k v = modifySpan sp $ \si ->
  si { siAttributes =
         SpanAttrs (Map.insert k v (unSpanAttrs (siAttributes si))) }

-- | Merge a list of attributes into a live span.
-- Right-biased: new values override existing ones for the same key.
setSpanAttrs
  :: Span -> [(AttrKey, AttrValue)] -> IO (Either SpanError ())
setSpanAttrs sp kvs = modifySpan sp $ \si ->
  si { siAttributes = siAttributes si <> attrs kvs }

-- | Set the status of a live span.
setSpanStatus :: Span -> SpanStatus -> IO (Either SpanError ())
setSpanStatus sp s = modifySpan sp $ \si -> si { siStatus = s }

-- | Set the span status to 'StatusError' with the given message.
-- Falls back to @\<unspecified error\>@ if the message is blank.
setStatusError :: Span -> Text -> IO (Either SpanError ())
setStatusError sp t =
  setSpanStatus sp $ StatusError $
    case mkErrorMessage t of
      Just m  -> m
      Nothing ->
         case mkErrorMessage (Text.pack "<unspecified error>") of
             Just m  -> m
             Nothing -> error "mkErrorMessage failed"

-- | Append a timestamped event to a live span.
addEvent :: Span -> Text -> SpanAttrs -> IO (Either SpanError ())
addEvent sp name evAttrs = do
  now <- clockNow (spanClock sp)
  modifySpan sp $ \si ->
    si { siEvents = SpanEvent name now evAttrs : siEvents si }

-- | Record an exception as a span event and set the status to error.
-- Follows the OpenTelemetry semantic conventions for exception events.
recordException
  :: Exception e => Span -> e -> IO (Either SpanError ())
recordException sp e = do
  let evAttrs = attrs
        [ ( AttrKey "exception.type"
          , AttrString (Text.pack (show (typeOf e))) )
        , ( AttrKey "exception.message"
          , AttrString (Text.pack (displayException e)) )
        ]
  _ <- setStatusError sp (Text.pack (displayException e))
  addEvent sp "exception" evAttrs

-- ---------------------------------------------------------------------------
-- Span lifecycle
-- ---------------------------------------------------------------------------

-- | The single span-creation primitive used by both 'inSpan' and 'inSpanM'.
-- Exposed for testing; prefer 'inSpan' or 'inSpanM' in application code.
inSpanCore
  :: Tracer
  -> Maybe SpanContext
  -> SpanName
  -> SpanKind
  -> SpanAttrs
  -> (Span -> IO a)
  -> IO a
inSpanCore tracer parent name kind initialAttrs body = do
  sid <- newSpanId
  tid <- maybe newTraceId (pure . scTraceId) parent
  let parentId  = fmap scSpanId parent
      flags0    = maybe defaultTraceFlags scTraceFlags parent
      decision  = runSampler
                    (tracerSampler tracer) parent tid name kind initialAttrs
  start <- clockNow (tracerClock tracer)
  let (state0, flags1, exportOnEnd) = case decision of
        RecordAndSample -> (SpanActive start, setSampled True  flags0, True)
        RecordOnly      -> (SpanActive start, setSampled False flags0, False)
        Drop            -> (SpanDropped,      setSampled False flags0, False)
      ctx = SpanContext tid sid parentId flags1
  tvar <- newTVarIO (SpanInternals state0 StatusUnset initialAttrs [])
  let sp = Span ctx name kind (tracerClock tracer) tvar
  bracket (pure sp) (finalize exportOnEnd) body
  where
    finalize exportOnEnd sp = do
      end <- clockNow (tracerClock tracer)
      mFinished <- atomically $ do
        si <- readTVar (spanInternals sp)
        case siState si of
          SpanActive st -> do
            let si' = si { siState = SpanEnded st end }
            writeTVar (spanInternals sp) si'
            pure $ Just $ FinishedSpan
              { fsContext    = spanContext sp
              , fsName       = spanName sp
              , fsKind       = spanKind sp
              , fsStartTime  = st
              , fsEndTime    = end
              , fsStatus     = siStatus si'
              , fsAttributes = siAttributes si'
              , fsEvents     = reverse (siEvents si')
              }
          _ -> pure Nothing
      case (exportOnEnd, mFinished) of
        (True, Just fs) -> do
          result <- try (exporterExport (tracerExporter tracer) (fs NE.:| []))
          case result of
            Left (e :: SomeException) ->
              logError (tracerLogger tracer)
                ("htrace: exporter threw during span finalisation: "
                  <> Text.pack (show e))
            Right _ -> pure ()
        _ -> pure ()

-- | Run an action inside a new root span (no parent context).
inSpan
  :: Tracer
  -> SpanName
  -> SpanKind
  -> SpanAttrs
  -> (Span -> IO a)
  -> IO a
inSpan tracer = inSpanCore tracer Nothing

-- | Run an action inside a new span, inheriting the parent context
-- from the ambient 'TraceM' environment.
inSpanM
  :: SpanName
  -> SpanKind
  -> SpanAttrs
  -> (Span -> TraceM a)
  -> TraceM a
inSpanM name kind initialAttrs body = do
  TraceContext parent tracer <- ask
  liftIO $ inSpanCore tracer parent name kind initialAttrs $ \sp ->
    runReaderT
      (body sp)
      (TraceContext (Just (spanContext sp)) tracer)

-- ---------------------------------------------------------------------------
-- Utilities
-- ---------------------------------------------------------------------------

-- | Return the current span context from the ambient 'TraceM' environment,
-- or 'Nothing' if there is no active span.
getCurrentSpanContext :: TraceM (Maybe SpanContext)
getCurrentSpanContext = asks tcCurrentSpanContext

-- | Flush all buffered spans in the tracer's exporter.
flush :: Tracer -> IO (Either ExportError ())
flush = exporterFlush . tracerExporter