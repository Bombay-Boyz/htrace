module Trace.Export.Types
  ( -- * Exporter interface
    SpanExporter (..)
    -- * Export results
  , ExportResult (..)
  , ExportError (..)
  , HttpStatus (..)
  , mkHttpStatus
  , ExporterInitError (..)
  , BatchConfigError (..)
    -- * Concrete exporters
  , noopExporter
  , memoryExporter
    -- * Internal logging
  , InternalLogger (..)
  , stderrLogger
  , silentLogger
  ) where


import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text.IO qualified as Text
import Data.Time (NominalDiffTime)
import System.IO (stderr)
import Control.Concurrent.STM (atomically, modifyTVar', newTVarIO, readTVarIO)
import Trace.Core (FinishedSpan)

-- ---------------------------------------------------------------------------
-- Exporter interface
-- ---------------------------------------------------------------------------

-- | A record-of-functions representing a span export backend.
-- All three operations must be safe to call from any thread.
data SpanExporter = SpanExporter
  { exporterExport   :: NonEmpty FinishedSpan -> IO ExportResult
    -- ^ Export a non-empty batch of finished spans.
  , exporterFlush    :: IO (Either ExportError ())
    -- ^ Block until all buffered spans have been delivered.
  , exporterShutdown :: IO ()
    -- ^ Release resources. Subsequent calls to 'exporterExport' are
    --   undefined behaviour; callers must not use the exporter after shutdown.
  }

-- ---------------------------------------------------------------------------
-- Export results
-- ---------------------------------------------------------------------------

-- | The outcome of a single 'exporterExport' call.
data ExportResult
  = ExportSuccess !Int
    -- ^ All spans were accepted. The 'Int' is the count delivered.
  | ExportFailure !ExportError
    -- ^ Export failed with the given error.
  deriving stock (Show, Eq)

-- | Errors that can be returned by an exporter.
data ExportError
  = EndpointUnreachable !Text
    -- ^ The remote endpoint could not be contacted.
  | MalformedResponse   !HttpStatus !Text
    -- ^ The endpoint returned an unexpected HTTP status.
  | ExportTimeout       !NominalDiffTime
    -- ^ The export did not complete within the allowed time.
  | SerializationFailed !Text
    -- ^ The span batch could not be serialized.
  deriving stock (Show, Eq)

-- ---------------------------------------------------------------------------
-- HTTP status
-- ---------------------------------------------------------------------------

-- | A validated HTTP status code in the range 100–599 inclusive.
-- Constructor hidden; use 'mkHttpStatus'.
newtype HttpStatus = HttpStatus { unHttpStatus :: Int }
  deriving stock (Show, Eq, Ord)

-- | Returns 'Nothing' for codes outside 100–599.
mkHttpStatus :: Int -> Maybe HttpStatus
mkHttpStatus n
  | n >= 100 && n <= 599 = Just (HttpStatus n)
  | otherwise            = Nothing

-- ---------------------------------------------------------------------------
-- Init-time errors
-- ---------------------------------------------------------------------------

-- | Errors that can arise when constructing an exporter.
data ExporterInitError
  = ExporterInvalidEndpoint   !Text
  | ExporterInvalidHeader     !Text !Text
  | ExporterUnsupportedScheme !Text
  | ExporterBatchInit         !BatchConfigError
  deriving stock (Show, Eq)

-- | Errors that can arise when validating a 'BatchConfig'.
data BatchConfigError
  = NonPositiveQueueSize !Int
  | NonPositiveBatchSize !Int
  | BatchExceedsQueue    !Int !Int
  | NonPositiveInterval  !NominalDiffTime
  | NonPositiveTimeout   !NominalDiffTime
  deriving stock (Show, Eq)

-- ---------------------------------------------------------------------------
-- Concrete exporters
-- ---------------------------------------------------------------------------

-- | An exporter that silently discards all spans.
-- Useful as a placeholder and in tests that only care about span lifecycle.
noopExporter :: SpanExporter
noopExporter = SpanExporter
  { exporterExport   = \ne -> pure (ExportSuccess (NE.length ne))
  , exporterFlush    = pure (Right ())
  , exporterShutdown = pure ()
  }

-- | An exporter that accumulates spans in memory.
-- Returns a pair of the exporter and a read action that returns all
-- spans received so far, in arrival order.
memoryExporter :: IO (SpanExporter, IO [FinishedSpan])
memoryExporter = do
  tvar <- newTVarIO ([] :: [FinishedSpan])
  let doExport ne = do
        atomically $ modifyTVar' tvar (NE.toList ne <>)
        pure (ExportSuccess (NE.length ne))
      readAll = fmap reverse (readTVarIO tvar)
  pure
    ( SpanExporter
        { exporterExport   = doExport
        , exporterFlush    = pure (Right ())
        , exporterShutdown = pure ()
        }
    , readAll
    )

-- ---------------------------------------------------------------------------
-- Internal logger
-- ---------------------------------------------------------------------------

-- | Callbacks used by htrace internals to report warnings and errors
-- without throwing exceptions into user code.
data InternalLogger = InternalLogger
  { logWarn  :: Text -> IO ()
  , logError :: Text -> IO ()
  }

-- | Writes to 'stderr' with a @[htrace WARN]@ / @[htrace ERROR]@ prefix.
stderrLogger :: InternalLogger
stderrLogger = InternalLogger
  { logWarn  = \t -> Text.hPutStrLn stderr ("[htrace WARN]  " <> t)
  , logError = \t -> Text.hPutStrLn stderr ("[htrace ERROR] " <> t)
  }

-- | Discards all log messages. Useful in tests.
silentLogger :: InternalLogger
silentLogger = InternalLogger
  { logWarn  = \_ -> pure ()
  , logError = \_ -> pure ()
  }