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
  , OverflowStrategy (..)
    -- * Concrete exporters
  , noopExporter
  , memoryExporter
    -- * Internal logging
  , InternalLogger (..)
  , stderrLogger
  , silentLogger
  , NetworkFailure(..)
  ) where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text.IO qualified as Text
import Data.Time (NominalDiffTime)
import System.IO (stderr)
import Control.Concurrent.STM
  ( atomically
  , modifyTVar'
  , newTVarIO
  , readTVarIO
  )

import Trace.Core (FinishedSpan)

-- ---------------------------------------------------------------------------
-- Exporter interface
-- ---------------------------------------------------------------------------

data SpanExporter = SpanExporter
  { exporterExport   :: NonEmpty FinishedSpan -> IO ExportResult
  , exporterFlush    :: IO (Either ExportError ())
  , exporterShutdown :: IO ()
  }

-- ---------------------------------------------------------------------------
-- Export results
-- ---------------------------------------------------------------------------

data ExportResult
  = ExportSuccess !Int
  | ExportFailure !ExportError
  deriving stock (Show, Eq)

-- NEW
data NetworkFailure
  = ConnectionRefused
  | DnsResolutionFailed
  | TlsHandshakeFailed
  | RequestTimedOut
  | OtherNetworkError
  deriving stock (Show, Eq)

data ExportError
  = NetworkError        !NetworkFailure !Text
  | MalformedResponse   !HttpStatus !Text
  | ExportTimeout       !NominalDiffTime
  | SerializationFailed !Text
  | ExporterShutDown
  deriving stock (Show, Eq)

-- ---------------------------------------------------------------------------
-- HTTP status
-- ---------------------------------------------------------------------------

newtype HttpStatus = HttpStatus { unHttpStatus :: Int }
  deriving stock (Show, Eq, Ord)

mkHttpStatus :: Int -> Maybe HttpStatus
mkHttpStatus n
  | n >= 100 && n <= 599 = Just (HttpStatus n)
  | otherwise            = Nothing

-- ---------------------------------------------------------------------------
-- Init-time errors
-- ---------------------------------------------------------------------------

data ExporterInitError
  = ExporterInvalidEndpoint        !Text
  | ExporterInvalidHeader          !Text !Text
  | ExporterUnsupportedScheme      !Text
  | ExporterUnsupportedCompression !Text
  | ExporterBatchInit              !BatchConfigError
  deriving stock (Show, Eq)

data BatchConfigError
  = NonPositiveQueueSize !Int
  | NonPositiveBatchSize !Int
  | BatchExceedsQueue    !Int !Int
  | NonPositiveInterval  !NominalDiffTime
  | NonPositiveTimeout   !NominalDiffTime
  | NonPositiveShutdownTimeout !NominalDiffTime
  deriving stock (Show, Eq)


data OverflowStrategy
  = DropNewest    -- ^ discard incoming spans when queue is full (default)
  | DropOldest    -- ^ evict oldest buffered spans to admit new ones
  | BlockProducer -- ^ block the caller until space is available
  deriving stock (Show, Eq, Enum, Bounded)

-- ---------------------------------------------------------------------------
-- Concrete exporters
-- ---------------------------------------------------------------------------

noopExporter :: SpanExporter
noopExporter = SpanExporter
  { exporterExport   = \ne -> pure (ExportSuccess (NE.length ne))
  , exporterFlush    = pure (Right ())
  , exporterShutdown = pure ()
  }

memoryExporter :: IO (SpanExporter, IO [FinishedSpan])
memoryExporter = do
  tvar <- newTVarIO ([] :: [FinishedSpan])

  let doExport ne = do
        atomically $
          modifyTVar' tvar (<> NE.toList ne)

        pure (ExportSuccess (NE.length ne))

      readAll =
        (readTVarIO tvar)

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

data InternalLogger = InternalLogger
  { logWarn  :: Text -> IO ()
  , logError :: Text -> IO ()
  }

stderrLogger :: InternalLogger
stderrLogger = InternalLogger
  { logWarn  = \t ->
      Text.hPutStrLn stderr ("[htrace WARN]  " <> t)

  , logError = \t ->
      Text.hPutStrLn stderr ("[htrace ERROR] " <> t)
  }

silentLogger :: InternalLogger
silentLogger = InternalLogger
  { logWarn  = \_ -> pure ()
  , logError = \_ -> pure ()
  }
