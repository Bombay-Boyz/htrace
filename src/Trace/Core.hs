module Trace.Core
  ( -- * Trace ID
    TraceId
  , unTraceId
  , newTraceId
  , traceIdFromBytes
    -- * Span ID
  , SpanId
  , unSpanId
  , newSpanId
  , spanIdFromBytes
    -- * Parse errors
  , IdParseError (..)
    -- * Trace flags
  , TraceFlags (..)
  , defaultTraceFlags
  , isSampled
  , setSampled
    -- * Span context
  , SpanContext (..)
    -- * Span kind
  , SpanKind (..)
    -- * Span status
  , SpanStatus (..)
  , ErrorMessage
  , unErrorMessage
  , mkErrorMessage
  , unspecifiedErrorMessage
    -- * Span name
  , SpanName (..)
  , mkSpanName
    -- * Span lifecycle types
  , SpanState (..)
  , SpanEvent (..)
  , SpanError (..)
  , SpanInternals (..)
  , Span (..)
  , readSpanInternals
  , FinishedSpan (..)
    -- * Instrumentation scope
  , InstrumentationScope (..)
    -- * Sampling
  , SamplingDecision (..)
  , Sampler (..)
  , alwaysOnSampler
  , alwaysOffSampler
  , traceIdRatioSampler
  , parentBasedSampler
  , traceIdWord64
    -- * Clock
  , Clock (..)
  , systemClock
  ) where

import Control.Concurrent.STM (TVar, readTVarIO)
import Crypto.Random (getRandomBytes)
import Data.Bits (clearBit, setBit, testBit)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime, getCurrentTime)
import Data.Word (Word8, Word64)

import Trace.Attributes (SpanAttrs)

-- ---------------------------------------------------------------------------
-- Trace ID
-- ---------------------------------------------------------------------------

newtype TraceId = TraceId { unTraceId :: ByteString }
  deriving stock (Show, Eq, Ord)

newTraceId :: IO TraceId
newTraceId = TraceId <$> getRandomBytes 16

-- ---------------------------------------------------------------------------
-- Span ID
-- ---------------------------------------------------------------------------

newtype SpanId = SpanId { unSpanId :: ByteString }
  deriving stock (Show, Eq, Ord)

newSpanId :: IO SpanId
newSpanId = SpanId <$> getRandomBytes 8

-- ---------------------------------------------------------------------------
-- Parse errors
-- ---------------------------------------------------------------------------

data IdParseError
  = WrongIdLength !Int !Int
  | AllZeroId
  deriving stock (Show, Eq)

-- ---------------------------------------------------------------------------
-- Internal helper
-- ---------------------------------------------------------------------------

mkIdFromBytes
  :: Int
  -> (ByteString -> a)
  -> ByteString
  -> Either IdParseError a
mkIdFromBytes expected ctor bs
  | BS.length bs /= expected = Left (WrongIdLength expected (BS.length bs))
  | BS.all (== 0) bs         = Left AllZeroId
  | otherwise                = Right (ctor bs)

traceIdFromBytes :: ByteString -> Either IdParseError TraceId
traceIdFromBytes = mkIdFromBytes 16 TraceId

spanIdFromBytes :: ByteString -> Either IdParseError SpanId
spanIdFromBytes = mkIdFromBytes 8 SpanId

-- ---------------------------------------------------------------------------
-- Trace flags
-- ---------------------------------------------------------------------------

newtype TraceFlags = TraceFlags { unTraceFlags :: Word8 }
  deriving stock (Show, Eq)

defaultTraceFlags :: TraceFlags
defaultTraceFlags = TraceFlags 0

isSampled :: TraceFlags -> Bool
isSampled (TraceFlags w) = testBit w 0

setSampled :: Bool -> TraceFlags -> TraceFlags
setSampled True  (TraceFlags w) = TraceFlags (setBit   w 0)
setSampled False (TraceFlags w) = TraceFlags (clearBit w 0)

-- ---------------------------------------------------------------------------
-- Span context
-- ---------------------------------------------------------------------------

data SpanContext = SpanContext
  { scTraceId    :: !TraceId
  , scSpanId     :: !SpanId
  , scParentId   :: !(Maybe SpanId)
  , scTraceFlags :: !TraceFlags
  } deriving stock (Show, Eq)

-- ---------------------------------------------------------------------------
-- Span kind
-- ---------------------------------------------------------------------------

data SpanKind
  = Server
  | Client
  | Producer
  | Consumer
  | Internal
  deriving stock (Show, Eq, Ord, Bounded, Enum)

-- ---------------------------------------------------------------------------
-- Span status
-- ---------------------------------------------------------------------------

newtype ErrorMessage = ErrorMessage { unErrorMessage :: Text }
  deriving stock (Show, Eq)

mkErrorMessage :: Text -> Maybe ErrorMessage
mkErrorMessage t
  | Text.null (Text.strip t) = Nothing
  | otherwise                = Just (ErrorMessage t)

-- | A pre-built 'ErrorMessage' used as a fallback when no message is provided.
-- Equivalent to @fromJust (mkErrorMessage "<unspecified error>")@.
unspecifiedErrorMessage :: ErrorMessage
unspecifiedErrorMessage = ErrorMessage (Text.pack "<unspecified error>")

data SpanStatus
  = StatusUnset
  | StatusOk
  | StatusError !ErrorMessage
  deriving stock (Show, Eq)

-- ---------------------------------------------------------------------------
-- Span name
-- ---------------------------------------------------------------------------

newtype SpanName = SpanName { unSpanName :: Text }
  deriving stock (Show, Eq)

mkSpanName :: Text -> Maybe SpanName
mkSpanName t
  | Text.null (Text.strip t) = Nothing
  | otherwise                = Just (SpanName t)

instance IsString SpanName where
  fromString s =
    case mkSpanName (Text.pack s) of
      Just n  -> n
      Nothing -> SpanName "<unnamed-span>"

-- ---------------------------------------------------------------------------
-- Span lifecycle
-- ---------------------------------------------------------------------------

data SpanState
  = SpanActive  !UTCTime
  | SpanEnded   !UTCTime !UTCTime
  | SpanDropped
  deriving stock (Show, Eq)

data SpanEvent = SpanEvent
  { eventName       :: !Text
  , eventTime       :: !UTCTime
  , eventAttributes :: !SpanAttrs
  } deriving stock (Show, Eq)

data SpanError
  = SpanAlreadyEnded
  | SpanWasDropped
  deriving stock (Show, Eq)

data SpanInternals = SpanInternals
  { siState      :: !SpanState
  , siStatus     :: !SpanStatus
  , siAttributes :: !SpanAttrs
  , siEvents     :: ![SpanEvent]
  } deriving stock (Show, Eq)

-- | A live, in-flight span. Not Show or Eq by design — use 'FinishedSpan'.
data Span = Span
  { spanContext   :: !SpanContext
  , spanName      :: !SpanName
  , spanKind      :: !SpanKind
  , spanClock     :: !Clock
  , spanInternals :: !(TVar SpanInternals)
  }

data FinishedSpan = FinishedSpan
  { fsContext    :: !SpanContext
  , fsName       :: !SpanName
  , fsKind       :: !SpanKind
  , fsStartTime  :: !UTCTime
  , fsEndTime    :: !UTCTime
  , fsStatus     :: !SpanStatus
  , fsAttributes :: !SpanAttrs
  , fsEvents     :: ![SpanEvent]
  } deriving stock (Show, Eq)

-- ---------------------------------------------------------------------------
-- Instrumentation scope
-- ---------------------------------------------------------------------------

data InstrumentationScope = InstrumentationScope
  { scopeName    :: !Text
  , scopeVersion :: !(Maybe Text)
  } deriving stock (Show, Eq)

-- ---------------------------------------------------------------------------
-- Sampling
-- ---------------------------------------------------------------------------

data SamplingDecision
  = Drop
  | RecordOnly
  | RecordAndSample
  deriving stock (Show, Eq)

newtype Sampler = Sampler
  { runSampler
      :: Maybe SpanContext
      -> TraceId
      -> SpanName
      -> SpanKind
      -> SpanAttrs
      -> SamplingDecision
  }

alwaysOnSampler :: Sampler
alwaysOnSampler = Sampler (\_ _ _ _ _ -> RecordAndSample)

alwaysOffSampler :: Sampler
alwaysOffSampler = Sampler (\_ _ _ _ _ -> Drop)

-- | Sample spans deterministically by trace-id using exact integer arithmetic.
-- The threshold is computed as @floor(rate * 2^64)@ using 'Integer' to avoid
-- 'Double' precision loss. A trace is sampled iff its first 8 bytes
-- (big-endian 'Word64') are strictly less than the threshold.
--
-- The rate is clamped to [0.0, 1.0] before use.
-- rate = 0.0 → nothing sampled; rate = 1.0 → all sampled.
traceIdRatioSampler :: Double -> Sampler
traceIdRatioSampler rate = Sampler $ \_ tid _ _ _ ->
  if traceIdWord64 tid < threshold then RecordAndSample else Drop
  where
    clamped :: Double
    clamped = max 0.0 (min 1.0 rate)
    threshold :: Word64
    threshold
      | clamped <= 0.0 = 0
      | clamped >= 1.0 = maxBound
      | otherwise      =
          fromIntegral
            ( floor
                ( toRational clamped
                  * (toRational (maxBound :: Word64) + 1)
                ) :: Integer
            )

-- | Extract the first 8 bytes of a 'TraceId' as a big-endian 'Word64'.
-- Exported for testing.
traceIdWord64 :: TraceId -> Word64
traceIdWord64 (TraceId bs) =
  BS.foldl' (\acc b -> acc * 256 + fromIntegral b) 0 (BS.take 8 bs)

-- | Respect the parent span's sampling decision when a parent context exists.
-- For root spans (no parent), delegates to the given sampler.
-- Sampled parent → 'RecordAndSample'; unsampled parent → 'Drop'.
parentBasedSampler :: Sampler -> Sampler
parentBasedSampler rootSampler = Sampler $ \mParent tid name kind attrs_ ->
  case mParent of
    Nothing     -> runSampler rootSampler Nothing tid name kind attrs_
    Just parent ->
      if isSampled (scTraceFlags parent)
        then RecordAndSample
        else Drop

-- ---------------------------------------------------------------------------
-- Clock
-- ---------------------------------------------------------------------------

newtype Clock = Clock { clockNow :: IO UTCTime }

systemClock :: Clock
systemClock = Clock getCurrentTime

-- | Read a consistent snapshot of a live span's internals.
-- This is the only sanctioned read path for code outside this package.
-- Writing is only possible through 'modifySpan' in "Trace.Monad".
readSpanInternals :: Span -> IO SpanInternals
readSpanInternals = readTVarIO . spanInternals