module Trace.Generators
  ( -- * Hedgehog generators
    genTraceId
  , genSpanId
  , genTraceFlags
  , genAttrKey
  , genAttrValue
  , genSpanAttrs
  , genSpanContext
  , genSpanContextNoParent
  , genSpanKind
  , genSpanName
  , genErrorMessage
  , genSpanStatus
  , genUTCTime
  , genSpanEvent
  , genFinishedSpan
    -- * Deterministic fixtures
  , sampleAttrs
  , sampleFinishedSpan
  , sampleSpan
  , genNonTraceparentHeader
  ) where

import Data.ByteString qualified as BS
import Data.Int (Int64)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Time (UTCTime, addUTCTime, secondsToNominalDiffTime)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..))
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Data.ByteString (ByteString)
import Trace.Attributes
import Trace.Core
import Data.CaseInsensitive qualified as CI
import Data.Text.Encoding qualified as TE
import Network.HTTP.Types (Header)

-- ---------------------------------------------------------------------------
-- ID generators
-- ---------------------------------------------------------------------------

genTraceId :: Gen TraceId
genTraceId = do
  bs <- Gen.bytes (Range.singleton 16)
  case traceIdFromBytes bs of
    Right t -> pure t
    Left _  -> Gen.discard

genSpanId :: Gen SpanId
genSpanId = do
  bs <- Gen.bytes (Range.singleton 8)
  case spanIdFromBytes bs of
    Right s -> pure s
    Left _  -> Gen.discard

-- ---------------------------------------------------------------------------
-- Flag generator
-- ---------------------------------------------------------------------------

genTraceFlags :: Gen TraceFlags
genTraceFlags = TraceFlags <$> Gen.word8 Range.linearBounded

-- ---------------------------------------------------------------------------
-- Attribute generators
-- ---------------------------------------------------------------------------

genAttrKey :: Gen AttrKey
genAttrKey = AttrKey <$> Gen.text (Range.linear 1 32) Gen.alphaNum

genAttrValue :: Gen AttrValue
genAttrValue = Gen.choice
  [ AttrString     <$> Gen.text (Range.linear 0 100) Gen.unicode
  , AttrInt        <$> Gen.integral (Range.linearBounded :: Range.Range Int64)
  , AttrDouble     <$> Gen.double  (Range.linearFracFrom 0 (-1e9) 1e9)
  , AttrBool       <$> Gen.bool
  , AttrStringList <$> Gen.list (Range.linear 0 5)
                         (Gen.text (Range.linear 0 20) Gen.unicode)
  , AttrIntList    <$> Gen.list (Range.linear 0 5)
                         (Gen.integral Range.linearBounded)
  ]

genSpanAttrs :: Gen SpanAttrs
genSpanAttrs =
  SpanAttrs . Map.fromList
    <$> Gen.list (Range.linear 0 10) ((,) <$> genAttrKey <*> genAttrValue)

-- ---------------------------------------------------------------------------
-- Span context generators
-- ---------------------------------------------------------------------------

-- | Generate a 'SpanContext' with no parent.
genSpanContextNoParent :: Gen SpanContext
genSpanContextNoParent = SpanContext
  <$> genTraceId
  <*> genSpanId
  <*> pure Nothing
  <*> genTraceFlags

-- | Generate a 'SpanContext' with an optional parent span ID.
genSpanContext :: Gen SpanContext
genSpanContext = SpanContext
  <$> genTraceId
  <*> genSpanId
  <*> Gen.maybe genSpanId
  <*> genTraceFlags

-- ---------------------------------------------------------------------------
-- Span type generators
-- ---------------------------------------------------------------------------

genSpanKind :: Gen SpanKind
genSpanKind = Gen.element [Server, Client, Producer, Consumer, Internal]

genSpanName :: Gen SpanName
genSpanName = do
  t <- Gen.text (Range.linear 1 50) Gen.alphaNum
  case mkSpanName t of
    Just n  -> pure n
    Nothing -> Gen.discard

genErrorMessage :: Gen ErrorMessage
genErrorMessage = do
  t <- Gen.text (Range.linear 1 100) Gen.unicode
  case mkErrorMessage t of
    Just m  -> pure m
    Nothing -> Gen.discard

genSpanStatus :: Gen SpanStatus
genSpanStatus = Gen.choice
  [ pure StatusUnset
  , pure StatusOk
  , StatusError <$> genErrorMessage
  ]

-- ---------------------------------------------------------------------------
-- Time generator
-- ---------------------------------------------------------------------------

-- | Generate a 'UTCTime' within a 10-year window centred on 2025-01-01.
genUTCTime :: Gen UTCTime
genUTCTime = do
  offsetSeconds <- Gen.integral
    (Range.linearFrom 0 (-157_788_000 :: Int) 157_788_000)
  pure $ addUTCTime
    (secondsToNominalDiffTime (fromIntegral offsetSeconds))
    anchor
  where
    anchor :: UTCTime
    anchor = UTCTime (fromGregorian 2025 1 1) 0

-- ---------------------------------------------------------------------------
-- Event and span generators
-- ---------------------------------------------------------------------------

genSpanEvent :: Gen SpanEvent
genSpanEvent = SpanEvent
  <$> Gen.text (Range.linear 1 30) Gen.alphaNum
  <*> genUTCTime
  <*> genSpanAttrs

-- | Generate a 'FinishedSpan' where start <= end is always guaranteed.
genFinishedSpan :: Gen FinishedSpan
genFinishedSpan = do
  ctx     <- genSpanContext
  name    <- genSpanName
  kind    <- genSpanKind
  start   <- genUTCTime
  durSecs <- Gen.integral (Range.linear 0 3600 :: Range.Range Int)
  let end = addUTCTime (secondsToNominalDiffTime (fromIntegral durSecs)) start
  status  <- genSpanStatus
  attrs_  <- genSpanAttrs
  events  <- Gen.list (Range.linear 0 5) genSpanEvent
  pure $ FinishedSpan ctx name kind start end status attrs_ events

-- ---------------------------------------------------------------------------
-- Header generator
-- ---------------------------------------------------------------------------

-- | Generate a header whose name is guaranteed not to be @traceparent@.
genNonTraceparentHeader :: Gen (CI.CI ByteString, ByteString)
genNonTraceparentHeader = do
  rawName <- TE.encodeUtf8 <$> Gen.text (Range.linear 1 20) Gen.alphaNum
  if CI.foldedCase (CI.mk rawName) == "traceparent"
    then Gen.discard
    else do
      val <- TE.encodeUtf8 <$> Gen.text (Range.linear 0 100) Gen.unicode
      pure (CI.mk rawName, val)
-- ---------------------------------------------------------------------------
-- Deterministic fixtures
-- ---------------------------------------------------------------------------

sampleAttrs :: SpanAttrs
sampleAttrs = attrs
  [ (AttrKey "k1", AttrString "hello")
  , (AttrKey "k2", AttrInt    42)
  , (AttrKey "k3", AttrBool   True)
  ]

-- | A fully-specified, known-good finished span for use in unit tests.
sampleFinishedSpan :: FinishedSpan
sampleFinishedSpan = FinishedSpan
  { fsContext = SpanContext
      { scTraceId    = either (error "sampleFinishedSpan: bad traceId") id
                         (traceIdFromBytes (BS.pack [1..16]))
      , scSpanId     = either (error "sampleFinishedSpan: bad spanId") id
                         (spanIdFromBytes (BS.pack [1..8]))
      , scParentId   = Nothing
      , scTraceFlags = setSampled True defaultTraceFlags
      }
  , fsName       = SpanName "sample"
  , fsKind       = Internal
  , fsStartTime  = UTCTime (fromGregorian 2025 1 1) 0
  , fsEndTime    = UTCTime (fromGregorian 2025 1 1) 1
  , fsStatus     = StatusOk
  , fsAttributes = attrs [(AttrKey "k", AttrString "v")]
  , fsEvents     = []
  }

-- | A family of deterministic spans differing only by index.
sampleSpan :: Int -> FinishedSpan
sampleSpan i = sampleFinishedSpan
  { fsName       = SpanName (Text.pack ("span-" <> show i))
  , fsAttributes = attrs [(AttrKey "i", AttrInt (fromIntegral i))]
  }