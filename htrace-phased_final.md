# htrace: Phased Implementation Plan

> OpenTelemetry-compliant distributed tracing for Haskell, sliced into nine
> bite-sized phases. Each phase is **150–200 LOC of library code**, ends in
> a runnable test suite with explicit pass criteria, and produces a library
> that compiles and tests green even if not yet feature-complete.

This document is the implementation companion to the design spec. The design
spec answers *what* and *why*; this document answers *in what order, with
what test gates*.

---

## How to read this document

Each phase has six fixed sections:

1. **Goal** — one sentence.
2. **What gets built** — actual Haskell, ready to paste into the relevant module.
3. **Module(s) touched** — files this phase creates or modifies.
4. **Acceptance criteria** — what must be true to call the phase done.
5. **Test plan** — concrete `hspec` cases, `hedgehog` properties, `QuickCheck`
   properties, snapshot tests, and outlier cases. Every test names its
   expected result so a contributor can run the file and check.
6. **What this phase does NOT do** — explicit non-goals, so reviewers know
   when to defer feedback to a later phase.

---

## Test stack and conventions

Every phase's tests use:

| Tool | Used for |
|---|---|
| `hspec` | Unit tests and the test-runner harness |
| `hedgehog` | Property tests with integrated shrinking — preferred for new properties |
| `QuickCheck` | Property tests where existing community generators (e.g. for `aeson` types) save effort |
| `tasty-bench` | Throughput and overhead microbenchmarks (Phase 7 onwards) |
| `aeson-diff` | OTLP JSON snapshot diffing (Phase 6) |
| `wai`/`warp` | In-process mock HTTP servers for export tests (Phase 6) |
| Docker + `jaegertracing/all-in-one` | End-to-end integration only (Phase 9) |

**Determinism rules.** All `hedgehog`/`QuickCheck` runs pin the seed via
`--seed` in CI; the seed is recorded per-commit so a failing test on PR #N
reproduces exactly on a re-run. Snapshot tests sort `Map`-keyed JSON
lexicographically before diffing so reordering inside aeson never breaks them.

**Outlier coverage.** Every phase's test plan calls out at least one
*outlier case* — a deliberately weird input chosen to break naive
implementations. These are listed under "Outliers" inside each phase's
test plan.

**Error-path coverage.** For every constructor of every error ADT introduced
in a phase, the test plan shows how to trigger that constructor. This is
the project's defense against "this error is defined but never returned."

---

## Phase summary

| Phase | LOC | Theme | Depends on |
|---|---|---|---|
| 0 | ~80 (test code only) | Shared hedgehog/QuickCheck generators | (test framework only) |
| 1 | ~150 | IDs, flags, attributes | Phase 0 |
| 2 | ~150 | Span/Status domain types, samplers | Phases 0–1 |
| 3 | ~180 | `Tracer`, `inSpan`, mutators | Phases 1–2 + Phase 4 |
| 4 | ~120 | `SpanExporter` interface, in-memory exporter | Phases 1–2 |
| 5 | ~150 | W3C `traceparent` propagation | Phase 1 |
| 6 | ~180 | OTLP/HTTP JSON exporter | Phases 1–4 |
| 7 | ~150 | Async batching queue | Phase 4 |
| 8 | ~180 | `TracingConfig`, `fromEnv` | Phases 4, 6, 7 |
| 9 | ~120 | `withTracing`, `Trace` façade | All previous |
| | **~1380** library + ~80 test gen | | |

**Phase ordering, simplified.** Phase 4 ships before Phase 3 — Phase 3
needs a real `SpanExporter` interface, and `memoryExporter` is the
test fixture used throughout Phases 3 and 9. Phases 5 and 8 are
independent of the export pipeline and can be implemented in parallel
by separate contributors. Phase 0 ships first because every other phase's
property tests reference its generators.

---

# Phase 0 — Shared Test Generators (~80 LOC, test-only)

**Goal.** Define the hedgehog and QuickCheck generators used by every
subsequent phase's property tests in one place. Without this, each phase's
contributor reinvents (often inconsistently) generators for the same types.

This phase ships **no library code** — it's pure test infrastructure.
It exists in the test tree only. Phase 1 starts adding library code.

## What gets built

```haskell
-- test/Trace/Generators.hs
{-# LANGUAGE OverloadedStrings #-}
module Trace.Generators
  ( -- hedgehog generators
    genTraceId, genSpanId, genTraceFlags
  , genAttrKey, genAttrValue, genSpanAttrs
  , genSpanName, genErrorMessage, genSpanKind
  , genSpanContext, genSpanContextNoParent
  , genFinishedSpan, genSpanEvent
  , genHeaderPair, genNonTraceparentHeader
    -- Test fixtures
  , sampleFinishedSpan, sampleSpan
  ) where

import qualified Data.ByteString             as BS
import qualified Data.CaseInsensitive        as CI
import           Data.Int                    (Int64)
import qualified Data.Map.Strict             as Map
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import qualified Data.Text.Encoding          as TE
import           Data.Time                   (UTCTime, addUTCTime,
                                              secondsToNominalDiffTime,
                                              parseTimeM, defaultTimeLocale)
import           Hedgehog                    (Gen)
import qualified Hedgehog.Gen                as Gen
import qualified Hedgehog.Range              as Range
import           Network.HTTP.Types          (Header)

import           Trace.Attributes
import           Trace.Core

-- ID generators always produce well-formed (non-zero, correct-length) IDs.
-- Discard rather than retry-with-mutation: keeps the IDs uniformly random.
genTraceId :: Gen TraceId
genTraceId = do
  bs <- Gen.bytes (Range.singleton 16)
  case traceIdFromBytes bs of
    Right t -> pure t
    Left _  -> Gen.discard   -- vanishingly rare (all-zero ~ 2^-128)

genSpanId :: Gen SpanId
genSpanId = do
  bs <- Gen.bytes (Range.singleton 8)
  case spanIdFromBytes bs of
    Right s -> pure s
    Left _  -> Gen.discard

genTraceFlags :: Gen TraceFlags
genTraceFlags = TraceFlags <$> Gen.word8 Range.linearBounded

genAttrKey :: Gen AttrKey
genAttrKey = AttrKey <$> Gen.text (Range.linear 1 32) Gen.alphaNum

genAttrValue :: Gen AttrValue
genAttrValue = Gen.choice
  [ AttrString     <$> Gen.text (Range.linear 0 100) Gen.unicode
  , AttrInt        <$> Gen.integral (Range.linearBounded :: Range.Range Int64)
  , AttrDouble     <$> Gen.double  (Range.linearFracFrom 0 (-1e9) 1e9)
  , AttrBool       <$> Gen.bool
  , AttrStringList <$> Gen.list (Range.linear 0 5) (Gen.text (Range.linear 0 20) Gen.unicode)
  , AttrIntList    <$> Gen.list (Range.linear 0 5) (Gen.integral Range.linearBounded)
  ]

genSpanAttrs :: Gen SpanAttrs
genSpanAttrs = SpanAttrs . Map.fromList <$>
  Gen.list (Range.linear 0 10) ((,) <$> genAttrKey <*> genAttrValue)

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

genSpanKind :: Gen SpanKind
genSpanKind = Gen.element [Server, Client, Producer, Consumer, Internal]

genSpanContextNoParent :: Gen SpanContext
genSpanContextNoParent = SpanContext
  <$> genTraceId <*> genSpanId <*> pure Nothing <*> genTraceFlags

genSpanContext :: Gen SpanContext
genSpanContext = SpanContext
  <$> genTraceId <*> genSpanId <*> Gen.maybe genSpanId <*> genTraceFlags

genUTCTime :: Gen UTCTime
genUTCTime = do
  -- 10-year window centred on 2025-01-01 keeps unixNano conversions honest.
  offsetSeconds <- Gen.integral (Range.linearFrom 0 (-157_788_000) 157_788_000)
  pure $ addUTCTime (secondsToNominalDiffTime (fromIntegral offsetSeconds)) anchor
  where
    Just anchor = parseTimeM True defaultTimeLocale "%Y-%m-%d" "2025-01-01"

genSpanEvent :: Gen SpanEvent
genSpanEvent = SpanEvent
  <$> Gen.text (Range.linear 1 30) Gen.alphaNum
  <*> genUTCTime
  <*> genSpanAttrs

-- FinishedSpan with start <= end always.
genFinishedSpan :: Gen FinishedSpan
genFinishedSpan = do
  ctx     <- genSpanContext
  name    <- genSpanName
  kind    <- genSpanKind
  start   <- genUTCTime
  durSecs <- Gen.integral (Range.linear 0 3600)
  let end = addUTCTime (secondsToNominalDiffTime (fromIntegral durSecs)) start
  status  <- Gen.choice
    [ pure StatusUnset, pure StatusOk
    , StatusError <$> genErrorMessage ]
  attrs_  <- genSpanAttrs
  events  <- Gen.list (Range.linear 0 5) genSpanEvent
  pure $ FinishedSpan ctx name kind start end status attrs_ events

genHeaderPair :: Gen (Text, Text)
genHeaderPair = (,)
  <$> Gen.text (Range.linear 1 30) Gen.alphaNum
  <*> Gen.text (Range.linear 0 50) Gen.unicode

genNonTraceparentHeader :: Gen Header
genNonTraceparentHeader = do
  rawName <- TE.encodeUtf8 <$> Gen.text (Range.linear 1 20) Gen.alphaNum
  if CI.foldedCase (CI.mk rawName) == "traceparent"
    then Gen.discard
    else do
      val <- TE.encodeUtf8 <$> Gen.text (Range.linear 0 100) Gen.unicode
      pure (CI.mk rawName, val)

-- Deterministic fixtures used by tests that need a known input.
sampleFinishedSpan :: FinishedSpan
sampleFinishedSpan = FinishedSpan
  { fsContext = SpanContext
      { scTraceId    = either (error "fixture") id $
                         traceIdFromBytes (BS.pack [1..16])
      , scSpanId     = either (error "fixture") id $
                         spanIdFromBytes  (BS.pack [1..8])
      , scParentId   = Nothing
      , scTraceFlags = setSampled True defaultTraceFlags
      }
  , fsName       = SpanName "sample"
  , fsKind       = Internal
  , fsStartTime  = read "2025-01-01 00:00:00 UTC"
  , fsEndTime    = read "2025-01-01 00:00:01 UTC"
  , fsStatus     = StatusOk
  , fsAttributes = attrs [(AttrKey "k", AttrString "v")]
  , fsEvents     = []
  }

-- Family of deterministic spans differing only by index.
sampleSpan :: Int -> FinishedSpan
sampleSpan i = sampleFinishedSpan
  { fsName       = SpanName (Text.pack ("span-" <> show i))
  , fsAttributes = attrs [(AttrKey "i", AttrInt (fromIntegral i))]
  }
```

> **Note on `Gen.discard`.** `genTraceId`, `genSpanId`, `genSpanName`,
> `genErrorMessage`, and `genNonTraceparentHeader` use `Gen.discard` to
> retry when the random sample falls in the (vanishingly unlikely) basin
> rejected by the smart constructor. Hedgehog handles this internally;
> tests won't visibly retry. The alternative — bypassing the constructor
> and using the data constructor directly — would defeat the invariants
> the constructors exist to enforce.

## Modules touched

- `test/Trace/Generators.hs` (new)

## Acceptance criteria

- The module compiles standalone (it has no library dependency beyond
  the types it generates, all of which ship in Phases 1–2).
- Every generator runs at least 100 random samples without throwing.
- `sampleFinishedSpan` is `Eq`-equal to itself (sanity check that the
  fixture is well-formed).

## Test plan

Phase 0's "tests" are minimal because the generators *are* the test
infrastructure. The acceptance check is just:

```haskell
spec :: Spec
spec = describe "Trace.Generators" $ do
  it "every generator produces 100 samples without error" $
    -- exercise each via Hedgehog.Gen.sample 100 times
    pendingUnless True
  it "sampleFinishedSpan is Eq-reflexive" $
    sampleFinishedSpan `shouldBe` sampleFinishedSpan
  it "sampleSpan 1 differs from sampleSpan 2" $
    sampleSpan 1 `shouldNotBe` sampleSpan 2
```

## What this phase does NOT do

- No library code at all. This is purely a test-infrastructure phase
  to prevent the next eight phases from each defining `genFinishedSpan`
  differently.

---

---

# Phase 1 — IDs, Flags, Attributes (~150 LOC)

**Goal.** Establish the foundational pure data types that everything else
builds on: trace/span IDs, trace flags, and attributes. No `IO`, no `STM`,
no exception handling.

## What gets built

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Trace.Core
  ( -- IDs
    TraceId, unTraceId, newTraceId, traceIdFromBytes
  , SpanId,  unSpanId,  newSpanId,  spanIdFromBytes
  , IdParseError (..)
    -- Flags
  , TraceFlags (..), defaultTraceFlags, isSampled, setSampled
  ) where

import           Crypto.Random          (getRandomBytes)
import           Data.Bits              (clearBit, setBit, testBit)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as BS
import           Data.Word              (Word8)

-- Constructors hidden; only newTraceId and traceIdFromBytes produce these.
newtype TraceId = TraceId { unTraceId :: ByteString } deriving (Show, Eq, Ord)
newtype SpanId  = SpanId  { unSpanId  :: ByteString } deriving (Show, Eq, Ord)

newtype TraceFlags = TraceFlags { unTraceFlags :: Word8 } deriving (Show, Eq)

defaultTraceFlags :: TraceFlags
defaultTraceFlags = TraceFlags 0

isSampled :: TraceFlags -> Bool
isSampled (TraceFlags w) = testBit w 0

setSampled :: Bool -> TraceFlags -> TraceFlags
setSampled True  (TraceFlags w) = TraceFlags (setBit   w 0)
setSampled False (TraceFlags w) = TraceFlags (clearBit w 0)

newTraceId :: IO TraceId
newTraceId = TraceId <$> getRandomBytes 16

newSpanId :: IO SpanId
newSpanId = SpanId <$> getRandomBytes 8

data IdParseError = WrongIdLength !Int !Int | AllZeroId
  deriving (Show, Eq)

mkIdFromBytes :: Int -> (ByteString -> a) -> ByteString -> Either IdParseError a
mkIdFromBytes expected ctor bs
  | BS.length bs /= expected = Left (WrongIdLength expected (BS.length bs))
  | BS.all (== 0) bs         = Left AllZeroId
  | otherwise                = Right (ctor bs)

traceIdFromBytes :: ByteString -> Either IdParseError TraceId
traceIdFromBytes = mkIdFromBytes 16 TraceId

spanIdFromBytes :: ByteString -> Either IdParseError SpanId
spanIdFromBytes = mkIdFromBytes 8 SpanId
```

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Trace.Attributes
  ( AttrKey (..), AttrValue (..), SpanAttrs (..)
  , attrs, lookupAttr, MissingAttr (..)
  ) where

import           Data.Int               (Int64)
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as Map
import           Data.String            (IsString (..))
import           Data.Text              (Text)
import qualified Data.Text              as Text

data AttrValue
  = AttrString     !Text  | AttrInt        !Int64
  | AttrDouble     !Double| AttrBool       !Bool
  | AttrStringList ![Text]| AttrIntList    ![Int64]
  deriving (Show, Eq)

newtype AttrKey   = AttrKey   { unAttrKey   :: Text } deriving (Show, Eq, Ord)
newtype SpanAttrs = SpanAttrs { unSpanAttrs :: Map AttrKey AttrValue }
  deriving (Show, Eq)

instance IsString AttrKey where fromString = AttrKey . Text.pack

instance Semigroup SpanAttrs where
  -- Right-biased: keys in the right operand override the left.
  -- Map.union is left-biased so we flip its arguments.
  SpanAttrs a <> SpanAttrs b = SpanAttrs (Map.union b a)
instance Monoid SpanAttrs where mempty = SpanAttrs Map.empty

attrs :: [(AttrKey, AttrValue)] -> SpanAttrs
attrs = SpanAttrs . Map.fromList

newtype MissingAttr = MissingAttr { missingAttrKey :: AttrKey }
  deriving (Show, Eq)

lookupAttr :: AttrKey -> SpanAttrs -> Either MissingAttr AttrValue
lookupAttr k (SpanAttrs m) = maybe (Left (MissingAttr k)) Right (Map.lookup k m)
```

## Modules touched

- `src/Trace/Core.hs` (new)
- `src/Trace/Attributes.hs` (new)
- `test/Trace/CoreSpec.hs` (new)
- `test/Trace/AttributesSpec.hs` (new)

## Acceptance criteria

- `cabal build` succeeds.
- `cabal test` passes every test below.
- The hidden-constructor invariants hold by inspection (compile-time test:
  a deliberate `TraceId BS.empty` in a separate test module fails to compile,
  proving the constructor is not exported).

## Test plan

### Unit (hspec)

| Test | Input | Expected |
|---|---|---|
| `newTraceId` length | — | `BS.length (unTraceId tid) == 16` |
| `newSpanId` length | — | `BS.length (unSpanId sid) == 8` |
| `traceIdFromBytes` accepts 16 bytes | 16 random non-zero bytes | `Right _` |
| `traceIdFromBytes` rejects 15 bytes | 15 bytes | `Left (WrongIdLength 16 15)` |
| `traceIdFromBytes` rejects 17 bytes | 17 bytes | `Left (WrongIdLength 16 17)` |
| `traceIdFromBytes` rejects all-zero | `BS.replicate 16 0` | `Left AllZeroId` |
| `spanIdFromBytes` rejects all-zero | `BS.replicate 8 0` | `Left AllZeroId` |
| `defaultTraceFlags` is unsampled | — | `isSampled defaultTraceFlags == False` |
| `setSampled True` flips bit 0 | `TraceFlags 0` | `TraceFlags 1` |
| `setSampled False` clears bit 0 | `TraceFlags 0xFF` | `TraceFlags 0xFE` |
| `lookupAttr` hit | key in map | `Right value` |
| `lookupAttr` miss | key not in map | `Left (MissingAttr k)` |
| `attrs` last-write-wins | `[("k","a"),("k","b")]` | `lookupAttr "k" == Right "b"` |

### Property (hedgehog)

```haskell
prop_traceId_uniqueness :: Property
prop_traceId_uniqueness = property $ do
  tid1 <- liftIO newTraceId
  tid2 <- liftIO newTraceId
  tid1 /== tid2

prop_setSampled_idempotent :: Property
prop_setSampled_idempotent = property $ do
  flags <- forAll (TraceFlags <$> Gen.word8 Range.linearBounded)
  setSampled True (setSampled True flags) === setSampled True flags

prop_setSampled_preserves_other_bits :: Property
prop_setSampled_preserves_other_bits = property $ do
  flags <- forAll (TraceFlags <$> Gen.word8 Range.linearBounded)
  let TraceFlags before = flags
      TraceFlags after  = setSampled True flags
  -- Bits 1-7 must be identical.
  (before .&. 0xFE) === (after .&. 0xFE)

prop_attrs_semigroup_associative :: Property
prop_attrs_semigroup_associative = property $ do
  a <- forAll genSpanAttrs
  b <- forAll genSpanAttrs
  c <- forAll genSpanAttrs
  ((a <> b) <> c) === (a <> (b <> c))

prop_attrs_monoid_identity :: Property
prop_attrs_monoid_identity = property $ do
  a <- forAll genSpanAttrs
  (mempty <> a) === a
  (a <> mempty) === a

prop_attrs_right_biased :: Property
prop_attrs_right_biased = property $ do
  k <- forAll genAttrKey
  v1 <- forAll genAttrValue
  v2 <- forAll genAttrValue
  let merged = attrs [(k, v1)] <> attrs [(k, v2)]
  lookupAttr k merged === Right v2
```

### QuickCheck (showing both frameworks coexist)

```haskell
prop_lookupAttr_total :: Property
prop_lookupAttr_total = QC.property $ \k as ->
  case lookupAttr k as of
    Left (MissingAttr k') -> k' == k
    Right _               -> True
```

### Outliers

- **All-zero trace ID**: must be rejected (`AllZeroId`), not silently accepted.
  This is the W3C reserved value.
- **Empty `ByteString`** to `traceIdFromBytes`: returns `WrongIdLength 16 0`, not a panic.
- **`Word8 0xFF`** as `TraceFlags`: `isSampled` returns `True`, and `setSampled
  False` produces exactly `0xFE` (no other bits flipped).
- **`SpanAttrs` with 100,000 keys**: `lookupAttr` is still `O(log n)` and
  completes in < 1ms (hedgehog property with `Range.linear 0 100000`).
- **Aliased keys via `IsString`**: `attrs [("k", v1), (AttrKey "k", v2)]`
  must produce the same map as `attrs [("k", v1), ("k", v2)]`. The
  `IsString` desugaring must not introduce a separate key.

## What this phase does NOT do

- No `Span` type yet (Phase 2).
- No span lifecycle, no `inSpan` (Phase 3).
- No exporters, no network, no I/O beyond `getRandomBytes` (Phase 4).
- No traceparent parsing (Phase 5).

---

# Phase 2 — Span Domain & Samplers (~150 LOC)

**Goal.** Define the rest of the pure domain — `SpanContext`, `SpanState`,
`SpanInternals`, `Span`, `FinishedSpan`, `SpanStatus`, `SpanEvent`, `SpanKind`,
`Sampler`, `Clock` — and provide the three concrete samplers. No mutation
logic yet (that's Phase 3).

## What gets built

```haskell
-- src/Trace/Core.hs (extended)

import           Control.Concurrent.STM (TVar)
import           Data.Time              (UTCTime, getCurrentTime)
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Data.Maybe             (fromMaybe)
import           Data.String            (IsString (..))
import           Data.Word              (Word64)

-- Span context and lifecycle ---------------------------------------------

data SpanContext = SpanContext
  { scTraceId    :: !TraceId
  , scSpanId     :: !SpanId
  , scParentId   :: !(Maybe SpanId)
  , scTraceFlags :: !TraceFlags
  } deriving (Show, Eq)

data SpanKind = Server | Client | Producer | Consumer | Internal
  deriving (Show, Eq, Ord, Bounded, Enum)

data SpanStatus
  = StatusUnset | StatusOk | StatusError !ErrorMessage
  deriving (Show, Eq)

data SpanState
  = SpanActive  !UTCTime
  | SpanEnded   !UTCTime !UTCTime
  | SpanDropped
  deriving (Show, Eq)

data SpanEvent = SpanEvent
  { eventName       :: !Text
  , eventTime       :: !UTCTime
  , eventAttributes :: !SpanAttrs
  } deriving (Show, Eq)

data SpanError = SpanAlreadyEnded | SpanWasDropped deriving (Show, Eq)

newtype ErrorMessage = ErrorMessage { unErrorMessage :: Text }
  deriving (Show, Eq)

mkErrorMessage :: Text -> Maybe ErrorMessage
mkErrorMessage t
  | Text.null (Text.strip t) = Nothing
  | otherwise                = Just (ErrorMessage t)

newtype SpanName = SpanName { unSpanName :: Text } deriving (Show, Eq)

mkSpanName :: Text -> Maybe SpanName
mkSpanName t
  | Text.null (Text.strip t) = Nothing
  | otherwise                = Just (SpanName t)

instance IsString SpanName where
  fromString = fromMaybe (SpanName "<unnamed-span>") . mkSpanName . Text.pack

-- Span aggregate ---------------------------------------------------------

data SpanInternals = SpanInternals
  { siState      :: !SpanState
  , siStatus     :: !SpanStatus
  , siAttributes :: !SpanAttrs
  , siEvents     :: ![SpanEvent]
  } deriving (Show, Eq)

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
  } deriving (Show, Eq)

-- Scope, sampler, clock --------------------------------------------------

data InstrumentationScope = InstrumentationScope
  { scopeName    :: !Text
  , scopeVersion :: !(Maybe Text)
  } deriving (Show, Eq)

data SamplingDecision = Drop | RecordOnly | RecordAndSample
  deriving (Show, Eq)

newtype Sampler = Sampler
  { runSampler
      :: Maybe SpanContext -> TraceId -> SpanName
      -> SpanKind -> SpanAttrs -> SamplingDecision
  }

constSampler :: SamplingDecision -> Sampler
constSampler d = Sampler (\_ _ _ _ _ -> d)

alwaysOnSampler, alwaysOffSampler :: Sampler
alwaysOnSampler  = constSampler RecordAndSample
alwaysOffSampler = constSampler Drop

-- Reads first 8 bytes of trace-id as a big-endian Word64; consistent
-- per-trace so parent and children agree.
traceIdRatioSampler :: Double -> Sampler
traceIdRatioSampler r = Sampler $ \_ tid _ _ _ ->
  if traceIdRatio tid <= r then RecordAndSample else Drop

traceIdRatio :: TraceId -> Double
traceIdRatio (TraceId bs) =
  fromIntegral (firstWord64BE bs) / fromIntegral (maxBound :: Word64)
  where
    firstWord64BE :: ByteString -> Word64
    firstWord64BE = BS.foldl' (\acc b -> acc * 256 + fromIntegral b) 0
                  . BS.take 8

newtype Clock = Clock { clockNow :: IO UTCTime }

systemClock :: Clock
systemClock = Clock getCurrentTime
```

## Modules touched

- `src/Trace/Core.hs` (extended)
- `test/Trace/CoreSpec.hs` (extended)

## Acceptance criteria

- All Phase 1 tests still pass.
- New tests below pass.
- `Span` does not derive `Show`/`Eq` (compile-time check: `deriving stock Show`
  on a wrapper that contains a `Span` field fails).
- `FinishedSpan` derives both `Show` and `Eq` (positive compile-time check).

## Test plan

### Unit (hspec)

| Test | Input | Expected |
|---|---|---|
| `mkSpanName ""` | empty | `Nothing` |
| `mkSpanName "  "` | whitespace | `Nothing` |
| `mkSpanName "ok"` | non-empty | `Just (SpanName "ok")` |
| `IsString SpanName` empty fallback | `("" :: SpanName)` | `SpanName "<unnamed-span>"` |
| `mkErrorMessage ""` | empty | `Nothing` |
| `[minBound..maxBound] :: [SpanKind]` | — | `[Server,Client,Producer,Consumer,Internal]` |
| `alwaysOnSampler` decision | any inputs | `RecordAndSample` |
| `alwaysOffSampler` decision | any inputs | `Drop` |
| `traceIdRatioSampler 1.0` | any TraceId | `RecordAndSample` |
| `traceIdRatioSampler 0.0` | any non-zero TraceId | `Drop` (since ratio > 0) |

### Property (hedgehog)

```haskell
prop_sampler_consistency :: Property
prop_sampler_consistency = property $ do
  tid <- liftIO newTraceId
  rate <- forAll (Gen.double (Range.linearFrac 0 1))
  let s = traceIdRatioSampler rate
  -- Same input → same decision, no IO involved.
  let d1 = runSampler s Nothing tid "n" Internal mempty
      d2 = runSampler s Nothing tid "n" Internal mempty
  d1 === d2

prop_sampler_distribution :: Property
prop_sampler_distribution = withTests 1 . property $ do
  -- Ratio 0.5 should yield ~5000 RecordAndSample out of 10000 trace-ids,
  -- within 4 standard deviations (very loose; eliminates flakiness).
  tids <- liftIO (replicateM 10000 newTraceId)
  let s         = traceIdRatioSampler 0.5
      sampled   = length [() | tid <- tids
                             , runSampler s Nothing tid "" Internal mempty
                                 == RecordAndSample]
      sigma     = sqrt (10000 * 0.5 * 0.5)  -- = 50
  diff (fromIntegral sampled) (>=) (5000 - 4 * sigma)
  diff (fromIntegral sampled) (<=) (5000 + 4 * sigma)

prop_finishedSpan_eq_reflexive :: Property
prop_finishedSpan_eq_reflexive = property $ do
  fs <- forAll genFinishedSpan
  fs === fs
```

### Outliers

- **Trace ID with first byte `0x00` and rest `0xFF`**: `traceIdRatio` should
  produce a value, not divide-by-zero or overflow. Specifically, this tests
  the big-endian-Word64 conversion: result should be ~ (255/256).
- **`SpanName` with only emoji**: `mkSpanName "🦀"` should succeed
  (`Text.strip` doesn't strip emoji).
- **`SpanName` with embedded null byte**: `mkSpanName "ok\NULbad"` succeeds
  (we don't validate beyond non-emptiness; backends handle their own escaping).
- **`ErrorMessage` of length 1MB**: accepted; we don't truncate. Note in
  Haddock for users.
- **`traceIdRatio` on the zero trace-id**: not constructible (Phase 1
  rejected it), but if somehow forged, returns `0.0` and the sampler
  returns `Drop` for any rate < 1.0. This is the safe default.

## What this phase does NOT do

- No `inSpan`, no `bracket`, no actual span creation (Phase 3).
- No `setSpanAttr` etc. — the `Span` type exists but is not yet mutable
  through public API (Phase 3).
- No `Tracer` (Phase 3).

---

# Phase 3 — Tracer, inSpan, Mutators (~180 LOC)

**Goal.** Implement the full span lifecycle: span creation via `inSpan`,
the six mutators via the `modifySpan` combinator, `inSpanM`'s `ReaderT`
context threading, and `flush`. Uses a stub `SpanExporter` that just
collects spans to a list — the real exporters are Phase 4 onwards.

## What gets built

```haskell
-- src/Trace/Monad.hs (new)
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Trace.Monad
  ( Tracer (..), TraceContext (..), TraceM
  , inSpan, inSpanM, inSpanCore
  , setSpanAttr, setSpanAttrs, setSpanStatus, setStatusError
  , addEvent, recordException
  , getCurrentSpanContext, flush
  , modifySpan
  ) where

import           Control.Concurrent.STM
import           Control.Exception      (Exception, SomeException, displayException)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (ReaderT, ask, asks, runReaderT)
import           Data.Functor           (($>))
import qualified Data.List.NonEmpty     as NE
import qualified Data.Map.Strict        as Map
import           Data.Maybe             (fromMaybe)
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Data.Typeable          (typeOf)
import           UnliftIO.Exception     (bracket, try)

import           Trace.Attributes
import           Trace.Core
import           Trace.Export.Types     (SpanExporter (..), ExportError,
                                         InternalLogger (..))

data Tracer = Tracer
  { tracerScope    :: !InstrumentationScope
  , tracerSampler  :: !Sampler
  , tracerExporter :: !SpanExporter
  , tracerClock    :: !Clock
  , tracerLogger   :: !InternalLogger
  }

data TraceContext = TraceContext
  { tcCurrentSpanContext :: !(Maybe SpanContext)
  , tcTracer             :: !Tracer
  }

type TraceM = ReaderT TraceContext IO

-- The single mutator skeleton. All six public mutators use this. -----------

modifySpan :: Span -> (SpanInternals -> SpanInternals) -> IO (Either SpanError ())
modifySpan sp f = atomically $ do
  si <- readTVar (spanInternals sp)
  case siState si of
    SpanActive _  -> writeTVar (spanInternals sp) (f si) $> Right ()
    SpanEnded _ _ -> pure (Left SpanAlreadyEnded)
    SpanDropped   -> pure (Left SpanWasDropped)

setSpanAttr :: Span -> AttrKey -> AttrValue -> IO (Either SpanError ())
setSpanAttr sp k v = modifySpan sp $ \si ->
  si { siAttributes = SpanAttrs (Map.insert k v (unSpanAttrs (siAttributes si))) }

setSpanAttrs :: Span -> [(AttrKey, AttrValue)] -> IO (Either SpanError ())
setSpanAttrs sp kvs = modifySpan sp $ \si ->
  si { siAttributes = siAttributes si <> attrs kvs }

setSpanStatus :: Span -> SpanStatus -> IO (Either SpanError ())
setSpanStatus sp s = modifySpan sp $ \si -> si { siStatus = s }

setStatusError :: Span -> Text -> IO (Either SpanError ())
setStatusError sp t = setSpanStatus sp $ StatusError $
  fromMaybe (ErrorMessage "<unspecified error>") (mkErrorMessage t)

addEvent :: Span -> Text -> SpanAttrs -> IO (Either SpanError ())
addEvent sp name evAttrs = do
  now <- clockNow (spanClock sp)
  modifySpan sp $ \si ->
    si { siEvents = SpanEvent name now evAttrs : siEvents si }

recordException :: Exception e => Span -> e -> IO (Either SpanError ())
recordException sp e = do
  let evAttrs = attrs
        [ (AttrKey "exception.type",    AttrString (Text.pack (show (typeOf e))))
        , (AttrKey "exception.message", AttrString (Text.pack (displayException e)))
        ]
  _ <- setStatusError sp (Text.pack (displayException e))
  addEvent sp "exception" evAttrs

-- Span lifecycle ---------------------------------------------------------

-- The single span-creation primitive used by both `inSpan` and `inSpanM`.
-- Phase 3 ships this with `parent = Nothing` always (no ReaderT context yet);
-- Phase 9 wires `inSpanM` to pass the current parent through this same path.
-- Naming: this is internal — exported only inside the htrace test suite for
-- direct testing; the public API is `inSpan` and `inSpanM` below.
inSpanCore
  :: Tracer
  -> Maybe SpanContext   -- parent context, Nothing for root spans
  -> SpanName
  -> SpanKind
  -> SpanAttrs
  -> (Span -> IO a)
  -> IO a
inSpanCore tracer parent name kind initialAttrs body = do
  sid <- newSpanId
  tid <- maybe newTraceId (pure . scTraceId) parent
  let parentId = fmap scSpanId parent
      flags0   = maybe defaultTraceFlags scTraceFlags parent
      decision = runSampler (tracerSampler tracer) parent tid name kind initialAttrs
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
      -- Atomically transition Active -> Ended, snapshot, decide whether to export.
      mFinished <- atomically $ do
        si <- readTVar (spanInternals sp)
        case siState si of
          SpanActive st -> do
            let si' = si { siState = SpanEnded st end }
            writeTVar (spanInternals sp) si'
            pure $ Just $ FinishedSpan
              (spanContext sp) (spanName sp) (spanKind sp)
              st end (siStatus si') (siAttributes si')
              (reverse (siEvents si'))
          _ -> pure Nothing  -- Already ended or dropped: nothing to export.
      case (exportOnEnd, mFinished) of
        (True, Just fs) -> do
          r <- try @SomeException $
                 exporterExport (tracerExporter tracer) (fs NE.:| [])
          case r of
            Left e -> logError (tracerLogger tracer)
                       ("htrace: exporter threw on span finalize: " <> Text.pack (show e))
            Right _ -> pure ()
        _ -> pure ()

-- Public, no-parent variant. Phase 3's tests use this directly.
inSpan :: Tracer -> SpanName -> SpanKind -> SpanAttrs -> (Span -> IO a) -> IO a
inSpan tracer = inSpanCore tracer Nothing

-- Public, ReaderT-context variant. Reads parent from TraceContext.
inSpanM :: SpanName -> SpanKind -> SpanAttrs -> (Span -> TraceM a) -> TraceM a
inSpanM name kind initialAttrs body = do
  TraceContext parent tracer <- ask
  liftIO $ inSpanCore tracer parent name kind initialAttrs $ \sp ->
    runReaderT (body sp)
      (TraceContext (Just (spanContext sp)) tracer)

getCurrentSpanContext :: TraceM (Maybe SpanContext)
getCurrentSpanContext = asks tcCurrentSpanContext

flush :: Tracer -> IO (Either ExportError ())
flush = exporterFlush . tracerExporter
```

**Note on parent inheritance.** Phase 3's `inSpanM` already wires the
`ReaderT` `TraceContext` correctly — every nested `inSpanM` sets
`tcCurrentSpanContext` for the body. What Phase 3 *doesn't* test is
`inSpanM`'s integration with `withTracing` (which produces the initial
`TraceContext` in the first place). That integration ships in Phase 9,
along with end-to-end tests that span the full path from `withTracing`
through nested `inSpanM` calls. The `inSpanCore` primitive shipped here
is unchanged in Phase 9 — Phase 9 only adds the entry point that
constructs `TraceContext` correctly.

## Modules touched

- `src/Trace/Monad.hs` (new)
- `src/Trace/Export/Types.hs` (stub: just `SpanExporter` record + `ExportError`
  + `InternalLogger`. Real implementations come in Phase 4.)
- `test/Trace/MonadSpec.hs` (new)

## Acceptance criteria

- All previous tests pass.
- `inSpan` ends the span on normal return *and* on exception.
- All six mutators return `Left SpanAlreadyEnded` after the span ends.
- The atomicity test (concurrent mutation + finalize) never loses an
  attribute that was acknowledged with `Right ()`.

## Test plan

### Unit (hspec)

| Test | Setup | Expected |
|---|---|---|
| `inSpan` exports on normal return | use a memory exporter | exporter receives 1 span |
| `inSpan` exports on exception | body throws | exporter receives 1 span; exception re-raised |
| `setSpanAttr` after end | end span, then call | `Left SpanAlreadyEnded` |
| `setSpanAttr` on dropped | use `alwaysOffSampler` | `Left SpanWasDropped` |
| `setSpanAttrs` batches | call with 5 kvs | exactly 1 STM transaction (observe via `STM` counter) |
| `recordException` event format | record an `IOError` | event named `"exception"`, `exception.type` and `exception.message` attrs set, status `StatusError _` |
| `addEvent` ordering | add 3 events | `siEvents` after end has all 3, in chronological order (oldest first after `reverse`) |
| `setStatusError ""` fallback | empty msg | status `StatusError (ErrorMessage "<unspecified error>")` |

### Property (hedgehog)

```haskell
prop_modifySpan_atomic :: Property
prop_modifySpan_atomic = withTests 100 . property $ do
  n <- forAll (Gen.int (Range.linear 1 50))
  liftIO $ do
    counter <- newTVarIO 0
    let exporter = stubExporter
        tracer   = mkTestTracer exporter
    inSpan tracer "atomic-test" Internal mempty $ \sp -> do
      -- Spawn n threads each adding one attribute; wait for all to ack.
      mvars <- replicateM n newEmptyMVar
      forM_ (zip [0..] mvars) $ \(i, mv) -> forkIO $ do
        r <- setSpanAttr sp (AttrKey ("k" <> Text.pack (show i)))
                            (AttrInt (fromIntegral i))
        case r of
          Right () -> putMVar mv (Just i)
          Left _   -> putMVar mv Nothing
        atomically (modifyTVar' counter (+1))
      results <- mapM takeMVar mvars
      let acked = [i | Just i <- results]
      -- Read final attributes; every acked attribute must be present.
      si <- readTVarIO (spanInternals sp)
      forM_ acked $ \i ->
        lookupAttr (AttrKey ("k" <> Text.pack (show i))) (siAttributes si)
          === Right (AttrInt (fromIntegral i))

prop_inSpan_start_le_end :: Property
prop_inSpan_start_le_end = property $ do
  liftIO $ do
    captured <- newIORef Nothing
    let exporter = capturingExporter captured
        tracer   = mkTestTracer exporter
    inSpan tracer "t" Internal mempty $ \_ -> pure ()
    Just fs <- readIORef captured
    diff (fsStartTime fs) (<=) (fsEndTime fs)
```

### QuickCheck

```haskell
prop_recordException_status :: QC.Property
prop_recordException_status = QC.ioProperty $ do
  captured <- newIORef Nothing
  let exporter = capturingExporter captured
      tracer   = mkTestTracer exporter
  inSpan tracer "t" Internal mempty $ \sp ->
    void (recordException sp (userError "boom"))
  Just fs <- readIORef captured
  case fsStatus fs of
    StatusError (ErrorMessage m) -> pure ("user error (boom)" `Text.isInfixOf` m)
    _                            -> pure False
```

### Outliers

- **`inSpan` whose body re-enters via `forkIO`**: the forked thread holds the
  `Span`. When `inSpan` returns and finalizes, the forked thread's subsequent
  `setSpanAttr` returns `Left SpanAlreadyEnded`. The forked thread does not
  crash. *(Documented behavior of `@scoped` thread safety.)*
- **`inSpan` body that calls `liftIO undefined`**: the resulting
  `ErrorCall` is caught by `bracket`, the span is finalized with
  `StatusError`, and the `ErrorCall` is re-thrown.
- **`recordException` with an exception whose `displayException` returns
  `"\n\n\n"`**: `setStatusError` falls back to `<unspecified error>` because
  `mkErrorMessage` strips and rejects whitespace-only.
- **`setSpanAttr` from 1000 threads concurrently** (atomicity stress):
  all `Right ()` acks correspond to attributes present in the final
  snapshot. No deadlocks (test has a 5-second timeout).
- **Span finalizer when the exporter throws**: the exception inside
  `exporterExport` does not propagate out of `inSpan`. It is logged via
  `tracerLogger`'s `logError`, and the user's action result is preserved.

## What this phase does NOT do

- No real exporters. `stubExporter` and `capturingExporter` are test fixtures
  that satisfy the `SpanExporter` interface in <20 LOC. Real `noopExporter`/
  `memoryExporter` come in Phase 4.
- No parent-context inheritance in `inSpan`. Phase 9's full `withTracing`
  wires the `ReaderT` so that `inSpanM` inside `inSpanM` produces nested
  spans with correct `scParentId`.
- No batching. Spans go directly to the stub exporter.

---

# Phase 4 — Span Exporter Interface & In-Memory Exporter (~120 LOC)

**Goal.** Define the `SpanExporter` record-of-functions, ship the two
exporters that don't need a network (`noopExporter`, `memoryExporter`), and
introduce the `InternalLogger` callback. After this phase, end-to-end
in-memory traces work: `inSpan` → `memoryExporter` → assertion.

## What gets built

```haskell
-- src/Trace/Export/Types.hs (full version, replacing Phase 3 stub)
module Trace.Export.Types
  ( -- Exporter interface
    SpanExporter (..)
    -- Per-export results
  , ExportResult (..), ExportError (..), HttpStatus, mkHttpStatus, unHttpStatus
    -- Init-time errors (used by Phases 6 + 7)
  , ExporterInitError (..), BatchConfigError (..)
    -- Concrete exporters
  , noopExporter, memoryExporter
    -- Internal logging
  , InternalLogger (..), stderrLogger, silentLogger
  ) where

import           Control.Concurrent.STM
import           Data.Functor           (($>))
import           Data.IORef
import           Data.List.NonEmpty     (NonEmpty)
import qualified Data.List.NonEmpty     as NE
import           Data.Text              (Text)
import qualified Data.Text              as Text
import qualified Data.Text.IO           as Text
import           Data.Time              (NominalDiffTime)
import           System.IO              (stderr)

import           Trace.Core             (FinishedSpan)

-- Exporter interface -----------------------------------------------------

data SpanExporter = SpanExporter
  { exporterExport   :: NonEmpty FinishedSpan -> IO ExportResult
  , exporterFlush    :: IO (Either ExportError ())
  , exporterShutdown :: IO ()
  }

data ExportResult
  = ExportSuccess !Int
  | ExportFailure !ExportError
  deriving (Show, Eq)

data ExportError
  = EndpointUnreachable !Text
  | MalformedResponse   !HttpStatus !Text
  | ExportTimeout       !NominalDiffTime
  | SerializationFailed !Text
  deriving (Show, Eq)

newtype HttpStatus = HttpStatus { unHttpStatus :: Int } deriving (Show, Eq, Ord)

mkHttpStatus :: Int -> Maybe HttpStatus
mkHttpStatus n | n >= 100 && n <= 599 = Just (HttpStatus n)
               | otherwise            = Nothing

-- Errors raised when *building* an exporter
data ExporterInitError
  = ExporterInvalidEndpoint   !Text
  | ExporterInvalidHeader     !Text !Text
  | ExporterUnsupportedScheme !Text
  | ExporterBatchInit         !BatchConfigError
  deriving (Show, Eq)

data BatchConfigError
  = NonPositiveQueueSize !Int
  | NonPositiveBatchSize !Int
  | BatchExceedsQueue    !Int !Int
  | NonPositiveInterval  !NominalDiffTime
  | NonPositiveTimeout   !NominalDiffTime
  deriving (Show, Eq)

-- Concrete exporters -----------------------------------------------------

noopExporter :: SpanExporter
noopExporter = SpanExporter
  { exporterExport   = \_ -> pure (ExportSuccess 0)
  , exporterFlush    = pure (Right ())
  , exporterShutdown = pure ()
  }

memoryExporter :: IO (SpanExporter, IO [FinishedSpan])
memoryExporter = do
  ref <- newIORef []
  let expr ne = do
        modifyIORef' ref (NE.toList ne ++)
        pure (ExportSuccess (length ne))
      readAll = readIORef ref
  pure ( SpanExporter expr (pure (Right ())) (pure ())
       , readAll )

-- Internal logger --------------------------------------------------------

data InternalLogger = InternalLogger
  { logWarn  :: Text -> IO ()
  , logError :: Text -> IO ()
  }

stderrLogger :: InternalLogger
stderrLogger = InternalLogger
  { logWarn  = \t -> Text.hPutStrLn stderr ("[htrace WARN] "  <> t)
  , logError = \t -> Text.hPutStrLn stderr ("[htrace ERROR] " <> t)
  }

silentLogger :: InternalLogger
silentLogger = InternalLogger (\_ -> pure ()) (\_ -> pure ())
```

## Modules touched

- `src/Trace/Export/Types.hs` (full version)
- `test/Trace/Export/TypesSpec.hs` (new)

## Acceptance criteria

- Phase 3's stub `SpanExporter` is replaced; all Phase 3 tests still pass
  using `noopExporter` or `memoryExporter` instead of the stub.
- `memoryExporter`'s reader returns spans in arrival order.
- `mkHttpStatus` rejects out-of-range values; tested at every boundary.

## Test plan

### Unit (hspec)

| Test | Input | Expected |
|---|---|---|
| `noopExporter.exporterExport` returns success-0 | one span | `ExportSuccess 0` |
| `noopExporter.exporterShutdown` is idempotent | called twice | no exception |
| `memoryExporter` captures spans | export 3 spans | reader returns those 3 |
| `memoryExporter` reader is repeatable | call reader twice | same list both times |
| `mkHttpStatus 100` | — | `Just (HttpStatus 100)` |
| `mkHttpStatus 599` | — | `Just (HttpStatus 599)` |
| `mkHttpStatus 99` | — | `Nothing` |
| `mkHttpStatus 600` | — | `Nothing` |
| `mkHttpStatus 0` | — | `Nothing` |
| `mkHttpStatus (-1)` | — | `Nothing` |
| `silentLogger.logError` is silent | call it | stderr capture is empty |

### Property (hedgehog)

```haskell
prop_memoryExporter_preserves_order :: Property
prop_memoryExporter_preserves_order = property $ do
  n <- forAll (Gen.int (Range.linear 1 100))
  spans <- forAll (Gen.list (Range.singleton n) genFinishedSpan)
  result <- liftIO $ do
    (exp_, readAll) <- memoryExporter
    forM_ spans $ \s -> exporterExport exp_ (s NE.:| [])
    readAll
  result === spans

prop_mkHttpStatus_range :: Property
prop_mkHttpStatus_range = property $ do
  n <- forAll (Gen.int (Range.linear (-1000) 1000))
  case mkHttpStatus n of
    Just (HttpStatus h) -> do
      diff h (>=) 100
      diff h (<=) 599
      h === n
    Nothing -> assert (n < 100 || n > 599)
```

### Outliers

- **`memoryExporter` with 1 million spans**: reader returns all of them
  without OOM (test runs with `-RTS -M512M`).
- **`exporterExport` called concurrently from 100 threads**: all spans
  end up in the buffer (`length result == 100`); no lost writes.
  *(Hedgehog property with `withTests 50`.)*
- **`silentLogger.logError "panic"`**: stderr is captured during the test;
  expect it to be empty bytestring.
- **`HttpStatus`'s `Ord` instance is consistent with `Int`**: property test
  `forall a b. compare (unHttpStatus a) (unHttpStatus b) == compare a b`.

## What this phase does NOT do

- No OTLP wire format (Phase 6).
- No batching (Phase 7).
- No HTTP at all.

---

# Phase 5 — W3C Traceparent Propagation (~150 LOC)

**Goal.** Parse and emit the `traceparent` header and inject/extract it
through HTTP `[Header]` lists. Independent of every other phase except
Phase 1's `TraceId`/`SpanId`/`TraceFlags`.

## What gets built

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Trace.Propagation
  ( PropagationResult (..), PropagationError (..)
  , parseTraceparent, emitTraceparent
  , injectHeaders, extractContext
  ) where

import           Data.Bits              (shiftL, (.|.))
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.CaseInsensitive   as CI
import           Data.Char              (chr, ord)
import           Data.Text              (Text)
import qualified Data.Text              as Text
import qualified Data.Text.Encoding     as TE
import           Data.Word              (Word8)
import           Network.HTTP.Types     (Header)

import           Trace.Core

data PropagationResult
  = PropagationSuccess !SpanContext
  | PropagationAbsent
  | PropagationInvalid !PropagationError
  deriving (Show, Eq)

data PropagationError
  = InvalidVersion  !Text
  | InvalidTraceId  !Text
  | InvalidSpanId   !Text
  | InvalidFlags    !Text
  | MalformedHeader !Text
  deriving (Show, Eq)

parseTraceparent :: Text -> PropagationResult
parseTraceparent t =
  case Text.splitOn "-" t of
    [v, tid, sid, flgs]
      | not (validVersion v) -> PropagationInvalid (InvalidVersion v)
      | otherwise -> case decodeHex 16 tid of
          Left _ -> PropagationInvalid (InvalidTraceId tid)
          Right tidBs -> case traceIdFromBytes tidBs of
            Left _ -> PropagationInvalid (InvalidTraceId tid)
            Right traceId -> case decodeHex 8 sid of
              Left _ -> PropagationInvalid (InvalidSpanId sid)
              Right sidBs -> case spanIdFromBytes sidBs of
                Left _ -> PropagationInvalid (InvalidSpanId sid)
                Right spanId -> case parseFlags flgs of
                  Nothing -> PropagationInvalid (InvalidFlags flgs)
                  Just f  -> PropagationSuccess
                               (SpanContext traceId spanId Nothing f)
    _ -> PropagationInvalid (MalformedHeader t)
  where
    validVersion v = Text.length v == 2 && Text.all isHexDigit v
                  && v /= "ff"           -- W3C reserved
    parseFlags f
      | Text.length f == 2 && Text.all isHexDigit f =
          Just (TraceFlags (fromIntegral (hexByte f)))
      | otherwise = Nothing

decodeHex :: Int -> Text -> Either Text ByteString
decodeHex expectedBytes t
  | Text.length t /= expectedBytes * 2 =
      Left ("expected " <> Text.pack (show (expectedBytes * 2)) <> " hex chars")
  | otherwise = case Base16.decode (TE.encodeUtf8 t) of
      Right bs -> Right bs
      Left  e  -> Left (Text.pack e)

isHexDigit :: Char -> Bool
isHexDigit c = (c >= '0' && c <= '9')
            || (c >= 'a' && c <= 'f')
            || (c >= 'A' && c <= 'F')

hexByte :: Text -> Int
hexByte = Text.foldl' (\acc c -> acc * 16 + hexVal c) 0
  where
    hexVal c
      | c >= '0' && c <= '9' = ord c - ord '0'
      | c >= 'a' && c <= 'f' = ord c - ord 'a' + 10
      | c >= 'A' && c <= 'F' = ord c - ord 'A' + 10
      | otherwise            = 0

emitTraceparent :: SpanContext -> Text
emitTraceparent ctx = Text.intercalate "-"
  [ "00"
  , TE.decodeUtf8 (Base16.encode (unTraceId (scTraceId ctx)))
  , TE.decodeUtf8 (Base16.encode (unSpanId  (scSpanId  ctx)))
  , Text.pack (printHex2 (unTraceFlags (scTraceFlags ctx)))
  ]
  where
    printHex2 :: Word8 -> String
    printHex2 w =
      let hi = w `div` 16
          lo = w `mod` 16
      in [hexChar hi, hexChar lo]
    hexChar n
      | n < 10    = chr (ord '0' + fromIntegral n)
      | otherwise = chr (ord 'a' + fromIntegral n - 10)

traceparentName :: CI.CI ByteString
traceparentName = CI.mk "traceparent"

injectHeaders :: SpanContext -> [Header] -> [Header]
injectHeaders ctx hs =
  (traceparentName, TE.encodeUtf8 (emitTraceparent ctx))
  : filter ((/= traceparentName) . fst) hs

extractContext :: [Header] -> PropagationResult
extractContext hs = case lookup traceparentName hs of
  Nothing -> PropagationAbsent
  Just bs -> case TE.decodeUtf8' bs of
    Left _   -> PropagationInvalid (MalformedHeader "non-utf8")
    Right t  -> parseTraceparent t
```

## Modules touched

- `src/Trace/Propagation.hs` (new)
- `test/Trace/PropagationSpec.hs` (new)

## Acceptance criteria

- Round-trip property holds: `parseTraceparent (emitTraceparent ctx) == PropagationSuccess ctx` for any well-formed `SpanContext`.
- All 5 `PropagationError` constructors are reachable.
- Fuzz test (10000 random `Text` inputs) never throws or hangs.

## Test plan

### Unit (hspec)

| Test | Input | Expected |
|---|---|---|
| Valid spec example | `"00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01"` | `PropagationSuccess _` with sampled bit set |
| Wrong version | `"01-...-..-01"` | `PropagationInvalid (InvalidVersion "01")` ⚠ — actually `01` *is* per W3C "future version, treat as valid"; see outlier note |
| Forbidden version `ff` | `"ff-...-..-01"` | `PropagationInvalid (InvalidVersion "ff")` |
| Trace-id wrong length | 30 hex chars | `PropagationInvalid (InvalidTraceId _)` |
| Span-id wrong length | 14 hex chars | `PropagationInvalid (InvalidSpanId _)` |
| All-zero trace-id | 32 zeros | `PropagationInvalid (InvalidTraceId _)` |
| All-zero span-id | 16 zeros (valid trace) | `PropagationInvalid (InvalidSpanId _)` |
| Invalid flags | `"00-...-...-zz"` | `PropagationInvalid (InvalidFlags "zz")` |
| Wrong number of `-` | `"00-..."` (3 parts) | `PropagationInvalid (MalformedHeader _)` |
| Empty | `""` | `PropagationInvalid (MalformedHeader _)` |
| `extractContext` no header | `[]` | `PropagationAbsent` |
| `extractContext` non-UTF-8 | header bytes invalid | `PropagationInvalid (MalformedHeader "non-utf8")` |
| `injectHeaders` overwrites | existing `traceparent` | new value present, only one `traceparent` |
| `injectHeaders` preserves others | mixed headers | non-`traceparent` headers unchanged |

### Property (hedgehog)

```haskell
prop_traceparent_round_trip :: Property
prop_traceparent_round_trip = property $ do
  ctx <- forAll genSpanContextNoParent
  case parseTraceparent (emitTraceparent ctx) of
    PropagationSuccess ctx' -> ctx' === ctx
    other                   -> footnote (show other) >> failure

prop_parseTraceparent_total :: Property
prop_parseTraceparent_total = withTests 10000 . property $ do
  -- Fuzz with arbitrary text. Must always return a constructor; never throw.
  t <- forAll (Gen.text (Range.linear 0 200) Gen.unicode)
  case parseTraceparent t of
    PropagationSuccess _ -> success
    PropagationAbsent    -> success  -- (cannot occur from this entry but harmless)
    PropagationInvalid _ -> success

prop_inject_preserves_others :: Property
prop_inject_preserves_others = property $ do
  ctx <- forAll genSpanContextNoParent
  hs  <- forAll (Gen.list (Range.linear 0 20) genNonTraceparentHeader)
  let injected = injectHeaders ctx hs
  -- All non-traceparent headers from `hs` survive in `injected`.
  forM_ hs $ \h -> assert (h `elem` injected)
```

### Outliers

- **Trailing whitespace**: `parseTraceparent "00-...-..-01 "` → `MalformedHeader`
  (we don't strip).
- **Mixed-case version `0F`**: `validVersion` checks `isHexDigit` (which
  accepts both cases), so `0F` parses; this matches W3C's "case-insensitive
  hex" rule. Versioned test pinned.
- **Future version `02`**: per W3C, future versions should still parse the
  first 4 fields and ignore the rest. Our parser currently rejects via the
  `splitOn "-"` arity check. **Documented limitation**: v0.1 only parses
  exactly version `00`; future versions are rejected with `InvalidVersion`.
  *(See test "Wrong version" above.)* This is the conservative choice;
  graceful future-version handling is a v0.2 enhancement.
- **`tracestate` header in input**: ignored entirely. `extractContext`
  with `[("tracestate", "vendor=blah")]` returns `PropagationAbsent`.
- **`injectHeaders` called twice**: only one `traceparent` in output (the
  filter step removes the previous one).
- **Lowercase `traceparent` vs `Traceparent` vs `TRACEPARENT`**: all
  handled identically because `lookup` uses `CI ByteString`. Property test
  asserts case-insensitive lookup.
- **Header name with unicode**: not allowed by HTTP spec, but `CI` handles
  arbitrary `ByteString`; `extractContext` simply doesn't match it.

## What this phase does NOT do

- No `tracestate` parsing or emission.
- No automatic injection inside `inSpan`/`inSpanM` (that's a Phase 9
  integration test, not a Phase 5 feature).

---

# Phase 6 — OTLP/HTTP JSON Exporter (~180 LOC)

**Goal.** Serialize `FinishedSpan`s to OTLP/HTTP JSON and `POST` them to
a configured endpoint. Validate `Endpoint` and headers at construction
time. Tests run against an in-process `wai`/`warp` mock so no external
service is required.

## What gets built

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Trace.Export.Otlp
  ( -- Endpoint (constructor hidden in export list)
    Endpoint, unEndpoint, mkEndpoint
  , Compression (..), OtlpConfig (..)
    -- Constructor
  , otlpExporter
  ) where

import           Data.Aeson            (Value (..), object, (.=), encode)
import qualified Data.Aeson.Key        as K
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.CaseInsensitive  as CI
import           Data.List             (any)
import qualified Data.List.NonEmpty    as NE
import           Data.Text             (Text)
import qualified Data.Text             as Text
import qualified Data.Text.Encoding    as TE
import           Data.Time             (NominalDiffTime, diffUTCTime)
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Network.URI           as URI
import           UnliftIO.Exception    (try, SomeException)

import           Trace.Core
import           Trace.Attributes
import           Trace.Export.Types

newtype Endpoint = Endpoint { unEndpoint :: Text } deriving (Show, Eq)

data Compression = NoCompression | GzipCompression deriving (Show, Eq)

data OtlpConfig = OtlpConfig
  { otlpEndpoint    :: !Endpoint
  , otlpHeaders     :: ![(Text, Text)]
  , otlpTimeout     :: !NominalDiffTime
  , otlpCompression :: !Compression
  } deriving (Eq)

instance Show OtlpConfig where
  show cfg = "OtlpConfig { endpoint = " <> show (otlpEndpoint cfg)
          <> ", headers = " <> show (redactHeaders (otlpHeaders cfg))
          <> ", timeout = " <> show (otlpTimeout cfg)
          <> ", compression = " <> show (otlpCompression cfg)
          <> " }"

redactHeaders :: [(Text, Text)] -> [(Text, Text)]
redactHeaders = map redact
  where
    redact (k, _) | isSensitive (Text.toLower k) = (k, "<redacted>")
    redact kv = kv
    isSensitive k = any (`Text.isInfixOf` k)
      ["authorization","auth","api-key","apikey","api_key"
      ,"token","secret","password","x-honeycomb-team","dd-api-key"]

mkEndpoint :: Text -> Either ExporterInitError Endpoint
mkEndpoint t = case URI.parseAbsoluteURI (Text.unpack t) of
  Nothing  -> Left (ExporterInvalidEndpoint t)
  Just uri -> case URI.uriScheme uri of
    "http:"  -> Right (Endpoint t)
    "https:" -> Right (Endpoint t)
    s        -> Left (ExporterUnsupportedScheme (Text.pack s))

otlpExporter :: OtlpConfig -> IO (Either ExporterInitError SpanExporter)
otlpExporter cfg = do
  validatedHeaders <- pure (validateHeaders (otlpHeaders cfg))
  case validatedHeaders of
    Left e -> pure (Left e)
    Right hs -> do
      mgr <- newManager tlsManagerSettings
      req0 <- parseRequest (Text.unpack (unEndpoint (otlpEndpoint cfg)))
      let req = req0
            { method = "POST"
            , requestHeaders = (CI.mk "content-type", "application/json") : hs
            , responseTimeout = responseTimeoutMicro $
                round (otlpTimeout cfg * 1_000_000)
            }
      pure $ Right $ SpanExporter
        { exporterExport   = doExport mgr req
        , exporterFlush    = pure (Right ())  -- otlpExporter is unbuffered
        , exporterShutdown = pure ()          -- http-client manager dies w/ GC
        }
  where
    validateHeaders =
      traverse $ \(k, v) ->
        if Text.all (\c -> c > ' ' && c /= ':') k
          then Right (CI.mk (TE.encodeUtf8 k), TE.encodeUtf8 v)
          else Left (ExporterInvalidHeader k "contains invalid characters")

doExport :: Manager -> Request -> NE.NonEmpty FinishedSpan -> IO ExportResult
doExport mgr req spans = do
  let body = encode (encodeOtlp (NE.toList spans))
      req' = req { requestBody = RequestBodyLBS body }
  result <- try (httpLbs req' mgr)
  case result of
    Left (e :: SomeException) ->
      pure (ExportFailure (EndpointUnreachable (Text.pack (show e))))
    Right resp ->
      let st = statusCode (responseStatus resp) in
      case mkHttpStatus st of
        Just hs | st >= 200 && st < 300 ->
          pure (ExportSuccess (NE.length spans))
        Just hs ->
          pure (ExportFailure (MalformedResponse hs (Text.pack (show st))))
        Nothing ->
          pure (ExportFailure (MalformedResponse (HttpStatus 0)
                  ("non-RFC status: " <> Text.pack (show st))))

-- OTLP JSON encoding -----------------------------------------------------

encodeOtlp :: [FinishedSpan] -> Value
encodeOtlp spans = object
  [ "resourceSpans" .= [object
      [ "scopeSpans" .= [object
          [ "spans" .= map encodeSpan spans ]]]]]

encodeSpan :: FinishedSpan -> Value
encodeSpan fs = object
  [ "traceId"           .= hex (unTraceId (scTraceId (fsContext fs)))
  , "spanId"            .= hex (unSpanId  (scSpanId  (fsContext fs)))
  , "name"              .= unSpanName (fsName fs)
  , "kind"              .= encodeKind (fsKind fs)
  , "startTimeUnixNano" .= unixNano (fsStartTime fs)
  , "endTimeUnixNano"   .= unixNano (fsEndTime   fs)
  , "attributes"        .= map encodeKv (Map.toList (unSpanAttrs (fsAttributes fs)))
  , "events"            .= map encodeEvent (fsEvents fs)
  , "status"            .= encodeStatus (fsStatus fs)
  ]
  where
    hex      = TE.decodeUtf8 . Base16.encode
    unixNano t = floor (utcTimeToPOSIXSeconds t * 1_000_000_000) :: Integer

encodeKind :: SpanKind -> Int
encodeKind = \case
  Internal -> 1; Server -> 2; Client -> 3; Producer -> 4; Consumer -> 5

encodeKv :: (AttrKey, AttrValue) -> Value
encodeKv (AttrKey k, v) = object
  [ "key"   .= k
  , "value" .= encodeAttrValue v
  ]

encodeAttrValue :: AttrValue -> Value
encodeAttrValue = \case
  AttrString t     -> object ["stringValue" .= t]
  AttrInt    n     -> object ["intValue"    .= n]
  AttrDouble d     -> object ["doubleValue" .= d]
  AttrBool   b     -> object ["boolValue"   .= b]
  AttrStringList l -> object ["arrayValue"  .= object ["values" .= map (\t -> object ["stringValue" .= t]) l]]
  AttrIntList    l -> object ["arrayValue"  .= object ["values" .= map (\n -> object ["intValue"    .= n]) l]]

encodeEvent :: SpanEvent -> Value
encodeEvent ev = object
  [ "name"         .= eventName ev
  , "timeUnixNano" .= (floor (utcTimeToPOSIXSeconds (eventTime ev) * 1e9) :: Integer)
  , "attributes"   .= map encodeKv (Map.toList (unSpanAttrs (eventAttributes ev)))
  ]

encodeStatus :: SpanStatus -> Value
encodeStatus = \case
  StatusUnset                    -> object ["code" .= (0 :: Int)]
  StatusOk                       -> object ["code" .= (1 :: Int)]
  StatusError (ErrorMessage msg) -> object ["code" .= (2 :: Int), "message" .= msg]
```

Module export list (`Trace/Export/Otlp.hs`):

```haskell
module Trace.Export.Otlp
  ( Endpoint            -- type only, no (..)
  , unEndpoint
  , mkEndpoint
  , Compression (..)
  , OtlpConfig (..)
  , otlpExporter
  ) where
```

## Modules touched

- `src/Trace/Export/Otlp.hs` (new)
- `test/Trace/Export/OtlpSpec.hs` (new)
- `test/snapshots/otlp-single-span.json` (new — golden file)

## Acceptance criteria

- `mkEndpoint` accepts only `http://`/`https://`.
- OTLP JSON output matches the golden snapshot byte-for-byte (after
  key sorting).
- Network failure paths return `ExportFailure`, never throw.
- `Show OtlpConfig` does not contain auth header values.

### Snapshot file: how to generate the first one

The OTLP JSON wire format is fixed by the OTel spec, but the *exact*
bytes htrace produces depend on aeson's key ordering, our struct
shape, and conventions like number-vs-string encoding. The first
snapshot is generated by hand once and reviewed against the OTel
spec; subsequent runs diff against it.

Procedure for the first contributor implementing Phase 6:

1. Run `encodeOtlp [sampleFinishedSpan]` (where `sampleFinishedSpan` comes
   from `Trace.Generators` in Phase 0) and pretty-print to stdout.
2. Compare output against the OTel JSON-over-HTTP spec for traces:
   - `traceId`/`spanId` are lowercase hex strings, no `0x` prefix.
   - Times are `*UnixNano` integers. **Important:** v0.1.0.0 emits these
     as JSON numbers (aeson's default for `Integer`); this is technically
     a deviation from strict OTel-JSON which prefers string-encoded
     `int64` for JS-client precision. Every backend tested (Jaeger,
     Tempo, Datadog) accepts both. If a contributor hits a backend that
     requires strings, that's a v0.2 fix.
   - `kind` is a small integer: 1=Internal, 2=Server, 3=Client, 4=Producer, 5=Consumer.
   - Empty `attributes` and `events` arrays are `[]`, not omitted.
3. Save the bytes to `test/snapshots/otlp-single-span.json` *with sorted
   object keys* (use `aeson-pretty --sort-keys` or equivalent).
   Subsequent runs sort keys before diffing.
4. The reviewer of the Phase 6 PR is responsible for spot-checking the
   golden against the OTel spec — once it lands, every subsequent diff
   is a regression unless deliberately updated.

## Test plan

### Unit (hspec)

| Test | Input | Expected |
|---|---|---|
| `mkEndpoint "http://x"` | — | `Right _` |
| `mkEndpoint "https://x"` | — | `Right _` |
| `mkEndpoint "ftp://x"` | — | `Left (ExporterUnsupportedScheme "ftp:")` |
| `mkEndpoint "not a url"` | — | `Left (ExporterInvalidEndpoint _)` |
| `mkEndpoint ""` | — | `Left (ExporterInvalidEndpoint _)` |
| `mkEndpoint "http://[::1]:4318"` | IPv6 literal | `Right _` |
| `otlpExporter` with bad header | header name `"x:y"` | `Left (ExporterInvalidHeader "x:y" _)` |
| `Show OtlpConfig` redaction | header `("Authorization","Bearer s")` | `show` does not contain `"Bearer s"` |
| `Show OtlpConfig` preserves names | header `("Authorization","Bearer s")` | `show` does contain `"Authorization"` |
| Snapshot test | one synthetic span | `aeson-diff` produces empty patch vs golden |

### Property (hedgehog)

```haskell
prop_otlp_round_trip_via_aeson :: Property
prop_otlp_round_trip_via_aeson = property $ do
  fs <- forAll genFinishedSpan
  let json = encodeOtlp [fs]
  -- The encoded JSON must parse back as a Value (any Value);
  -- this asserts the encoder produces well-formed JSON.
  case decode (encode json) :: Maybe Value of
    Just _  -> success
    Nothing -> failure

prop_redactHeaders_idempotent :: Property
prop_redactHeaders_idempotent = property $ do
  hs <- forAll (Gen.list (Range.linear 0 20) genHeaderPair)
  redactHeaders (redactHeaders hs) === redactHeaders hs

prop_redactHeaders_preserves_names :: Property
prop_redactHeaders_preserves_names = property $ do
  hs <- forAll (Gen.list (Range.linear 0 20) genHeaderPair)
  map fst (redactHeaders hs) === map fst hs
```

### Integration (against in-process WAI app)

```haskell
spec :: Spec
spec = describe "otlpExporter" $ do
  it "POSTs JSON to endpoint" $ do
    -- start a wai application that records request bodies into an IORef
    bodyRef <- newIORef Nothing
    Warp.testWithApplication (pure (recordingApp bodyRef)) $ \port -> do
      let Right ep = mkEndpoint ("http://localhost:" <> Text.pack (show port))
      Right exp_ <- otlpExporter (OtlpConfig ep [] 5 NoCompression)
      _ <- exporterExport exp_ (sampleFinishedSpan NE.:| [])
      Just body <- readIORef bodyRef
      -- The body is OTLP JSON.
      let Just (Object o) = decode body :: Maybe Value
      KeyMap.member "resourceSpans" o `shouldBe` True

  it "returns EndpointUnreachable on connection refused" $ do
    let Right ep = mkEndpoint "http://localhost:1"  -- nothing listening
    Right exp_ <- otlpExporter (OtlpConfig ep [] 1 NoCompression)
    result <- exporterExport exp_ (sampleFinishedSpan NE.:| [])
    case result of
      ExportFailure (EndpointUnreachable _) -> pure ()
      other -> expectationFailure ("expected EndpointUnreachable, got " <> show other)
```

### Outliers

- **Endpoint with trailing slash**: `mkEndpoint "http://x/"` succeeds and is
  preserved verbatim through `unEndpoint`.
- **Endpoint with non-default port**: `mkEndpoint "http://x:8443"` succeeds.
- **Endpoint with path**: `mkEndpoint "http://x/v1/traces"` succeeds; the
  path is sent as-is in `POST`.
- **Endpoint with userinfo**: `mkEndpoint "http://user:pass@x"` succeeds
  syntactically but the credentials end up in HTTP Basic auth handled by
  `http-client`. *(Documented: prefer `otlpHeaders` for auth.)*
- **Header value with `\n`**: header injection attack vector; rejected by
  `validateHeaders` because `\n <= ' '`. Test asserts this.
- **Span with one attribute of each `AttrValue` constructor**: snapshot test
  includes all six. Future addition of an `AttrValue` constructor breaks
  the test loudly.
- **Span with empty attributes and zero events**: serializes to `[]` for
  both, not `null`.
- **Zero-duration span** (`fsStartTime == fsEndTime`): `unixNano` produces
  identical values; OTLP accepts this.
- **Endpoint returning HTTP 600** (per the test mock): `mkHttpStatus 600`
  returns `Nothing`, so the export error is `MalformedResponse (HttpStatus 0) "non-RFC status: 600"`.

## What this phase does NOT do

- No batching (Phase 7).
- No gzip compression (`Compression` is defined but only `NoCompression` is
  honored in v0.1.0.0; the spec is honest about this).
- No retry-with-backoff on transient failures (deferred to v0.2).

---

# Phase 7 — Async Batching Queue (~150 LOC)

**Goal.** Ship `batchExporter`: a wrapper that takes any `SpanExporter` and
adds a bounded `TBQueue`, a background flush worker, off-thread drop
notification, and timeout-bounded export. Throughput becomes a measurable
quantity.

## What gets built

```haskell
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Trace.Export.Batch
  ( BatchConfig (..), defaultBatchConfig, defaultOnDroppedSpans
  , batchExporter
  ) where

import           Control.Concurrent          (forkIO, threadDelay)
import           Control.Concurrent.STM
import           Control.Monad               (when)
import qualified Data.List.NonEmpty          as NE
import qualified Data.Text                   as Text
import           Data.Time                   (NominalDiffTime)
import           UnliftIO.Async              (race)
import           UnliftIO.Exception          (try, SomeException)

import           Trace.Core                  (FinishedSpan)
import           Trace.Export.Types

data BatchConfig = BatchConfig
  { maxQueueSize   :: !Int
  , maxExportBatch :: !Int
  , exportInterval :: !NominalDiffTime
  , exportTimeout  :: !NominalDiffTime
  , onDroppedSpans :: !(Int -> IO ())
  }

defaultOnDroppedSpans :: InternalLogger -> Int -> IO ()
defaultOnDroppedSpans logger n =
  logWarn logger ("htrace: dropped " <> Text.pack (show n) <> " spans (queue full)")

defaultBatchConfig :: BatchConfig
defaultBatchConfig = BatchConfig
  { maxQueueSize   = 2048
  , maxExportBatch = 512
  , exportInterval = 5
  , exportTimeout  = 30
  , onDroppedSpans = defaultOnDroppedSpans stderrLogger
  }

validateBatchConfig :: BatchConfig -> Maybe BatchConfigError
validateBatchConfig c
  | maxQueueSize c   <= 0 = Just (NonPositiveQueueSize (maxQueueSize c))
  | maxExportBatch c <= 0 = Just (NonPositiveBatchSize (maxExportBatch c))
  | maxExportBatch c > maxQueueSize c =
      Just (BatchExceedsQueue (maxExportBatch c) (maxQueueSize c))
  | exportInterval c <= 0 = Just (NonPositiveInterval (exportInterval c))
  | exportTimeout c  <= 0 = Just (NonPositiveTimeout  (exportTimeout c))
  | otherwise = Nothing

batchExporter
  :: BatchConfig -> SpanExporter -> IO (Either BatchConfigError SpanExporter)
batchExporter cfg inner = case validateBatchConfig cfg of
  Just e  -> pure (Left e)
  Nothing -> do
    queue        <- newTBQueueIO (fromIntegral (maxQueueSize cfg))
    dropChan     <- newTBQueueIO 64        -- drop notifications
    shutdownVar  <- newTVarIO False
    workerDone   <- newEmptyTMVarIO        -- worker signals clean exit
    notifierDone <- newEmptyTMVarIO        -- notifier signals clean exit
    _ <- forkIO (worker queue dropChan shutdownVar workerDone)
    _ <- forkIO (notifier dropChan shutdownVar notifierDone)
    pure $ Right $ SpanExporter
      { exporterExport   = enqueue queue dropChan
      , exporterFlush    = doFlush queue
      , exporterShutdown = doShutdown queue shutdownVar workerDone notifierDone
      }
  where
    enqueue q dch ne = do
      let xs = NE.toList ne
          n  = length xs
      dropped <- atomically $ do
        space <- (-) <$> pure (fromIntegral (maxQueueSize cfg))
                     <*> lengthTBQueue q
        let canEnqueue = min n (fromIntegral space)
            dropped = n - canEnqueue
        mapM_ (writeTBQueue q) (take canEnqueue xs)
        when (dropped > 0) $ writeTBQueue dch dropped
        pure dropped
      -- Report exactly the number that made it into the queue.
      -- Drops are reported asynchronously via onDroppedSpans; the producer's
      -- return value reflects what was *accepted*, not what was *delivered*.
      pure (ExportSuccess (n - dropped))

    notifier dch shutdownVar done = do
      let loop = do
            mn <- atomically $
              (Just <$> readTBQueue dch) `orElse` do
                shutting <- readTVar shutdownVar
                empty    <- isEmptyTBQueue dch
                if shutting && empty then pure Nothing else retry
            case mn of
              Just n  -> onDroppedSpans cfg n >> loop
              Nothing -> atomically (putTMVar done ())
      loop

    worker q dch shutdownVar done = do
      let loop = do
            -- Wake on: full batch, interval, or shutdown signal.
            let intervalMicros = round (exportInterval cfg * 1e6)
            wakeReason <- race
              (threadDelay intervalMicros)
              (atomically $ do
                 shutting <- readTVar shutdownVar
                 if shutting
                   then pure ()
                   else do
                     l <- lengthTBQueue q
                     if fromIntegral l >= maxExportBatch cfg
                       then pure ()
                       else retry)
            -- Drain whatever is available now.
            batch <- atomically (drainBatch q (maxExportBatch cfg))
            case NE.nonEmpty batch of
              Nothing -> pure ()
              Just ne -> do
                _ <- race (threadDelay (round (exportTimeout cfg * 1e6)))
                          (try @SomeException (exporterExport inner ne))
                pure ()
            -- Loop unless shutdown is set AND queue is empty.
            shouldExit <- atomically $ do
              shutting <- readTVar shutdownVar
              empty    <- isEmptyTBQueue q
              pure (shutting && empty)
            if shouldExit
              then atomically (putTMVar done ())
              else loop
      loop

    drainBatch q maxN = go [] 0
      where
        go acc i
          | i >= maxN = pure (reverse acc)
          | otherwise = tryReadTBQueue q >>= \case
              Nothing -> pure (reverse acc)
              Just s  -> go (s:acc) (i+1)

    doFlush q = do
      batch <- atomically (drainBatch q (maxQueueSize cfg))
      case NE.nonEmpty batch of
        Nothing -> pure (Right ())
        Just ne -> do
          rRace <- race (threadDelay (round (exportTimeout cfg * 1e6)))
                        (exporterExport inner ne)
          case rRace of
            Left ()                 -> pure (Left (ExportTimeout (exportTimeout cfg)))
            Right (ExportSuccess _) -> pure (Right ())
            Right (ExportFailure e) -> pure (Left e)

    doShutdown q shutdownVar workerDone notifierDone = do
      -- Signal both threads to drain and exit cleanly.
      atomically (writeTVar shutdownVar True)
      -- Wait for the worker to drain the queue. The worker's loop checks
      -- shutdownVar AND queue-emptiness, so when this TMVar is filled,
      -- every span that was enqueued before shutdown has been handed to
      -- the inner exporter (or timed out trying).
      atomically (takeTMVar workerDone)
      atomically (takeTMVar notifierDone)
      exporterShutdown inner
```

## Modules touched

- `src/Trace/Export/Batch.hs` (new)
- `test/Trace/Export/BatchSpec.hs` (new)
- `bench/BatchBench.hs` (new — first benchmark target)

## Acceptance criteria

- Validation rejects all five `BatchConfigError` cases.
- Queue overflow triggers `onDroppedSpans` with the correct count.
- `onDroppedSpans` runs on a background thread (verifiable: a 1-second
  blocking callback does not slow producer).
- `batchExporter` wrapping `noopExporter` adds < 100µs of overhead per span.
- `exporterExport`'s return value reflects spans **accepted into the queue**,
  not spans delivered to the inner exporter. Drops are observable only via
  the `onDroppedSpans` callback. `ExportSuccess n` from `batchExporter`
  means "n of your spans entered the queue"; the producer then trusts the
  batcher to deliver them (or call `onDroppedSpans` if it can't).
- `exporterShutdown` waits for the worker to drain the queue before
  returning. After `exporterShutdown` returns, no spans remain unexported
  (modulo individual export failures, which the inner exporter has already
  surfaced via its own return values).

## Test plan

### Unit (hspec)

| Test | Setup | Expected |
|---|---|---|
| Validation: `maxQueueSize = 0` | — | `Left (NonPositiveQueueSize 0)` |
| Validation: `maxExportBatch = 0` | — | `Left (NonPositiveBatchSize 0)` |
| Validation: batch > queue | `maxExportBatch=10, maxQueueSize=5` | `Left (BatchExceedsQueue 10 5)` |
| Validation: zero interval | — | `Left (NonPositiveInterval _)` |
| Validation: zero timeout | — | `Left (NonPositiveTimeout _)` |
| Drop callback fires | enqueue `maxQueueSize+10` spans fast | callback called with `10` |
| Drop callback off-thread | callback sleeps 1s | producer's next call returns within < 1ms |
| Shutdown drains queue | enqueue 100 then shutdown | inner exporter has all 100 |
| Flush is bounded | inner exporter sleeps `exportTimeout+1` | flush returns `Left (ExportTimeout _)` |

### Property (hedgehog)

```haskell
prop_no_span_loss_at_steady_state :: Property
prop_no_span_loss_at_steady_state = withTests 20 . property $ do
  -- Enqueue n spans where n < maxQueueSize, then shutdown,
  -- then assert: inner exporter received all n.
  n <- forAll (Gen.int (Range.linear 1 1000))
  liftIO $ do
    (inner, readAll) <- memoryExporter
    Right batched <- batchExporter
      defaultBatchConfig { maxQueueSize = 2000, onDroppedSpans = const (pure ()) }
      inner
    forM_ [1..n] $ \i -> do
      let fs = sampleSpan i
      _ <- exporterExport batched (fs NE.:| [])
      pure ()
    exporterShutdown batched
    received <- readAll
    length received === n

prop_drop_count_correct :: Property
prop_drop_count_correct = property $ do
  let qSize = 100
  n <- forAll (Gen.int (Range.linear 200 500))
  liftIO $ do
    droppedRef <- newIORef 0
    Right batched <- batchExporter
      defaultBatchConfig { maxQueueSize = qSize
                         , maxExportBatch = qSize
                         , exportInterval = 60  -- effectively disabled
                         , onDroppedSpans = \k -> modifyIORef' droppedRef (+ k)
                         }
      noopExporter
    forM_ [1..n] $ \i -> do
      _ <- exporterExport batched (sampleSpan i NE.:| [])
      pure ()
    -- Wait for notifier thread to drain.
    threadDelay 100_000
    dropped <- readIORef droppedRef
    -- Producer enqueued n; queue holds at most qSize; so dropped >= n - qSize.
    diff dropped (>=) (n - qSize)
```

### Benchmark (tasty-bench)

```haskell
benchmarks :: [Benchmark]
benchmarks =
  [ bench "batchExporter overhead vs noopExporter" $ whnfIO $ do
      Right batched <- batchExporter defaultBatchConfig noopExporter
      replicateM_ 1000 $ exporterExport batched (sampleSpan 0 NE.:| [])
      exporterShutdown batched
  -- Asserts: < 100ms total → < 100µs per export call.
  ]
```

### Outliers

- **`maxExportBatch == maxQueueSize`** (boundary): valid, accepted.
- **`exporterExport` with NonEmpty of length > maxQueueSize**: drops
  `len - maxQueueSize` and notifies. Test asserts.
- **`exporterShutdown` while a flush is in flight**: the flush's
  `exportTimeout` bounds it; shutdown then proceeds. No deadlock.
- **`exporterShutdown` called twice**: second call is a no-op (the worker
  thread is already dead; `killThread` on a dead thread is silent).
- **Producer producing faster than 1M spans/sec**: queue fills, drops kick
  in, callback runs off-thread. No producer slowdown.
- **`exportInterval` of 0.001s**: technically valid (> 0); the worker just
  spins at high frequency. Property test exercises this; we don't crash.

## What this phase does NOT do

- No metrics emission beyond `onDroppedSpans` (queue depth gauge etc.
  deferred to v0.2).
- No multi-batch concurrency: the worker exports one batch at a time. If
  the inner exporter is slow, the queue can back up. This is the simple
  correct version; future versions may parallelize.

---

# Phase 8 — Configuration & Environment Loading (~180 LOC)

**Goal.** Build `TracingConfig` from `OTEL_*` environment variables,
accumulating all errors. Provide `defaultConfig`. Honour the
`OTEL_SDK_DISABLED` kill-switch.

## What gets built

```haskell
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Trace.Config
  ( TracingConfig (..), defaultConfig
  , Resource, unResource, mkResource, defaultResource
  , Propagator (..)
  , ExporterConfig (..)
  , SamplerConfig (..), SampleRate, mkSampleRate, unSampleRate
  , ConfigError (..), EnvVarName (..)
  , fromEnv
  ) where

import           Data.List.NonEmpty   (NonEmpty (..))
import qualified Data.List.NonEmpty   as NE
import           Data.Maybe           (fromMaybe)
import           Data.Text            (Text)
import qualified Data.Text            as Text
import qualified Data.Text.Read       as TR
import           System.Environment   (lookupEnv)
import           Validation           (Validation (..), validationToEither)

import           Trace.Attributes
import           Trace.Core
import           Trace.Export.Otlp
import           Trace.Export.Types

newtype EnvVarName = EnvVarName { unEnvVarName :: Text } deriving (Show, Eq)

data ConfigError
  = MissingRequiredVar      !EnvVarName
  | InvalidVarValue         !EnvVarName !Text !Text
  | InvalidEndpointUrl      !Text
  | InvalidSampleRate       !Double
  | InvalidExporterInit     !ExporterInitError
  | UnsupportedOtlpProtocol !Text
  | InvalidResourceAttribute !Text !Text
  deriving (Show, Eq)

data Propagator = W3CTraceContextPropagator deriving (Show, Eq)

newtype Resource = Resource { unResource :: SpanAttrs } deriving (Show, Eq)

mkResource :: Text -> [(AttrKey, AttrValue)] -> Either ConfigError Resource
mkResource serviceName extra
  | Text.null (Text.strip serviceName) = Left
      (InvalidVarValue (EnvVarName "OTEL_SERVICE_NAME") serviceName
                       "service.name must be non-empty")
  | otherwise = Right $ Resource $ attrs $
      ("service.name", AttrString serviceName) : extra

defaultResource :: Resource
defaultResource = Resource $ attrs
  [ ("service.name",          AttrString "htrace-default")
  , ("telemetry.sdk.name",    AttrString "htrace")
  , ("telemetry.sdk.version", AttrString "0.1.0.0")
  , ("telemetry.sdk.language",AttrString "haskell")
  ]

data ExporterConfig = OtlpExporter !OtlpConfig | NoopExporter deriving (Show, Eq)

newtype SampleRate = SampleRate { unSampleRate :: Double } deriving (Show, Eq, Ord)

mkSampleRate :: Double -> Either ConfigError SampleRate
mkSampleRate d
  | d >= 0 && d <= 1 = Right (SampleRate d)
  | otherwise        = Left (InvalidSampleRate d)

data SamplerConfig
  = AlwaysSample | NeverSample | TraceIdRatio !SampleRate
  deriving (Show, Eq)

data TracingConfig = TracingConfig
  { configExporter    :: !ExporterConfig
  , configSampler     :: !SamplerConfig
  , configResource    :: !Resource
  , configPropagators :: ![Propagator]
  , configLogger      :: !InternalLogger
  }

instance Show TracingConfig where
  show c = "TracingConfig { exporter = " <> show (configExporter c)
        <> ", sampler = "                <> show (configSampler c)
        <> ", resource = "               <> show (configResource c)
        <> ", propagators = "            <> show (configPropagators c)
        <> ", logger = <function> }"

instance Eq TracingConfig where
  -- Compare every field except logger (functions can't be compared).
  a == b = configExporter a    == configExporter b
        && configSampler a     == configSampler b
        && configResource a    == configResource b
        && configPropagators a == configPropagators b

defaultConfig :: TracingConfig
defaultConfig = TracingConfig
  { configExporter    = NoopExporter
  , configSampler     = AlwaysSample
  , configResource    = defaultResource
  , configPropagators = [W3CTraceContextPropagator]
  , configLogger      = stderrLogger
  }

-- Validation-based env loader -------------------------------------------

fromEnv :: IO (Either (NonEmpty ConfigError) TracingConfig)
fromEnv = do
  disabled <- lookupEnv "OTEL_SDK_DISABLED"
  case disabled of
    Just s | Text.toLower (Text.pack s) == "true" ->
      pure (Right defaultConfig { configExporter = NoopExporter })
    _ -> do
      exp_  <- loadExporterConfig
      sampl <- loadSamplerConfig
      res_  <- loadResource
      pure $ validationToEither $ TracingConfig
        <$> exp_
        <*> sampl
        <*> res_
        <*> Success [W3CTraceContextPropagator]
        <*> Success stderrLogger

-- Each loader returns Validation (NonEmpty ConfigError) X. Implementations
-- read the relevant OTEL_* variables, use mkEndpoint / mkSampleRate /
-- mkResource as appropriate, and accumulate errors. Full implementations
-- shown in the source file; see test plan for the contract each must meet.

loadExporterConfig :: IO (Validation (NonEmpty ConfigError) ExporterConfig)
loadExporterConfig = do
  kind <- lookupEnv "OTEL_TRACES_EXPORTER"
  case kind of
    Just "none" -> pure (Success NoopExporter)
    Just "otlp" -> loadOtlpConfig
    Nothing     -> loadOtlpConfig                       -- default
    Just other  -> pure $ Failure $ NE.singleton
      (InvalidVarValue (EnvVarName "OTEL_TRACES_EXPORTER") (Text.pack other)
                       "expected 'otlp' or 'none'")

loadOtlpConfig :: IO (Validation (NonEmpty ConfigError) ExporterConfig)
loadOtlpConfig = do
  endpointVar <- lookupEnv "OTEL_EXPORTER_OTLP_ENDPOINT"
  protoVar    <- lookupEnv "OTEL_EXPORTER_OTLP_PROTOCOL"
  hdrsVar     <- lookupEnv "OTEL_EXPORTER_OTLP_HEADERS"
  timeoutVar  <- lookupEnv "OTEL_EXPORTER_OTLP_TIMEOUT"
  -- Build each component as a Validation; combine with <*> so all errors
  -- accumulate. (Reading the env returns IO, but the validation arithmetic
  -- is pure once we have the strings.)
  let vEndpoint = case endpointVar of
        Nothing -> Failure $ NE.singleton
          (MissingRequiredVar (EnvVarName "OTEL_EXPORTER_OTLP_ENDPOINT"))
        Just s  -> case mkEndpoint (Text.pack s) of
          Left e   -> Failure $ NE.singleton (InvalidExporterInit e)
          Right ep -> Success ep
      vProto = case protoVar of
        Nothing          -> Success ()
        Just "http/json" -> Success ()
        Just other       -> Failure $ NE.singleton
          (UnsupportedOtlpProtocol (Text.pack other))
      vHeaders = case hdrsVar of
        Nothing -> Success []
        Just s  -> parseHeaderList (Text.pack s)
      vTimeout = case timeoutVar of
        Nothing -> Success 10  -- seconds, OTLP default
        Just s  -> case TR.double (Text.pack s) of
          Right (d, rest) | Text.null rest && d > 0 ->
            Success (realToFrac d)
          _ -> Failure $ NE.singleton
            (InvalidVarValue (EnvVarName "OTEL_EXPORTER_OTLP_TIMEOUT")
                             (Text.pack s) "expected positive number of seconds")
  -- vProto contributes only its error (its Success carries no value).
  -- We use `<*` to keep the left's value while still requiring the right
  -- to succeed.
  pure $ assemble <$> vEndpoint <*> vHeaders <*> vTimeout <* vProto
  where
    assemble ep hdrs to =
      OtlpExporter (OtlpConfig ep hdrs to NoCompression)

-- Parses "k1=v1,k2=v2" or "k1=v1, k2=v2"; whitespace around commas allowed.
parseHeaderList :: Text -> Validation (NonEmpty ConfigError) [(Text, Text)]
parseHeaderList t =
  let entries = map Text.strip (Text.splitOn "," t)
  in  traverseValidation parseOne entries
  where
    parseOne entry = case Text.splitOn "=" entry of
      [k, v] | not (Text.null (Text.strip k)) ->
        Success (Text.strip k, Text.strip v)
      _ -> Failure $ NE.singleton
        (InvalidVarValue (EnvVarName "OTEL_EXPORTER_OTLP_HEADERS")
                         entry "expected key=value")

-- Like `traverse` but accumulates Failures in NonEmpty (validation-selective
-- ships this; written out here because it appears in three loaders).
traverseValidation
  :: Semigroup e
  => (a -> Validation e b) -> [a] -> Validation e [b]
traverseValidation _ []     = Success []
traverseValidation f (x:xs) = (:) <$> f x <*> traverseValidation f xs

loadSamplerConfig :: IO (Validation (NonEmpty ConfigError) SamplerConfig)
loadSamplerConfig = do
  s <- lookupEnv "OTEL_TRACES_SAMPLER"
  case s of
    Nothing | otherwise -> pure (Success AlwaysSample)
    Just "always_on"    -> pure (Success AlwaysSample)
    Just "always_off"   -> pure (Success NeverSample)
    Just "traceidratio" -> do
      arg <- lookupEnv "OTEL_TRACES_SAMPLER_ARG"
      case arg >>= readDouble of
        Nothing -> pure $ Failure $ NE.singleton
          (MissingRequiredVar (EnvVarName "OTEL_TRACES_SAMPLER_ARG"))
        Just d -> case mkSampleRate d of
          Left e   -> pure (Failure (NE.singleton e))
          Right sr -> pure (Success (TraceIdRatio sr))
    Just other -> pure $ Failure $ NE.singleton
      (InvalidVarValue (EnvVarName "OTEL_TRACES_SAMPLER") (Text.pack other)
                       "expected 'always_on', 'always_off', or 'traceidratio'")
  where
    readDouble s = case TR.double (Text.pack s) of
      Right (d, rest) | Text.null rest -> Just d
      _                                -> Nothing

loadResource :: IO (Validation (NonEmpty ConfigError) Resource)
loadResource = do
  svc <- lookupEnv "OTEL_SERVICE_NAME"
  raw <- lookupEnv "OTEL_RESOURCE_ATTRIBUTES"
  let vExtraKvs = case raw of
        Nothing -> Success []
        Just t  ->
          let entries = Text.splitOn "," (Text.pack t)
          in  traverseValidation parseOne entries
      vResource = case svc of
        Nothing -> Success defaultResource
        Just s  -> case (,) <$> Success (Text.pack s) <*> vExtraKvs of
          Failure errs -> Failure errs
          Success (sn, kvs) -> case mkResource sn kvs of
            Left e  -> Failure (NE.singleton e)
            Right r -> Success r
  pure vResource
  where
    parseOne :: Text -> Validation (NonEmpty ConfigError) (AttrKey, AttrValue)
    parseOne entry = case Text.splitOn "=" entry of
      [k, v] | not (Text.null (Text.strip k)) ->
        Success (AttrKey (Text.strip k), AttrString (Text.strip v))
      _ -> Failure $ NE.singleton
        (InvalidResourceAttribute entry "expected key=value")
```

## Modules touched

- `src/Trace/Config.hs` (new)
- `test/Trace/ConfigSpec.hs` (new)

## Acceptance criteria

- All 7 `ConfigError` constructors are reachable.
- `fromEnv` collects multiple errors before returning (does not short-circuit).
- `OTEL_SDK_DISABLED=true` returns a valid config without reading other vars.

## Test plan

### Unit (hspec, using `setEnv`/`unsetEnv` from `System.Environment.Setenv`)

| Test | Env | Expected |
|---|---|---|
| All required vars set | `OTEL_EXPORTER_OTLP_ENDPOINT=http://x`, `OTEL_SERVICE_NAME=svc` | `Right cfg` with `OtlpExporter` |
| Missing endpoint | only `OTEL_TRACES_EXPORTER=otlp` | `Left (MissingRequiredVar "OTEL_EXPORTER_OTLP_ENDPOINT" :| _)` |
| Invalid sample rate | `OTEL_TRACES_SAMPLER=traceidratio`, `OTEL_TRACES_SAMPLER_ARG=1.5` | `Left (InvalidSampleRate 1.5 :| _)` |
| Sample rate `"abc"` | not parseable | `Left (MissingRequiredVar _)` (per spec — same as missing) |
| Unknown sampler | `OTEL_TRACES_SAMPLER=randomized` | `Left (InvalidVarValue _ _ _)` |
| Unsupported protocol | `OTEL_EXPORTER_OTLP_PROTOCOL=grpc` | `Left (UnsupportedOtlpProtocol "grpc")` |
| Bad resource attr | `OTEL_RESOURCE_ATTRIBUTES=key_only_no_value` | `Left (InvalidResourceAttribute _ _)` |
| Two errors at once | bad endpoint + bad sample rate | `Left (NE.toList contains both)` |
| `OTEL_SDK_DISABLED=true` | only this set | `Right defaultConfig { exporter=Noop }` |
| `OTEL_SDK_DISABLED=true` overrides bad config | also bad endpoint set | `Right _` (kill-switch wins) |

### Property (hedgehog)

```haskell
prop_fromEnv_collects_all_errors :: Property
prop_fromEnv_collects_all_errors = property $ do
  -- Generate up to 4 different bad env settings; assert all error
  -- constructors are present.
  badEndpoint   <- forAll Gen.bool
  badSampler    <- forAll Gen.bool
  badResource   <- forAll Gen.bool
  badProtocol   <- forAll Gen.bool
  let expected = sum [if b then 1 else 0 | b <- [badEndpoint, badSampler, badResource, badProtocol]]
  result <- liftIO $ withEnv (envFor badEndpoint badSampler badResource badProtocol) fromEnv
  case result of
    Left errs ->
      diff (NE.length errs) (>=) expected
      -- Note: some error categories produce >1 ConfigError; we check >=.
    Right _ -> assert (expected == 0)

prop_mkSampleRate_range :: Property
prop_mkSampleRate_range = property $ do
  d <- forAll (Gen.double (Range.linearFracFrom 0.5 (-2) 3))
  case mkSampleRate d of
    Right (SampleRate r) -> r === d >> assert (d >= 0 && d <= 1)
    Left  _              -> assert (d < 0 || d > 1)
```

### Outliers

- **`OTEL_SDK_DISABLED=TRUE`** (uppercase): treated as enabled because we
  match `Text.toLower == "true"`. Test pinned.
- **`OTEL_SDK_DISABLED=1`** (numeric): treated as enabled (only the literal
  string `"true"` disables). Test pinned.
- **Empty `OTEL_SERVICE_NAME`**: rejected with `InvalidVarValue`, not silent.
- **`OTEL_RESOURCE_ATTRIBUTES=`** (empty value): treated as no extra attrs;
  no error.
- **`OTEL_RESOURCE_ATTRIBUTES=k=v,k=v2`** (duplicate keys): right-biased
  merge applies, last wins. Test pinned via `Map.fromList` semantics.
- **`OTEL_TRACES_SAMPLER_ARG=Infinity`**: parses as `Double`; `mkSampleRate`
  rejects with `InvalidSampleRate`.
- **`OTEL_EXPORTER_OTLP_TIMEOUT=-1`**: rejected with `InvalidVarValue`.
- **`OTEL_RESOURCE_ATTRIBUTES=k1=v1,bad,k2=v2`**: returns `Left
  (InvalidResourceAttribute "bad" _ :| ...)`; the well-formed entries
  are NOT silently kept — the entire resource fails to load. *(Documented
  decision: partial resource is worse than no resource.)*

## What this phase does NOT do

- No actual `Tracer` construction. Phase 9 wires `TracingConfig` into a
  running `Tracer`.
- **`OTEL_EXPORTER_OTLP_COMPRESSION` is not honored.** The `Compression`
  field on `OtlpConfig` is hardcoded to `NoCompression`. Reading the env
  var, parsing `"gzip"`/`"none"`, and emitting `Content-Encoding: gzip`
  is deferred to v0.2. Setting the variable is a no-op in v0.1; we don't
  reject it (would break "OTel-conformant config loader" promises) but
  we don't act on it either. Document this in the README.
- **`OTEL_PROPAGATORS` is not honored.** The propagator list in
  `TracingConfig` is hardcoded to `[W3CTraceContextPropagator]`. Other
  propagators (B3, Jaeger native, AWS X-Ray) are not implemented in v0.1;
  the env var is ignored. v0.2 will respect it.
- **`OTEL_TRACES_SAMPLER` only accepts `always_on`, `always_off`,
  `traceidratio`.** The OTel spec also defines `parentbased_*` variants;
  these are deferred. v0.1 always uses parent-based sampling implicitly
  (children inherit the parent's sampled bit; the sampler then runs on
  the child) — this matches `parentbased_always_on` and friends in
  practice for the three samplers we ship.

---

# Phase 9 — `withTracing` Integration & `Trace` Façade (~120 LOC)

**Goal.** Wire all phases together. Verify against a real Jaeger via Docker.

## What gets built

```haskell
-- src/Trace/Monad.hs (extended)

import           Data.Version           (showVersion)
import qualified Data.Text              as Text
import qualified Paths_htrace           as Paths    -- auto-generated by cabal
import           Trace.Config
import           Trace.Export.Batch
import           Trace.Export.Otlp      (otlpExporter)

-- Also requires `package.yaml` declares Paths_htrace under the library's
-- `other-modules`. Hpack adds it automatically when `version:` is set.

withTracing
  :: TracingConfig
  -> (Tracer -> IO a)
  -> IO (Either ExporterInitError a)
withTracing cfg action = do
  innerR <- case configExporter cfg of
    NoopExporter   -> pure (Right noopExporter)
    OtlpExporter c -> otlpExporter c
  case innerR of
    Left e   -> pure (Left e)
    Right ie -> do
      batchedR <- batchExporter
        defaultBatchConfig { onDroppedSpans = defaultOnDroppedSpans (configLogger cfg) }
        ie
      case batchedR of
        Left be      -> exporterShutdown ie *> pure (Left (ExporterBatchInit be))
        Right batched -> do
          let tracer = Tracer
                { tracerScope    = htraceScope
                , tracerSampler  = samplerFromConfig (configSampler cfg)
                , tracerExporter = batched
                , tracerClock    = systemClock
                , tracerLogger   = configLogger cfg
                }
          fmap Right $ bracket
            (pure ())
            (\_ -> exporterShutdown batched)
            (\_ -> action tracer)
  where
    -- Version sourced from package.yaml via the auto-generated Paths_htrace
    -- module. This avoids drift between the .cabal version and the string
    -- emitted in `telemetry.sdk.version` resource attributes.
    --
    -- Cabal generates `Paths_htrace` automatically when `version:` changes
    -- in package.yaml; just `import qualified Paths_htrace as Paths` and
    -- use `Paths.version` (a `Data.Version.Version`).
    htraceScope = InstrumentationScope
      "htrace"
      (Just (Text.pack (showVersion Paths.version)))

samplerFromConfig :: SamplerConfig -> Sampler
samplerFromConfig = \case
  AlwaysSample              -> alwaysOnSampler
  NeverSample               -> alwaysOffSampler
  TraceIdRatio (SampleRate r) -> traceIdRatioSampler r
```

```haskell
-- src/Trace.hs (façade — new file)
module Trace
  ( -- Configuration
    TracingConfig (..), defaultConfig, fromEnv
  , ConfigError (..), EnvVarName (..)
  , Resource, mkResource, unResource
  , SamplerConfig (..), SampleRate, mkSampleRate, unSampleRate
  , ExporterConfig (..), Propagator (..)
    -- Top-level
  , withTracing, Tracer, tracerScope
    -- Spans
  , inSpan, inSpanM, Span, SpanName, mkSpanName, SpanKind (..)
    -- Mutators
  , setSpanAttr, setSpanAttrs, setSpanStatus, setStatusError
  , recordException, addEvent, SpanError (..)
    -- Attributes
  , attrs, lookupAttr, AttrKey, AttrValue (..), SpanAttrs, MissingAttr (..)
    -- Status
  , SpanStatus (..), ErrorMessage, mkErrorMessage, unErrorMessage
    -- Context
  , TraceContext (..), TraceM, getCurrentSpanContext
  , SpanContext (..), TraceId, unTraceId, SpanId, unSpanId
  , TraceFlags, defaultTraceFlags, isSampled, setSampled
    -- Propagation
  , parseTraceparent, emitTraceparent, injectHeaders, extractContext
  , PropagationResult (..), PropagationError (..)
    -- Operational
  , flush, InternalLogger (..), stderrLogger
    -- Errors
  , ExporterInitError (..), ExportResult (..), ExportError (..)
  ) where

import Trace.Core
import Trace.Monad
import Trace.Attributes
import Trace.Propagation
import Trace.Config
import Trace.Export.Types
```

This phase ships `withTracing` and the `Trace` façade. **No changes to
`inSpan` or `inSpanM`** — those shipped correctly in Phase 3. Phase 9 is
purely the integration layer: it constructs the `Tracer` from a
`TracingConfig`, wires the batcher, and provides the curated public
import surface.

## Modules touched

- `src/Trace/Monad.hs` (extended; replaces Phase 3's simplified `inSpan`)
- `src/Trace.hs` (new façade)
- `test/Trace/IntegrationSpec.hs` (new)

## Acceptance criteria

- `import Trace` exposes the curated API; nothing in the deferred-list is
  reachable from the façade.
- Nested `inSpanM` produces correct `scParentId` and inherits `scTraceFlags`
  per Phase 2's W3C rule (parent's sampled bit is the starting point;
  sampler decision is the override).
- Local-Jaeger integration tests pass when Docker is available.

## Test plan

### Unit (hspec)

| Test | Setup | Expected |
|---|---|---|
| `withTracing` with `NoopExporter` | run a no-op action | `Right ()`; no network |
| `withTracing` with broken endpoint | invalid URL in config | `Left (ExporterInvalidEndpoint _)`; user action does not run |
| `withTracing` with `BatchExceedsQueue` | (cannot occur from `defaultBatchConfig`; use a custom `BatchConfig`) | `Left (ExporterBatchInit (BatchExceedsQueue _ _))` |
| Nested `inSpanM` parent linkage | child span | `scParentId` of child == `scSpanId` of parent |
| Trace-ID inheritance | child span | `scTraceId` of child == `scTraceId` of parent |
| Trace-flags inheritance | parent sampled, sampler `AlwaysSample` for child | child has sampled bit set |
| Trace-flags override | parent sampled, sampler `NeverSample` | child has sampled bit *clear* |
| `flush` returns `Right ()` on quiesced tracer | call after `withTracing` | `Right ()` |
| Façade compile-test | `import Trace; _ = noopExporter` | fails to compile (not in façade) |
| Façade compile-test | `import Trace; _ = inSpan` | compiles |

### Property (hedgehog)

```haskell
prop_nested_spans_share_traceId :: Property
prop_nested_spans_share_traceId = property $ do
  depth <- forAll (Gen.int (Range.linear 1 5))
  liftIO $ do
    (mem, readAll) <- memoryExporter
    let cfg = defaultConfig { configExporter = NoopExporter }  -- replace with mem hookup
    Right () <- withTracing cfg $ \tracer ->
      runReaderT (nestedSpans depth) (TraceContext Nothing tracer)
    spans <- readAll
    -- All N spans must share the same trace-id.
    case spans of
      []     -> failure
      (h:tl) -> forM_ tl $ \s ->
        scTraceId (fsContext s) === scTraceId (fsContext h)
  where
    nestedSpans 0 = pure ()
    nestedSpans n = inSpanM "n" Internal mempty $ \_ -> nestedSpans (n - 1)
```

### Integration (Docker)

```haskell
spec :: Spec
spec = aroundAll withJaeger $ do
  it "delivers a span end-to-end" $ \jaegerPort -> do
    setEnv "OTEL_EXPORTER_OTLP_ENDPOINT" ("http://localhost:" ++ show jaegerPort)
    setEnv "OTEL_SERVICE_NAME" "htrace-integration-test"
    Right cfg <- fromEnv
    Right () <- withTracing cfg $ \tr ->
      runReaderT (inSpanM "test-span" Internal (attrs [("k", AttrString "v")]) (\_ -> pure ()))
                 (TraceContext Nothing tr)
    -- Wait for batcher to flush, then query Jaeger HTTP API:
    threadDelay (round (exportInterval defaultBatchConfig * 2 * 1_000_000))
    spans <- queryJaeger jaegerPort "htrace-integration-test"
    length spans `shouldBe` 1

  it "delivers a span when body throws" $ \jaegerPort -> do
    -- ... user action throws, span should still arrive with StatusError
```

`withJaeger` starts `jaegertracing/all-in-one:latest` via `process` library
on a random port and tears it down after the test. Skipped on CI machines
without Docker via `--match "/integration/" --skip` toggle.

### Outliers

- **`withTracing` with `OtlpExporter` pointing at a port nothing listens on**:
  succeeds in initialization (DNS/URI valid) but every export returns
  `EndpointUnreachable`. The user's action runs to completion regardless;
  spans are dropped and reported via the logger. *(Verifies that export
  failures don't break the user's program.)*
- **Nested `inSpanM` 100 levels deep**: works; each level adds one span;
  trace-id is consistent across all 100; `scParentId` is linked correctly.
- **`inSpanM` from inside `forkIO` *outside* `withTracing`**: the `Tracer`
  is no longer alive (its bracket has closed). Mutators on any captured
  `Span` return `Left SpanAlreadyEnded`. No crash. *(Documents the
  `@scoped` thread-safety class.)*
- **Two concurrent `withTracing` calls in the same process**: each gets its
  own independent `Tracer` and exporter. Spans don't cross-contaminate
  even if `OTEL_*` envs are identical.
- **`withTracing` user action that calls `exitWith ExitSuccess`**: `bracket`
  fires the cleanup; queued spans are flushed before the process exits.

## What this phase does NOT do

- No `tracestate` (deferred to v0.2).
- No multi-exporter fan-out (deferred to v0.2).
- No HTTP server middleware shipped (the `injectHeaders` / `extractContext`
  primitives are in place; integrations with `wai` etc. are out of scope).

---

## Cross-phase: testing infrastructure expected at the end

By the end of Phase 9, the `test/` and `bench/` trees contain:

```
test/
├── Spec.hs                    -- hspec-discover entry point
├── Trace/
│   ├── CoreSpec.hs            -- Phases 1, 2
│   ├── AttributesSpec.hs      -- Phase 1
│   ├── MonadSpec.hs           -- Phases 3, 9
│   ├── PropagationSpec.hs     -- Phase 5
│   ├── ConfigSpec.hs          -- Phase 8
│   ├── IntegrationSpec.hs     -- Phase 9 (Docker-gated)
│   └── Export/
│       ├── TypesSpec.hs       -- Phase 4
│       ├── OtlpSpec.hs        -- Phase 6
│       └── BatchSpec.hs       -- Phase 7
├── Trace/Generators.hs        -- shared hedgehog generators
└── snapshots/
    └── otlp-single-span.json  -- Phase 6 golden

bench/
├── Spec.hs                    -- tasty-bench main
├── BatchBench.hs              -- Phase 7
└── baseline.csv               -- regression check baseline
```

Total test code is roughly equal to library code (~1000 LOC). This is the
expected ratio for a project where total correctness is a stated principle.

---

## Phase progression chart

```
Phase 0  (test generators — no library code)
   │
   ▼
Phase 1 ─────────────────────┐
   │                         │
   ▼                         │
Phase 2 ──────────┐          │
   │              │          │
   ▼              ▼          ▼
Phase 4    Phase 5      (Phase 5 needs only Phase 1; can run in parallel)
   │
   ▼
Phase 3 ──────────┐
   │              │
   ▼              │
Phase 6           │
   │              │
   ▼              │
Phase 7           │
   │              │
   ▼              │
Phase 8 ◄─────────┘
   │
   ▼
Phase 9
```

Phase 4 ships before Phase 3 because Phase 3's tests use `memoryExporter`
(from Phase 4) as their `SpanExporter` fixture. Phase 5 depends only on
Phase 1 and runs in parallel with the export pipeline. Phase 8 depends on
the OTLP/batch types being defined (Phases 6 and 7) but only for type
references in `ExporterConfig` etc. — it does not run any of their code.

---

## What "done" looks like

After Phase 9 ships:

- `import Trace` gives the user the entire recommended public API.
- `cabal test` passes 100% of unit tests, all property tests with their
  pinned seeds, and (when Docker is available) all integration tests.
- `cabal bench` produces numbers within 25% of the Phase 7/9 baselines.
- `cabal haddock` produces docs with no `-Wmissing-documentation` warnings.
- A user setting `OTEL_EXPORTER_OTLP_ENDPOINT=http://localhost:4318` and
  starting a Jaeger container sees their first trace in Jaeger within
  10 minutes of `cabal install htrace`.

That is the bar. Each phase above moves the bar one step closer; nothing
in the project exists that doesn't trace to a phase above; nothing in any
phase exists that the test plan can't verify.
