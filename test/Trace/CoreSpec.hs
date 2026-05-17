module Trace.CoreSpec (spec) where

import Control.Monad (replicateM)
import Data.Bits ((.&.))
import Data.ByteString qualified as BS
import Hedgehog (forAll, property, (===))
import Hedgehog qualified as H
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Trace.Core
import Trace.Generators
import Control.Exception (evaluate)

spec :: Spec
spec = do

  -- -------------------------------------------------------------------------
  -- Phase 1: IDs and flags
  -- -------------------------------------------------------------------------

  describe "TraceId" $ do
    describe "newTraceId" $ do
      it "produces a 16-byte value" $ do
        tid <- newTraceId
        BS.length (unTraceId tid) `shouldBe` 16

      it "produces unique values" $ do
        tid1 <- newTraceId
        tid2 <- newTraceId
        tid1 `shouldNotBe` tid2

    describe "traceIdFromBytes" $ do
      it "accepts 16 non-zero bytes" $ do
        let bs = BS.pack [1..16]
        case traceIdFromBytes bs of
          Right tid -> unTraceId tid `shouldBe` bs
          Left  e   -> expectationFailure (show e)

      it "rejects 15 bytes" $
        traceIdFromBytes (BS.replicate 15 1)
          `shouldBe` Left (WrongIdLength 16 15)

      it "rejects 17 bytes" $
        traceIdFromBytes (BS.replicate 17 1)
          `shouldBe` Left (WrongIdLength 16 17)

      it "rejects 0 bytes" $
        traceIdFromBytes BS.empty
          `shouldBe` Left (WrongIdLength 16 0)

      it "rejects all-zero bytes" $
        traceIdFromBytes (BS.replicate 16 0)
          `shouldBe` Left AllZeroId

  describe "SpanId" $ do
    describe "newSpanId" $ do
      it "produces an 8-byte value" $ do
        sid <- newSpanId
        BS.length (unSpanId sid) `shouldBe` 8

    describe "spanIdFromBytes" $ do
      it "accepts 8 non-zero bytes" $ do
        let bs = BS.pack [1..8]
        case spanIdFromBytes bs of
          Right sid -> unSpanId sid `shouldBe` bs
          Left  e   -> expectationFailure (show e)

      it "rejects 7 bytes" $
        spanIdFromBytes (BS.replicate 7 1)
          `shouldBe` Left (WrongIdLength 8 7)

      it "rejects 9 bytes" $
        spanIdFromBytes (BS.replicate 9 1)
          `shouldBe` Left (WrongIdLength 8 9)

      it "rejects all-zero bytes" $
        spanIdFromBytes (BS.replicate 8 0)
          `shouldBe` Left AllZeroId

  describe "TraceFlags" $ do
    it "defaultTraceFlags is unsampled" $
      isSampled defaultTraceFlags `shouldBe` False

    it "setSampled True sets bit 0" $
      setSampled True (TraceFlags 0) `shouldBe` TraceFlags 1

    it "setSampled False clears bit 0" $
      setSampled False (TraceFlags 0xFF) `shouldBe` TraceFlags 0xFE

    it "setSampled True on already-sampled is identity" $
      setSampled True (TraceFlags 1) `shouldBe` TraceFlags 1

    it "setSampled False on unsampled is identity" $
      setSampled False (TraceFlags 0) `shouldBe` TraceFlags 0

  -- -------------------------------------------------------------------------
  -- Phase 2: Span domain types
  -- -------------------------------------------------------------------------

  describe "SpanName" $ do
    it "mkSpanName rejects empty string" $
      mkSpanName "" `shouldBe` Nothing

    it "mkSpanName rejects whitespace-only string" $
      mkSpanName "   " `shouldBe` Nothing

    it "mkSpanName accepts a normal name" $
      mkSpanName "my-span" `shouldBe` Just (SpanName "my-span")

    it "mkSpanName accepts emoji" $
      mkSpanName "🦀" `shouldBe` Just (SpanName "🦀")

    
    it "errors on empty string" $ do
      evaluate ("" :: SpanName) `shouldThrow` errorCall
       "SpanName.fromString: empty or whitespace-only span name. Use mkSpanName for a safe constructor."

    it "IsString accepts a non-empty string" $
      ("my-span" :: SpanName) `shouldBe` SpanName "my-span"

  describe "ErrorMessage" $ do
    it "mkErrorMessage rejects empty string" $
      mkErrorMessage "" `shouldBe` Nothing

    it "mkErrorMessage rejects whitespace-only string" $
      mkErrorMessage "  " `shouldBe` Nothing

    it "mkErrorMessage accepts a normal message" $ do
      case mkErrorMessage "something went wrong" of
        Just em -> unErrorMessage em `shouldBe` "something went wrong"
        Nothing -> expectationFailure "expected Just but got Nothing"

  describe "SpanKind" $ do
    it "covers all constructors via Enum" $
      [minBound .. maxBound :: SpanKind]
        `shouldBe` [Server, Client, Producer, Consumer, Internal]

  describe "Sampler" $ do
    it "alwaysOnSampler returns RecordAndSample" $ do
      tid <- newTraceId
      runSampler alwaysOnSampler Nothing tid "n" Internal mempty
        `shouldBe` RecordAndSample

    it "alwaysOffSampler returns Drop" $ do
      tid <- newTraceId
      runSampler alwaysOffSampler Nothing tid "n" Internal mempty
        `shouldBe` Drop

    it "traceIdRatioSampler 1.0 always samples" $ do
      tids <- replicateM 100 newTraceId
      let s = traceIdRatioSampler 1.0
      all (\tid -> runSampler s Nothing tid "n" Internal mempty
                     == RecordAndSample) tids
        `shouldBe` True

    it "traceIdRatioSampler 0.0 never samples" $ do
      tids <- replicateM 100 newTraceId
      let s = traceIdRatioSampler 0.0
      all (\tid -> runSampler s Nothing tid "n" Internal mempty
                     == Drop) tids
        `shouldBe` True

  describe "FinishedSpan fixtures" $ do
    it "sampleFinishedSpan is Eq-reflexive" $
      sampleFinishedSpan `shouldBe` sampleFinishedSpan

    it "sampleSpan 1 differs from sampleSpan 2" $
      sampleSpan 1 `shouldNotBe` sampleSpan 2

    it "sampleFinishedSpan start <= end" $
      fsStartTime sampleFinishedSpan
        `shouldSatisfy` (<= fsEndTime sampleFinishedSpan)

  -- -------------------------------------------------------------------------
  -- Phase R3: integer-threshold sampler and parentBasedSampler
  -- -------------------------------------------------------------------------

  describe "traceIdRatioSampler" $ do
    it "rate 0.0 samples nothing" $ do
      tids <- replicateM 200 newTraceId
      let s = traceIdRatioSampler 0.0
      all (\t -> runSampler s Nothing t "n" Internal mempty == Drop) tids
        `shouldBe` True

    it "rate 1.0 samples everything" $ do
      tids <- replicateM 200 newTraceId
      let s = traceIdRatioSampler 1.0
      all (\t -> runSampler s Nothing t "n" Internal mempty == RecordAndSample) tids
        `shouldBe` True

    it "negative rate clamps to 0 — samples nothing" $ do
      tids <- replicateM 100 newTraceId
      let s = traceIdRatioSampler (-1.0)
      all (\t -> runSampler s Nothing t "n" Internal mempty == Drop) tids
        `shouldBe` True

    it "rate > 1 clamps to 1 — samples everything" $ do
      tids <- replicateM 100 newTraceId
      let s = traceIdRatioSampler 2.0
      all (\t -> runSampler s Nothing t "n" Internal mempty == RecordAndSample) tids
        `shouldBe` True

    it "is deterministic for the same trace-id" $ do
      tid <- newTraceId
      let s = traceIdRatioSampler 0.5
      runSampler s Nothing tid "n" Internal mempty
        `shouldBe` runSampler s Nothing tid "n" Internal mempty

  describe "parentBasedSampler" $ do
    it "no parent: delegates to root sampler — alwaysOn" $ do
      tid <- newTraceId
      let s = parentBasedSampler alwaysOnSampler
      runSampler s Nothing tid "n" Internal mempty
        `shouldBe` RecordAndSample

    it "no parent: delegates to root sampler — alwaysOff" $ do
      tid <- newTraceId
      let s = parentBasedSampler alwaysOffSampler
      runSampler s Nothing tid "n" Internal mempty
        `shouldBe` Drop

    it "sampled parent gives RecordAndSample regardless of root" $ do
      tid <- newTraceId
      sid <- newSpanId
      let sampledCtx = SpanContext tid sid Nothing (setSampled True defaultTraceFlags)
          s          = parentBasedSampler alwaysOffSampler
      runSampler s (Just sampledCtx) tid "n" Internal mempty
        `shouldBe` RecordAndSample

    it "unsampled parent gives Drop regardless of root" $ do
      tid <- newTraceId
      sid <- newSpanId
      let unsampledCtx = SpanContext tid sid Nothing defaultTraceFlags
          s            = parentBasedSampler alwaysOnSampler
      runSampler s (Just unsampledCtx) tid "n" Internal mempty
        `shouldBe` Drop

  -- -------------------------------------------------------------------------
  -- Properties
  -- -------------------------------------------------------------------------

  describe "properties" $ do
    it "setSampled is idempotent" $
      hedgehog prop_setSampled_idempotent

    it "setSampled preserves bits 1-7" $
      hedgehog prop_setSampled_preserves_other_bits

    it "genTraceId always produces valid ids" $
      hedgehog prop_genTraceId_valid

    it "sampler is deterministic for same input" $
      hedgehog prop_sampler_deterministic

    it "genFinishedSpan always has start <= end" $
      hedgehog prop_finishedSpan_start_le_end

    it "genFinishedSpan is Eq-reflexive" $
      hedgehog prop_finishedSpan_eq_reflexive

    it "parentBasedSampler inherits parent sampling decision" $
      hedgehog prop_parentBased_inherits_parent_decision

    it "parentBasedSampler with no parent delegates to root" $
      hedgehog prop_parentBased_no_parent_delegates

-- ---------------------------------------------------------------------------
-- Properties
-- ---------------------------------------------------------------------------

prop_setSampled_idempotent :: H.PropertyT IO ()
prop_setSampled_idempotent = do
  flags <- forAll genTraceFlags
  setSampled True (setSampled True flags) === setSampled True flags

prop_setSampled_preserves_other_bits :: H.PropertyT IO ()
prop_setSampled_preserves_other_bits = do
  flags <- forAll genTraceFlags
  let TraceFlags before = flags
      TraceFlags after  = setSampled True flags
  (before .&. 0xFE) === (after .&. 0xFE)

prop_genTraceId_valid :: H.PropertyT IO ()
prop_genTraceId_valid = do
  tid <- forAll genTraceId
  H.assert $ case traceIdFromBytes (unTraceId tid) of
    Right _ -> True
    Left _  -> False

prop_sampler_deterministic :: H.PropertyT IO ()
prop_sampler_deterministic = do
  tid  <- forAll genTraceId
  name <- forAll genSpanName
  kind <- forAll genSpanKind
  as   <- forAll genSpanAttrs
  let s  = traceIdRatioSampler 0.5
      d1 = runSampler s Nothing tid name kind as
      d2 = runSampler s Nothing tid name kind as
  d1 === d2

prop_finishedSpan_start_le_end :: H.PropertyT IO ()
prop_finishedSpan_start_le_end = do
  fs <- forAll genFinishedSpan
  H.assert (fsStartTime fs <= fsEndTime fs)

prop_finishedSpan_eq_reflexive :: H.PropertyT IO ()
prop_finishedSpan_eq_reflexive = do
  fs <- forAll genFinishedSpan
  fs === fs

prop_parentBased_inherits_parent_decision :: H.PropertyT IO ()
prop_parentBased_inherits_parent_decision = do
  ctx <- forAll genSpanContext
  let s      = parentBasedSampler alwaysOnSampler
      result = runSampler s (Just ctx) (scTraceId ctx) "n" Internal mempty
  if isSampled (scTraceFlags ctx)
    then result === RecordAndSample
    else result === Drop

prop_parentBased_no_parent_delegates :: H.PropertyT IO ()
prop_parentBased_no_parent_delegates = do
  tid  <- H.evalIO newTraceId
  rate <- forAll (Gen.double (Range.linearFrac 0.0 1.0))
  let root   = traceIdRatioSampler rate
      pBased = parentBasedSampler root
      direct = runSampler root   Nothing tid "n" Internal mempty
      via    = runSampler pBased Nothing tid "n" Internal mempty
  direct === via