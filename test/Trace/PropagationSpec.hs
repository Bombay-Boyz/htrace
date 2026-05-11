{-# LANGUAGE OverloadedStrings #-}

module Trace.PropagationSpec (spec) where

import Control.Monad (forM_)
import Data.CaseInsensitive qualified as CI
import Data.Text qualified as Text
import Hedgehog (forAll, (===))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

import Trace.Core
import Trace.Propagation
import Trace.Generators

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do

  describe "parseTraceparent" $ do
    it "parses the W3C spec example" $ do
      let t = "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01"
      case parseTraceparent t of
        PropagationSuccess ctx -> do
          isSampled (scTraceFlags ctx) `shouldBe` True
          scParentId ctx `shouldBe` Nothing
        other -> expectationFailure ("unexpected: " <> show other)

    it "rejects forbidden version ff" $
      parseTraceparent "ff-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01"
        `shouldBe` PropagationInvalid (InvalidVersion "ff")

    it "rejects forbidden version FF (case-normalised)" $
      parseTraceparent "FF-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01"
        `shouldBe` PropagationInvalid (InvalidVersion "ff")

    it "accepts future version 01 (forward compatibility)" $
      case parseTraceparent "01-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01" of
        PropagationSuccess _ -> pure ()
        other -> expectationFailure ("expected PropagationSuccess, got: " <> show other)

    it "accepts future version 0f" $
      case parseTraceparent "0f-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01" of
        PropagationSuccess _ -> pure ()
        other -> expectationFailure ("expected PropagationSuccess, got: " <> show other)

    it "accepts future version 0F (uppercase hex)" $
      case parseTraceparent "0F-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01" of
        PropagationSuccess _ -> pure ()
        other -> expectationFailure ("expected PropagationSuccess, got: " <> show other)

    it "ignores extra fields for future versions" $
      case parseTraceparent "01-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01-extra-data" of
        PropagationSuccess _ -> pure ()
        other -> expectationFailure ("expected PropagationSuccess, got: " <> show other)

    it "ignores extra fields for version 00 as well" $
      case parseTraceparent "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01-ignored" of
        PropagationSuccess _ -> pure ()
        other -> expectationFailure ("expected PropagationSuccess, got: " <> show other)

    it "rejects malformed inputs" $ do
      let cases =
            [ "00-4bf92f3577b34da6a3ce929d0e0e47-00f067aa0ba902b7-01"   -- short trace-id
            , "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902-01"   -- short span-id
            , "00-00000000000000000000000000000000-00f067aa0ba902b7-01"  -- all-zero trace-id
            , "00-4bf92f3577b34da6a3ce929d0e0e4736-0000000000000000-01"  -- all-zero span-id
            , "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-zz"  -- bad flags
            , "00-4bf92f3577b34da6a3ce929d0e0e4736"                      -- only 2 fields
            , ""                                                          -- empty
            ]
      forM_
        cases
        (\t -> case parseTraceparent t of
            PropagationInvalid _ -> pure ()
            other -> expectationFailure ("unexpected: " <> show other))

    it "rejects fewer than four fields even for future version" $
      case parseTraceparent "01-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7" of
        PropagationInvalid (MalformedHeader _) -> pure ()
        other -> expectationFailure ("unexpected: " <> show other)

  describe "emitTraceparent" $ do
    it "emits valid structure" $ do
      let t = emitTraceparent (fsContext sampleFinishedSpan)
      Text.isPrefixOf "00-" t `shouldBe` True
      length (Text.splitOn "-" t) `shouldBe` 4

    it "always emits version 00" $ do
      let t = emitTraceparent (fsContext sampleFinishedSpan)
      case Text.splitOn "-" t of
        (v:_) -> v `shouldBe` "00"
        []    -> expectationFailure "empty result"

  describe "extractContext" $ do
    it "returns absent when missing" $
      extractContext [] `shouldBe` PropagationAbsent

    it "returns invalid for non-utf8 header value" $ do
      let badHeader = (CI.mk "traceparent", "\xff\xfe")
      case extractContext [badHeader] of
        PropagationInvalid (MalformedHeader _) -> pure ()
        other -> expectationFailure ("unexpected: " <> show other)

  describe "injectHeaders" $ do
    it "round-trips context" $ do
      let ctx = fsContext sampleFinishedSpan
          hs  = injectHeaders ctx []
      case extractContext hs of
        PropagationSuccess ctx' -> do
          scTraceId ctx' `shouldBe` scTraceId ctx
          scSpanId  ctx' `shouldBe` scSpanId ctx
        other -> expectationFailure ("unexpected: " <> show other)

    it "replaces existing traceparent" $ do
      let ctx1 = fsContext sampleFinishedSpan
          ctx2 = fsContext (sampleSpan 99)
          hs1  = injectHeaders ctx1 []
          hs2  = injectHeaders ctx2 hs1
          count = length (filter ((== CI.mk "traceparent") . fst) hs2)
      count `shouldBe` 1

  describe "properties" $ do

    it "round-trip property" $
      hedgehog prop_traceparent_round_trip

    it "parse is total" $
      hedgehog prop_parseTraceparent_total

    it "inject preserves other headers" $
      hedgehog prop_inject_preserves_others

    it "inject produces single traceparent" $
      hedgehog prop_inject_single_traceparent

-- ---------------------------------------------------------------------------
-- Properties
-- ---------------------------------------------------------------------------

prop_traceparent_round_trip :: H.PropertyT IO ()
prop_traceparent_round_trip = do
  ctx <- forAll genSpanContextNoParent
  -- emitTraceparent always emits version "00"; must still round-trip.
  case parseTraceparent (emitTraceparent ctx) of
    PropagationSuccess ctx' -> do
      scTraceId ctx'    === scTraceId ctx
      scSpanId ctx'     === scSpanId ctx
      scTraceFlags ctx' === scTraceFlags ctx
    other -> H.footnote (show other) >> H.failure

prop_parseTraceparent_total :: H.PropertyT IO ()
prop_parseTraceparent_total = do
  t <- forAll (Gen.text (Range.linear 0 200) Gen.unicode)
  case parseTraceparent t of
    PropagationSuccess _ -> H.success
    PropagationAbsent    -> H.success
    PropagationInvalid _ -> H.success

prop_inject_preserves_others :: H.PropertyT IO ()
prop_inject_preserves_others = do
  ctx <- forAll genSpanContextNoParent
  hs  <- forAll (Gen.list (Range.linear 0 20) genNonTraceparentHeader)
  let injected = injectHeaders ctx hs
  forM_ hs $ \h -> H.assert (h `elem` injected)

prop_inject_single_traceparent :: H.PropertyT IO ()
prop_inject_single_traceparent = do
  ctx <- forAll genSpanContextNoParent
  hs  <- forAll (Gen.list (Range.linear 0 20) genNonTraceparentHeader)
  let injected = injectHeaders ctx hs
      count = length (filter ((== CI.mk "traceparent") . fst) injected)
  count === 1
