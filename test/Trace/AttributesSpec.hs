module Trace.AttributesSpec (spec) where

import Hedgehog (forAll, (===))
import Hedgehog qualified as H
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

import Trace.Attributes
import Trace.Generators

spec :: Spec
spec = do
  describe "attrs" $ do
    it "builds a map from a list" $ do
      let a = attrs [(AttrKey "k", AttrString "v")]
      lookupAttr (AttrKey "k") a `shouldBe` Right (AttrString "v")

    it "last entry wins for duplicate keys" $ do
      let a = attrs [ (AttrKey "k", AttrString "first")
                    , (AttrKey "k", AttrString "second") ]
      lookupAttr (AttrKey "k") a `shouldBe` Right (AttrString "second")

    it "IsString produces the same key as AttrKey" $ do
      let a = attrs [("k", AttrInt 1), (AttrKey "k", AttrInt 2)]
      lookupAttr "k" a `shouldBe` Right (AttrInt 2)

  describe "lookupAttr" $ do
    it "returns Right for a present key" $
      lookupAttr (AttrKey "k1") sampleAttrs
        `shouldBe` Right (AttrString "hello")

    it "returns Left MissingAttr for an absent key" $
      lookupAttr (AttrKey "missing") sampleAttrs
        `shouldBe` Left (MissingAttr (AttrKey "missing"))

    it "MissingAttr carries the queried key" $ do
      let Left (MissingAttr k) = lookupAttr (AttrKey "x") mempty
      k `shouldBe` AttrKey "x"

  describe "Semigroup SpanAttrs" $ do
    it "right operand wins on collision" $ do
      let left_  = attrs [(AttrKey "k", AttrString "L")]
          right_ = attrs [(AttrKey "k", AttrString "R")]
      lookupAttr (AttrKey "k") (left_ <> right_)
        `shouldBe` Right (AttrString "R")

    it "non-overlapping keys are both present" $ do
      let a = attrs [(AttrKey "a", AttrInt 1)]
          b = attrs [(AttrKey "b", AttrInt 2)]
          c = a <> b
      lookupAttr (AttrKey "a") c `shouldBe` Right (AttrInt 1)
      lookupAttr (AttrKey "b") c `shouldBe` Right (AttrInt 2)

  describe "Monoid SpanAttrs" $ do
    it "mempty is the left identity" $ do
      let a = attrs [(AttrKey "k", AttrBool True)]
      (mempty <> a) `shouldBe` a

    it "mempty is the right identity" $ do
      let a = attrs [(AttrKey "k", AttrBool True)]
      (a <> mempty) `shouldBe` a

  describe "properties" $ do
    it "Semigroup is associative" $
      hedgehog prop_semigroup_associative
    it "right-bias holds" $
      hedgehog prop_attrs_right_biased
    it "monoid left identity" $
      hedgehog prop_monoid_left_identity
    it "monoid right identity" $
      hedgehog prop_monoid_right_identity

-- ---------------------------------------------------------------------------
-- Properties
-- ---------------------------------------------------------------------------

prop_semigroup_associative :: H.PropertyT IO ()
prop_semigroup_associative = do
  a <- forAll genSpanAttrs
  b <- forAll genSpanAttrs
  c <- forAll genSpanAttrs
  ((a <> b) <> c) === (a <> (b <> c))

prop_attrs_right_biased :: H.PropertyT IO ()
prop_attrs_right_biased = do
  k  <- forAll genAttrKey
  v1 <- forAll genAttrValue
  v2 <- forAll genAttrValue
  let merged = attrs [(k, v1)] <> attrs [(k, v2)]
  lookupAttr k merged === Right v2

prop_monoid_left_identity :: H.PropertyT IO ()
prop_monoid_left_identity = do
  a <- forAll genSpanAttrs
  (mempty <> a) === a

prop_monoid_right_identity :: H.PropertyT IO ()
prop_monoid_right_identity = do
  a <- forAll genSpanAttrs
  (a <> mempty) === a