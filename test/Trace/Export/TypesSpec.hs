module Trace.Export.TypesSpec (spec) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Data.List.NonEmpty qualified as NE
import Hedgehog (forAll, (===))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

import Trace.Export.Types
import Trace.Generators

spec :: Spec
spec = do

  describe "noopExporter" $ do
    it "exporterExport returns ExportSuccess with span count" $ do
      let spans = sampleFinishedSpan NE.:| [sampleSpan 1, sampleSpan 2]
      result <- exporterExport noopExporter spans
      result `shouldBe` ExportSuccess 3

    it "exporterFlush returns Right ()" $ do
      result <- exporterFlush noopExporter
      result `shouldBe` Right ()

    it "exporterShutdown is idempotent" $ do
      exporterShutdown noopExporter
      exporterShutdown noopExporter

  describe "memoryExporter" $ do
    it "captures exported spans" $ do
      (exporter, readAll) <- memoryExporter
      _ <- exporterExport exporter (sampleSpan 1 NE.:| [])
      _ <- exporterExport exporter (sampleSpan 2 NE.:| [])
      spans <- readAll
      length spans `shouldBe` 2

    it "preserves arrival order" $ do
      (exporter, readAll) <- memoryExporter
      let expected = map sampleSpan [1..5]
      mapM_ (\s -> exporterExport exporter (s NE.:| [])) expected
      received <- readAll
      received `shouldBe` expected

    it "reader is repeatable" $ do
      (exporter, readAll) <- memoryExporter
      _ <- exporterExport exporter (sampleFinishedSpan NE.:| [])
      spans1 <- readAll
      spans2 <- readAll
      spans1 `shouldBe` spans2

    it "exporterFlush returns Right ()" $ do
      (exporter, _) <- memoryExporter
      result <- exporterFlush exporter
      result `shouldBe` Right ()

    it "handles concurrent exports without losing spans" $ do
      (exporter, readAll) <- memoryExporter
      let n = 100
      done <- newEmptyMVar
      mapM_ (\i -> forkIO $ do
        _ <- exporterExport exporter (sampleSpan i NE.:| [])
        putMVar done ()) [1..n]
      mapM_ (\_ -> takeMVar done) [1..n]
      spans <- readAll
      length spans `shouldBe` n

  describe "mkHttpStatus" $ do
    it "accepts 100" $
      fmap unHttpStatus (mkHttpStatus 100) `shouldBe` Just 100

    it "accepts 200" $
      fmap unHttpStatus (mkHttpStatus 200) `shouldBe` Just 200

    it "accepts 599" $
      fmap unHttpStatus (mkHttpStatus 599) `shouldBe` Just 599

    it "rejects 99" $
      mkHttpStatus 99 `shouldBe` Nothing

    it "rejects 600" $
      mkHttpStatus 600 `shouldBe` Nothing

    it "rejects 0" $
      mkHttpStatus 0 `shouldBe` Nothing

    it "rejects negative" $
      mkHttpStatus (-1) `shouldBe` Nothing

  describe "silentLogger" $ do
    it "logWarn does not throw" $
      logWarn silentLogger "test warning"

    it "logError does not throw" $
      logError silentLogger "test error"

  describe "HttpStatus Ord" $ do
    it "ordering is consistent with Int" $ do
      let s200 = maybe (error "bad 200") id (mkHttpStatus 200)
          s404 = maybe (error "bad 404") id (mkHttpStatus 404)
          s500 = maybe (error "bad 500") id (mkHttpStatus 500)
      compare s200 s404 `shouldBe` LT
      compare s500 s404 `shouldBe` GT
      compare s200 s200 `shouldBe` EQ

  describe "properties" $ do
    it "mkHttpStatus range is 100-599" $
      hedgehog prop_mkHttpStatus_range

    it "memoryExporter preserves order under sequential export" $
      hedgehog prop_memoryExporter_preserves_order

-- ---------------------------------------------------------------------------
-- Properties
-- ---------------------------------------------------------------------------

prop_mkHttpStatus_range :: H.PropertyT IO ()
prop_mkHttpStatus_range = do
  n <- forAll (Gen.int (Range.linear (-1000) 1000))
  case mkHttpStatus n of
    Just hs -> do
      let h = unHttpStatus hs
      H.assert (h >= 100)
      H.assert (h <= 599)
      h === n
    Nothing ->
      H.assert (n < 100 || n > 599)

prop_memoryExporter_preserves_order :: H.PropertyT IO ()
prop_memoryExporter_preserves_order = do
  n <- forAll (Gen.int (Range.linear 1 50))
  let spans = map sampleSpan [1..n]
  received <- H.evalIO $ do
    (exporter, readAll) <- memoryExporter
    mapM_ (\s -> exporterExport exporter (s NE.:| [])) spans
    readAll
  received === spans