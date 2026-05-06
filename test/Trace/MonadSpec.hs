module Trace.MonadSpec (spec) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Concurrent.STM (readTVarIO)
import Control.Monad (forM_, replicateM, void)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as Text
import Hedgehog (forAll, (===))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)
import Control.Monad.Reader (runReaderT)
import Trace.Attributes
import Trace.Core
import Trace.Export.Types
import Trace.Monad
import Trace.Generators

-- ---------------------------------------------------------------------------
-- Test helpers
-- ---------------------------------------------------------------------------

mkTestTracer :: SpanExporter -> Tracer
mkTestTracer exporter = Tracer
  { tracerScope    = InstrumentationScope "test" Nothing
  , tracerSampler  = alwaysOnSampler
  , tracerExporter = exporter
  , tracerClock    = systemClock
  , tracerLogger   = silentLogger
  }

withCapturedSpan
  :: (Tracer -> Span -> IO ())
  -> IO FinishedSpan
withCapturedSpan action = do
  (exporter, readAll) <- memoryExporter
  let tracer = mkTestTracer exporter
  inSpan tracer "test" Internal mempty (action tracer)
  spans <- readAll
  case spans of
    (s:_) -> pure s
    []    -> fail "no span was exported"

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do

  describe "inSpan" $ do
    it "exports span on normal return" $ do
      (exporter, readAll) <- memoryExporter
      let tracer = mkTestTracer exporter
      inSpan tracer "t" Internal mempty (\_ -> pure ())
      spans <- readAll
      length spans `shouldBe` 1

    it "exports span when body throws" $ do
      (exporter, readAll) <- memoryExporter
      let tracer = mkTestTracer exporter
      _ <- (inSpan tracer "t" Internal mempty
              (\_ -> fail "boom" :: IO ()) :: IO ())
             `shouldThrow` anyException
      spans <- readAll
      length spans `shouldBe` 1

    it "re-raises exception after finalising" $ do
      (exporter, _) <- memoryExporter
      let tracer = mkTestTracer exporter
      let action = inSpan tracer "t" Internal mempty
                     (\_ -> ioError (userError "boom"))
      action `shouldThrow` anyIOException

    it "span start time <= end time" $ do
      fs <- withCapturedSpan (\_ _ -> pure ())
      fsStartTime fs `shouldSatisfy` (<= fsEndTime fs)

    it "alwaysOffSampler produces no exported spans" $ do
      (exporter, readAll) <- memoryExporter
      let tracer = (mkTestTracer exporter)
                     { tracerSampler = alwaysOffSampler }
      inSpan tracer "t" Internal mempty (\_ -> pure ())
      spans <- readAll
      length spans `shouldBe` 0

  describe "setSpanAttr" $ do
    it "sets an attribute on a live span" $ do
      fs <- withCapturedSpan $ \_ sp -> do
        _ <- setSpanAttr sp (AttrKey "k") (AttrString "v")
        pure ()
      lookupAttr (AttrKey "k") (fsAttributes fs)
        `shouldBe` Right (AttrString "v")

    it "returns Left SpanAlreadyEnded after span ends" $ do
      (exporter, _) <- memoryExporter
      let tracer = mkTestTracer exporter
      spRef <- newIORef (error "uninitialised")
      inSpan tracer "t" Internal mempty $ \sp -> writeIORef spRef sp
      sp <- readIORef spRef
      result <- setSpanAttr sp (AttrKey "k") (AttrString "v")
      result `shouldBe` Left SpanAlreadyEnded

    it "returns Left SpanWasDropped on dropped span" $ do
      (exporter, _) <- memoryExporter
      let tracer = (mkTestTracer exporter)
                     { tracerSampler = alwaysOffSampler }
      result <- inSpan tracer "t" Internal mempty $ \sp ->
        setSpanAttr sp (AttrKey "k") (AttrString "v")
      result `shouldBe` Left SpanWasDropped

  describe "setSpanAttrs" $ do
    it "merges multiple attributes" $ do
      fs <- withCapturedSpan $ \_ sp -> do
        _ <- setSpanAttrs sp
               [ (AttrKey "a", AttrInt 1)
               , (AttrKey "b", AttrInt 2) ]
        pure ()
      lookupAttr (AttrKey "a") (fsAttributes fs) `shouldBe` Right (AttrInt 1)
      lookupAttr (AttrKey "b") (fsAttributes fs) `shouldBe` Right (AttrInt 2)

  describe "setStatusError" $ do
    it "sets StatusError with message" $ do
      fs <- withCapturedSpan $ \_ sp -> do
        _ <- setStatusError sp "something failed"
        pure ()
      case fsStatus fs of
        StatusError em -> unErrorMessage em `shouldBe` "something failed"
        other          -> expectationFailure ("expected StatusError, got: " <> show other)

    it "falls back to <unspecified error> for blank message" $ do
      fs <- withCapturedSpan $ \_ sp -> do
        _ <- setStatusError sp "   "
        pure ()
      case fsStatus fs of
        StatusError em ->
          unErrorMessage em `shouldBe` "<unspecified error>"
        other ->
          expectationFailure ("expected StatusError, got: " <> show other)

  describe "addEvent" $ do
    it "appends events in chronological order" $ do
      fs <- withCapturedSpan $ \_ sp -> do
        _ <- addEvent sp "first"  mempty
        _ <- addEvent sp "second" mempty
        _ <- addEvent sp "third"  mempty
        pure ()
      map eventName (fsEvents fs) `shouldBe` ["first", "second", "third"]

    it "returns Left SpanAlreadyEnded after span ends" $ do
      (exporter, _) <- memoryExporter
      let tracer = mkTestTracer exporter
      spRef <- newIORef (error "uninitialised")
      inSpan tracer "t" Internal mempty $ \sp -> writeIORef spRef sp
      sp <- readIORef spRef
      result <- addEvent sp "late" mempty
      result `shouldBe` Left SpanAlreadyEnded

  describe "recordException" $ do
    it "sets exception attributes" $ do
      fs <- withCapturedSpan $ \_ sp -> do
        void $ recordException sp (userError "boom")
      let ev = head (fsEvents fs)
      lookupAttr (AttrKey "exception.message") (eventAttributes ev)
        `shouldSatisfy` (\r -> case r of
          Right (AttrString m) -> "boom" `Text.isInfixOf` m
          _ -> False)

    it "sets span status to StatusError" $ do
      fs <- withCapturedSpan $ \_ sp -> do
        void $ recordException sp (userError "boom")
      case fsStatus fs of
        StatusError _ -> pure ()
        _             -> expectationFailure "expected StatusError"

  describe "inSpanM" $ do
    it "child span inherits trace ID from parent" $ do
      (exporter, readAll) <- memoryExporter
      let tracer = mkTestTracer exporter
          ctx = TraceContext Nothing tracer
      runReaderT
        (inSpanM "parent" Internal mempty $ \_ ->
           inSpanM "child" Internal mempty $ \_ -> pure ())
        ctx
      spans <- readAll
      let traceIds = map (scTraceId . fsContext) spans
      length (filter (== head traceIds) traceIds) `shouldBe` 2

    it "child span has correct parent ID" $ do
      (exporter, readAll) <- memoryExporter
      let tracer = mkTestTracer exporter
          ctx = TraceContext Nothing tracer
      runReaderT
        (inSpanM "parent" Internal mempty $ \parent ->
           inSpanM "child" Internal mempty $ \_ -> do
             parentCtx <- getCurrentSpanContext
             liftIO $ case parentCtx of
               Just ctx -> do
                 scTraceId ctx `shouldBe` scTraceId (spanContext parent)
                 scParentId ctx `shouldBe`
                   Just (scSpanId (spanContext parent))
               Nothing ->
                 expectationFailure "Expected active span context")
        ctx
      spans <- readAll
      let [child, parent] = spans
      scParentId (fsContext child)
        `shouldBe` Just (scSpanId (fsContext parent))

  describe "flush" $ do
    it "returns Right ()" $ do
      let tracer = mkTestTracer noopExporter
      flush tracer `shouldReturn` Right ()