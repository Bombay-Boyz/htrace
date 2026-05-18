module Trace.IntegrationSpec (spec) where

import Control.Monad.Reader (runReaderT)
import Data.IORef (newIORef, writeIORef, readIORef)
import Test.Hspec

import Trace.Attributes
import Trace.Config
import Trace.Core
import Trace.Export.Types
import Trace.Generators (mkTestSpanName)
import Trace.Monad

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do

  describe "withTracing" $ do
    it "succeeds with NoopExporter" $ do
      result <- withTracing defaultConfig (\_ -> pure ())
      case result of
        Right () -> pure ()
        Left e   -> expectationFailure ("unexpected error: " <> show e)

    it "runs the user action" $ do
      ran <- newIORef False
      result <- withTracing defaultConfig (\_ -> writeIORef ran True)
      result `shouldBe` Right ()
      readIORef ran >>= (`shouldBe` True)

    it "shuts down cleanly when action throws" $ do
      _ <- (withTracing defaultConfig
              (\_ -> ioError (userError "boom")) :: IO (Either ExporterInitError ()))
             `shouldThrow` anyIOException
      pure ()

    it "nested inSpanM spans share the same trace ID" $ do
      (mem, readAll) <- memoryExporter
      let tracer = Tracer
            { tracerScope    = InstrumentationScope "test" Nothing
            , tracerResource = defaultResource
            , tracerSampler  = alwaysOnSampler
            , tracerExporter = mem
            , tracerClock    = systemClock
            , tracerLogger   = silentLogger
            }
      runReaderT
        ( inSpanM (mkTestSpanName "parent") Internal mempty $ \_ ->
            inSpanM (mkTestSpanName "child")  Internal mempty $ \_ -> pure ()
        )
        (TraceContext Nothing tracer)
      spans <- readAll
      length spans `shouldBe` 2
      let tids = map (scTraceId . fsContext) spans
      case tids of
        (t:ts) -> all (== t) ts `shouldBe` True
        []     -> expectationFailure "no spans"

    it "child span has correct scParentId" $ do
      (mem, readAll) <- memoryExporter
      let tracer = Tracer
            { tracerScope    = InstrumentationScope "test" Nothing
            , tracerResource = defaultResource
            , tracerSampler  = alwaysOnSampler
            , tracerExporter = mem
            , tracerClock    = systemClock
            , tracerLogger   = silentLogger
            }
      runReaderT
        ( inSpanM (mkTestSpanName "parent") Internal mempty $ \_ ->
            inSpanM (mkTestSpanName "child")  Internal mempty $ \_ -> pure ()
        )
        (TraceContext Nothing tracer)
      spans <- readAll
      (child, parent) <- case spans of
        [c, p] -> pure (c, p)
        _      -> fail ("expected 2 spans, got " <> show (length spans))
      scParentId (fsContext child)
        `shouldBe` Just (scSpanId (fsContext parent))

    it "samplerFromConfig AlwaysSample exports spans" $ do
      (mem, readAll) <- memoryExporter
      let tracer = Tracer
            { tracerScope    = InstrumentationScope "test" Nothing
            , tracerResource = defaultResource
            , tracerSampler  = samplerFromConfig AlwaysSample
            , tracerExporter = mem
            , tracerClock    = systemClock
            , tracerLogger   = silentLogger
            }
      inSpan tracer (mkTestSpanName "t") Internal mempty (\_ -> pure ())
      spans <- readAll
      length spans `shouldBe` 1

    it "samplerFromConfig NeverSample exports no spans" $ do
      (mem, readAll) <- memoryExporter
      let tracer = Tracer
            { tracerScope    = InstrumentationScope "test" Nothing
            , tracerResource = defaultResource
            , tracerSampler  = samplerFromConfig NeverSample
            , tracerExporter = mem
            , tracerClock    = systemClock
            , tracerLogger   = silentLogger
            }
      inSpan tracer (mkTestSpanName "t") Internal mempty (\_ -> pure ())
      spans <- readAll
      length spans `shouldBe` 0

    it "flush returns Right () inside withTracing" $ do
      Right () <- withTracing defaultConfig $ \tracer -> do
        result <- flush tracer
        result `shouldBe` Right ()
      pure ()

  describe "Trace facade" $ do
    it "inSpan is accessible and works end-to-end" $ do
      (mem, readAll) <- memoryExporter
      let tracer = Tracer
            { tracerScope    = InstrumentationScope "test" Nothing
            , tracerResource = defaultResource
            , tracerSampler  = alwaysOnSampler
            , tracerExporter = mem
            , tracerClock    = systemClock
            , tracerLogger   = silentLogger
            }
      inSpan tracer (mkTestSpanName "facade-test") Internal
        (attrs [(AttrKey "test", AttrString "yes")])
        (\_ -> pure ())
      spans <- readAll
      length spans `shouldBe` 1
