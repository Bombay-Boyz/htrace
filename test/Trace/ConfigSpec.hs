{-# LANGUAGE BlockArguments #-}
module Trace.ConfigSpec (spec) where

import Control.Exception (bracket_)
import Data.List.NonEmpty qualified as NE
import System.Environment (setEnv, unsetEnv)
import Test.Hspec

import Trace.Attributes
import Trace.Config
import Trace.Export.Types

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

withEnvVars :: [(String, String)] -> IO a -> IO a
withEnvVars kvs action =
  bracket_
    (mapM_ (uncurry setEnv) kvs)
    (mapM_ (unsetEnv . fst) kvs)
    action

withCleanEnv :: IO a -> IO a
withCleanEnv action =
  bracket_
    (mapM_ unsetEnv otelVars)
    (mapM_ unsetEnv otelVars)
    action
  where
    otelVars =
      [ "OTEL_SDK_DISABLED"
      , "OTEL_TRACES_EXPORTER"
      , "OTEL_EXPORTER_OTLP_ENDPOINT"
      , "OTEL_EXPORTER_OTLP_PROTOCOL"
      , "OTEL_EXPORTER_OTLP_HEADERS"
      , "OTEL_EXPORTER_OTLP_TIMEOUT"
      , "OTEL_TRACES_SAMPLER"
      , "OTEL_TRACES_SAMPLER_ARG"
      , "OTEL_SERVICE_NAME"
      , "OTEL_RESOURCE_ATTRIBUTES"
      ]

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = around_ withCleanEnv $ do

  describe "fromEnv" $ do

    describe "OTEL_SDK_DISABLED" $ do
      it "returns defaultConfig with NoopExporter when set to 'true'" $ do
        withEnvVars [("OTEL_SDK_DISABLED", "true")] $ do
          result <- fromEnv
          case result of
            Right cfg -> configExporter cfg `shouldBe` NoopExporter
            Left errs -> expectationFailure (show errs)

      it "disables when set to 'TRUE' (case-insensitive match)" $ do
        withEnvVars [("OTEL_SDK_DISABLED", "TRUE")] $ do
          result <- fromEnv
          case result of
            Right cfg -> configExporter cfg `shouldBe` NoopExporter
            Left errs -> expectationFailure (show errs)

      it "does not disable when set to '1'" $ do
        withEnvVars
          [ ("OTEL_SDK_DISABLED",    "1")
          , ("OTEL_TRACES_EXPORTER", "none")
          ] $ do
          result <- fromEnv
          case result of
            Right cfg -> configExporter cfg `shouldBe` NoopExporter
            Left errs -> expectationFailure (show errs)

    describe "OTEL_TRACES_EXPORTER" $ do
      it "returns NoopExporter when set to 'none'" $ do
        withEnvVars [("OTEL_TRACES_EXPORTER", "none")] $ do
          result <- fromEnv
          case result of
            Right cfg -> configExporter cfg `shouldBe` NoopExporter
            Left errs -> expectationFailure (show errs)

      it "returns error for unknown exporter type" $ do
        withEnvVars [("OTEL_TRACES_EXPORTER", "zipkin")] $ do
          result <- fromEnv
          case result of
            Left errs ->
              any isInvalidVarValue (NE.toList errs) `shouldBe` True
            Right _ ->
              expectationFailure "expected Left"

    describe "OTEL_EXPORTER_OTLP_ENDPOINT" $ do
      it "builds OtlpExporter for a valid http endpoint" $ do
        withEnvVars
          [ ("OTEL_EXPORTER_OTLP_ENDPOINT", "http://localhost:4318")
          , ("OTEL_SERVICE_NAME",           "svc")
          ] $ do
          result <- fromEnv
          case result of
            Right cfg -> case configExporter cfg of
              OtlpExporter _ -> pure ()
              other ->
                expectationFailure ("expected OtlpExporter, got: " <> show other)
            Left errs -> expectationFailure (show errs)

      it "returns MissingRequiredVar when OTLP endpoint is absent" $ do
        withEnvVars [("OTEL_TRACES_EXPORTER", "otlp")] $ do
          result <- fromEnv
          case result of
            Left errs ->
              any isMissingVar (NE.toList errs) `shouldBe` True
            Right _ ->
              expectationFailure "expected Left"

      it "returns error for unsupported protocol" $ do
        withEnvVars
          [ ("OTEL_EXPORTER_OTLP_ENDPOINT", "http://localhost:4318")
          , ("OTEL_EXPORTER_OTLP_PROTOCOL", "grpc")
          ] $ do
          result <- fromEnv
          case result of
            Left errs ->
              any isUnsupportedProtocol (NE.toList errs) `shouldBe` True
            Right _ ->
              expectationFailure "expected Left"

      it "accepts http/json protocol" $ do
        withEnvVars
          [ ("OTEL_EXPORTER_OTLP_ENDPOINT", "http://localhost:4318")
          , ("OTEL_EXPORTER_OTLP_PROTOCOL", "http/json")
          ] $ do
          result <- fromEnv
          case result of
            Right cfg -> case configExporter cfg of
              OtlpExporter _ -> pure ()
              other ->
                expectationFailure ("expected OtlpExporter, got: " <> show other)
            Left errs -> expectationFailure (show errs)

    describe "OTEL_EXPORTER_OTLP_TIMEOUT" $ do
      it "returns error for negative timeout" $ do
        withEnvVars
          [ ("OTEL_EXPORTER_OTLP_ENDPOINT", "http://localhost:4318")
          , ("OTEL_EXPORTER_OTLP_TIMEOUT",  "-1")
          ] $ do
          result <- fromEnv
          case result of
            Left errs ->
              any isInvalidVarValue (NE.toList errs) `shouldBe` True
            Right _ ->
              expectationFailure "expected Left"

      it "returns error for non-numeric timeout" $ do
        withEnvVars
          [ ("OTEL_EXPORTER_OTLP_ENDPOINT", "http://localhost:4318")
          , ("OTEL_EXPORTER_OTLP_TIMEOUT",  "abc")
          ] $ do
          result <- fromEnv
          case result of
            Left errs ->
              any isInvalidVarValue (NE.toList errs) `shouldBe` True
            Right _ ->
              expectationFailure "expected Left"

    describe "OTEL_TRACES_SAMPLER" $ do
      it "defaults to AlwaysSample when unset" $ do
        withEnvVars [("OTEL_TRACES_EXPORTER", "none")] $ do
          result <- fromEnv
          case result of
            Right cfg -> configSampler cfg `shouldBe` AlwaysSample
            Left errs -> expectationFailure (show errs)

      it "sets AlwaysSample for 'always_on'" $ do
        withEnvVars
          [ ("OTEL_TRACES_EXPORTER", "none")
          , ("OTEL_TRACES_SAMPLER",  "always_on")
          ] $ do
          result <- fromEnv
          case result of
            Right cfg -> configSampler cfg `shouldBe` AlwaysSample
            Left errs -> expectationFailure (show errs)

      it "sets NeverSample for 'always_off'" $ do
        withEnvVars
          [ ("OTEL_TRACES_EXPORTER", "none")
          , ("OTEL_TRACES_SAMPLER",  "always_off")
          ] $ do
          result <- fromEnv
          case result of
            Right cfg -> configSampler cfg `shouldBe` NeverSample
            Left errs -> expectationFailure (show errs)

      it "sets TraceIdRatio for 'traceidratio' with valid arg" $ do
        withEnvVars
          [ ("OTEL_TRACES_EXPORTER",    "none")
          , ("OTEL_TRACES_SAMPLER",     "traceidratio")
          , ("OTEL_TRACES_SAMPLER_ARG", "0.5")
          ] $ do
          result <- fromEnv
          case result of
            Right cfg -> case configSampler cfg of
              TraceIdRatio sr -> unSampleRate sr `shouldBe` 0.5
              other ->
                expectationFailure ("expected TraceIdRatio, got: " <> show other)
            Left errs -> expectationFailure (show errs)

      it "returns error for sample rate > 1" $ do
        withEnvVars
          [ ("OTEL_TRACES_EXPORTER",    "none")
          , ("OTEL_TRACES_SAMPLER",     "traceidratio")
          , ("OTEL_TRACES_SAMPLER_ARG", "1.5")
          ] $ do
          result <- fromEnv
          case result of
            Left errs ->
              any isInvalidSampleRate (NE.toList errs) `shouldBe` True
            Right _ ->
              expectationFailure "expected Left"

      it "returns MissingRequiredVar when sampler arg is absent" $ do
        withEnvVars
          [ ("OTEL_TRACES_EXPORTER", "none")
          , ("OTEL_TRACES_SAMPLER",  "traceidratio")
          ] $ do
          result <- fromEnv
          case result of
            Left errs ->
              any isMissingVar (NE.toList errs) `shouldBe` True
            Right _ ->
              expectationFailure "expected Left"

      it "returns error for unknown sampler" $ do
        withEnvVars
          [ ("OTEL_TRACES_EXPORTER", "none")
          , ("OTEL_TRACES_SAMPLER",  "random")
          ] $ do
          result <- fromEnv
          case result of
            Left errs ->
              any isInvalidVarValue (NE.toList errs) `shouldBe` True
            Right _ ->
              expectationFailure "expected Left"

      it "sets ParentBased AlwaysSample for 'parentbased_always_on'" $ do
        withEnvVars
          [ ("OTEL_TRACES_EXPORTER", "none")
          , ("OTEL_TRACES_SAMPLER",  "parentbased_always_on")
          ] $ do
          result <- fromEnv
          case result of
            Right cfg -> configSampler cfg `shouldBe` ParentBased AlwaysSample
            Left errs -> expectationFailure (show errs)

      it "sets ParentBased NeverSample for 'parentbased_always_off'" $ do
        withEnvVars
          [ ("OTEL_TRACES_EXPORTER", "none")
          , ("OTEL_TRACES_SAMPLER",  "parentbased_always_off")
          ] $ do
          result <- fromEnv
          case result of
            Right cfg -> configSampler cfg `shouldBe` ParentBased NeverSample
            Left errs -> expectationFailure (show errs)

      it "sets ParentBased TraceIdRatio for 'parentbased_traceidratio' with valid arg" $ do
        withEnvVars
          [ ("OTEL_TRACES_EXPORTER",    "none")
          , ("OTEL_TRACES_SAMPLER",     "parentbased_traceidratio")
          , ("OTEL_TRACES_SAMPLER_ARG", "0.25")
          ] $ do
          result <- fromEnv
          case result of
            Right cfg ->
              case configSampler cfg of
                ParentBased (TraceIdRatio sr) -> unSampleRate sr `shouldBe` 0.25
                other -> expectationFailure ("expected ParentBased TraceIdRatio, got: " <> show other)
            Left errs -> expectationFailure (show errs)

      it "returns MissingRequiredVar for 'parentbased_traceidratio' without arg" $ do
        withEnvVars
          [ ("OTEL_TRACES_EXPORTER", "none")
          , ("OTEL_TRACES_SAMPLER",  "parentbased_traceidratio")
          ] $ do
          result <- fromEnv
          case result of
            Left errs ->
              any isMissingVar (NE.toList errs) `shouldBe` True
            Right _ ->
              expectationFailure "expected Left"

    describe "OTEL_SERVICE_NAME" $ do
      it "sets service.name in resource" $ do
        withEnvVars
          [ ("OTEL_TRACES_EXPORTER", "none")
          , ("OTEL_SERVICE_NAME",    "my-service")
          ] $ do
          result <- fromEnv
          case result of
            Right cfg ->
              lookupAttr (AttrKey "service.name")
                         (unResource (configResource cfg))
                `shouldBe` Right (AttrString "my-service")
            Left errs -> expectationFailure (show errs)

      it "returns error for whitespace-only service name" $ do
        withEnvVars
          [ ("OTEL_TRACES_EXPORTER", "none")
          , ("OTEL_SERVICE_NAME",    "   ")
          ] $ do
          result <- fromEnv
          case result of
            Left errs ->
              any isInvalidVarValue (NE.toList errs) `shouldBe` True
            Right _ ->
              expectationFailure "expected Left"

    describe "OTEL_RESOURCE_ATTRIBUTES" $ do
      it "parses key=value pairs" $ do
        withEnvVars
          [ ("OTEL_TRACES_EXPORTER",     "none")
          , ("OTEL_SERVICE_NAME",        "svc")
          , ("OTEL_RESOURCE_ATTRIBUTES", "env=prod,region=us-east-1")
          ] $ do
          result <- fromEnv
          case result of
            Right cfg -> do
              lookupAttr (AttrKey "env")
                         (unResource (configResource cfg))
                `shouldBe` Right (AttrString "prod")
              lookupAttr (AttrKey "region")
                         (unResource (configResource cfg))
                `shouldBe` Right (AttrString "us-east-1")
            Left errs -> expectationFailure (show errs)

      it "returns error for malformed attribute" $ do
        withEnvVars
          [ ("OTEL_TRACES_EXPORTER",     "none")
          , ("OTEL_SERVICE_NAME",        "svc")
          , ("OTEL_RESOURCE_ATTRIBUTES", "badentry")
          ] $ do
          result <- fromEnv
          case result of
            Left errs ->
              any isInvalidResourceAttr (NE.toList errs) `shouldBe` True
            Right _ ->
              expectationFailure "expected Left"

      it "empty value is treated as no extra attributes" $ do
        withEnvVars
          [ ("OTEL_TRACES_EXPORTER",     "none")
          , ("OTEL_SERVICE_NAME",        "svc")
          , ("OTEL_RESOURCE_ATTRIBUTES", "")
          ] $ do
          result <- fromEnv
          case result of
            Right _ -> pure ()
            Left errs -> expectationFailure (show errs)

    describe "error accumulation" $ do
      it "collects multiple errors simultaneously" $ do
        withEnvVars
          [ ("OTEL_TRACES_EXPORTER",    "otlp")
          , ("OTEL_TRACES_SAMPLER",     "traceidratio")
          , ("OTEL_TRACES_SAMPLER_ARG", "1.5")
          ] $ do
          result <- fromEnv
          case result of
            Left errs -> NE.length errs `shouldSatisfy` (>= 2)
            Right _   -> expectationFailure "expected Left"

  describe "mkSampleRate" $ do
    it "accepts 0.0" $
      case mkSampleRate 0.0 of
        Right sr -> unSampleRate sr `shouldBe` 0.0
        Left e   -> expectationFailure (show e)

    it "accepts 1.0" $
      case mkSampleRate 1.0 of
        Right sr -> unSampleRate sr `shouldBe` 1.0
        Left e   -> expectationFailure (show e)

    it "accepts 0.5" $
      case mkSampleRate 0.5 of
        Right sr -> unSampleRate sr `shouldBe` 0.5
        Left e   -> expectationFailure (show e)

    it "rejects -0.1" $
      case mkSampleRate (-0.1) of
        Left (InvalidSampleRate _) -> pure ()
        other -> expectationFailure ("unexpected: " <> show other)

    it "rejects 1.1" $
      case mkSampleRate 1.1 of
        Left (InvalidSampleRate _) -> pure ()
        other -> expectationFailure ("unexpected: " <> show other)

  describe "mkResource" $ do
    it "accepts a valid service name" $
      case mkResource "my-service" [] of
        Right r ->
          lookupAttr (AttrKey "service.name") (unResource r)
            `shouldBe` Right (AttrString "my-service")
        Left e -> expectationFailure (show e)

    it "rejects empty service name" $
      case mkResource "" [] of
        Left (InvalidVarValue _ _ _) -> pure ()
        other -> expectationFailure ("unexpected: " <> show other)

    it "rejects whitespace-only service name" $
      case mkResource "   " [] of
        Left (InvalidVarValue _ _ _) -> pure ()
        other -> expectationFailure ("unexpected: " <> show other)

-- ---------------------------------------------------------------------------
-- Predicates for error classification
-- ---------------------------------------------------------------------------

isMissingVar :: ConfigError -> Bool
isMissingVar (MissingRequiredVar _) = True
isMissingVar _                      = False

isInvalidVarValue :: ConfigError -> Bool
isInvalidVarValue (InvalidVarValue _ _ _) = True
isInvalidVarValue _                       = False

isInvalidSampleRate :: ConfigError -> Bool
isInvalidSampleRate (InvalidSampleRate _) = True
isInvalidSampleRate _                     = False

isUnsupportedProtocol :: ConfigError -> Bool
isUnsupportedProtocol (UnsupportedOtlpProtocol _) = True
isUnsupportedProtocol _                           = False

isInvalidResourceAttr :: ConfigError -> Bool
isInvalidResourceAttr (InvalidResourceAttribute _ _) = True
isInvalidResourceAttr _                              = False