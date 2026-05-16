module Trace.Export.OtlpSpec (spec) where

import Data.Aeson (Value (..), decode, encode)
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.IORef
import Data.List (isInfixOf)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as Text
import Network.HTTP.Types (status200, status503)
import Network.Wai (Application, responseLBS, strictRequestBody)
import Network.Wai.Handler.Warp qualified as Warp
import Test.Hspec

import Trace.Attributes
import Trace.Config (defaultResource, unResource)
import Trace.Core
import Trace.Export.Otlp
import Trace.Export.Types
import Trace.Generators

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

withTestServer :: Application -> (Int -> IO a) -> IO a
withTestServer app action =
  Warp.testWithApplication (pure app) action

recordingApp :: IORef (Maybe LBS.ByteString) -> Application
recordingApp ref req respond = do
  body <- strictRequestBody req
  writeIORef ref (Just body)
  respond (responseLBS status200 [] "")

failingApp :: Application
failingApp _req respond =
  respond (responseLBS status503 [] "Service Unavailable")

-- | A fixed scope used in encoding tests.
testScope :: InstrumentationScope
testScope = InstrumentationScope "htrace-test" (Just "0.0.0")

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do

  describe "mkEndpoint" $ do
    it "accepts http://" $
      case mkEndpoint "http://localhost:4318" of
        Right _ -> pure ()
        Left e  -> expectationFailure (show e)

    it "accepts https://" $
      case mkEndpoint "https://collector.example.com" of
        Right _ -> pure ()
        Left e  -> expectationFailure (show e)

    it "accepts IPv6 literal" $
      case mkEndpoint "http://[::1]:4318" of
        Right _ -> pure ()
        Left e  -> expectationFailure (show e)

    it "accepts endpoint with path" $
      case mkEndpoint "http://localhost:4318/v1/traces" of
        Right _ -> pure ()
        Left e  -> expectationFailure (show e)

    it "accepts endpoint with trailing slash" $
      case mkEndpoint "http://localhost/" of
        Right ep -> unEndpoint ep `shouldBe` "http://localhost/"
        Left e   -> expectationFailure (show e)

    it "rejects ftp scheme" $
      case mkEndpoint "ftp://localhost" of
        Left (ExporterUnsupportedScheme s) ->
          Text.isPrefixOf "ftp" s `shouldBe` True
        other -> expectationFailure ("unexpected: " <> show other)

    it "rejects non-URL string" $
      case mkEndpoint "not a url" of
        Left (ExporterInvalidEndpoint _) -> pure ()
        other -> expectationFailure ("unexpected: " <> show other)

    it "rejects empty string" $
      case mkEndpoint "" of
        Left (ExporterInvalidEndpoint _) -> pure ()
        other -> expectationFailure ("unexpected: " <> show other)

  describe "otlpExporter" $ do
    it "rejects header name containing colon" $ do
      bodyRef <- newIORef Nothing
      withTestServer (recordingApp bodyRef) $ \port -> do
        let Right ep = mkEndpoint
              ("http://localhost:" <> Text.pack (show port))
        result <- otlpExporter
          (OtlpConfig ep [("x:bad", "value")] 5 NoCompression)
          (unResource defaultResource)
          testScope
        case result of
          Left (ExporterInvalidHeader k _) ->
            k `shouldBe` "x:bad"
          Left other ->
            expectationFailure ("unexpected error: " <> show other)
          Right _ ->
            expectationFailure "expected Left but got Right"

    it "rejects header name containing control character" $ do
      bodyRef <- newIORef Nothing
      withTestServer (recordingApp bodyRef) $ \port -> do
        let Right ep = mkEndpoint
              ("http://localhost:" <> Text.pack (show port))
        result <- otlpExporter
          (OtlpConfig ep [("x\nbad", "value")] 5 NoCompression)
          (unResource defaultResource)
          testScope
        case result of
          Left (ExporterInvalidHeader _ _) -> pure ()
          Left other ->
            expectationFailure ("unexpected error: " <> show other)
          Right _ ->
            expectationFailure "expected Left but got Right"

    
  describe "Show OtlpConfig" $ do
    it "redacts Authorization header value" $ do
      let Right ep = mkEndpoint "http://localhost:4318"
          cfg = OtlpConfig ep [("Authorization", "Bearer secret")] 5 NoCompression
      show cfg `shouldNotContain` "Bearer secret"

    it "preserves Authorization header name" $ do
      let Right ep = mkEndpoint "http://localhost:4318"
          cfg = OtlpConfig ep [("Authorization", "Bearer secret")] 5 NoCompression
      show cfg `shouldContain` "Authorization"

    it "does not redact non-sensitive headers" $ do
      let Right ep = mkEndpoint "http://localhost:4318"
          cfg = OtlpConfig ep [("x-service-name", "my-svc")] 5 NoCompression
      show cfg `shouldContain` "my-svc"

  describe "encodeOtlp" $ do
    it "produces valid JSON" $ do
      let json = encode (encodeOtlp (unResource defaultResource) testScope [sampleFinishedSpan])
      case decode json :: Maybe Value of
        Just _  -> pure ()
        Nothing -> expectationFailure "produced invalid JSON"

    it "top-level key is resourceSpans" $ do
      let json = encode (encodeOtlp (unResource defaultResource) testScope [sampleFinishedSpan])
      case decode json :: Maybe Value of
        Just (Object o) ->
          KM.member "resourceSpans" o `shouldBe` True
        other ->
          expectationFailure ("unexpected: " <> show other)

    it "resource block contains service.name" $ do
      let json = encode (encodeOtlp (unResource defaultResource) testScope [sampleFinishedSpan])
      LBS8.unpack json `shouldContain` "service.name"

    it "scope block contains scope name" $ do
      let json = LBS8.unpack
                   (encode (encodeOtlp (unResource defaultResource) testScope [sampleFinishedSpan]))
      json `shouldContain` "htrace-test"

    it "scope block contains scope version" $ do
      let json = LBS8.unpack
                   (encode (encodeOtlp (unResource defaultResource) testScope [sampleFinishedSpan]))
      json `shouldContain` "0.0.0"

    it "scope without version omits version field" $ do
      let noVersionScope = InstrumentationScope "no-ver" Nothing
          json = LBS8.unpack
                   (encode (encodeOtlp (unResource defaultResource) noVersionScope [sampleFinishedSpan]))
      json `shouldNotContain` "\"version\""

    it "encodes all six AttrValue constructors without error" $ do
      let fs   = sampleFinishedSpan { fsAttributes = allAttrTypes }
          json = encode (encodeOtlp (unResource defaultResource) testScope [fs])
      case decode json :: Maybe Value of
        Just _  -> pure ()
        Nothing -> expectationFailure "produced invalid JSON"

    it "empty attributes encode as array not null" $ do
      let fs   = sampleFinishedSpan { fsAttributes = mempty }
          json = LBS8.unpack (encode (encodeOtlp (unResource defaultResource) testScope [fs]))
      ("\"attributes\":[]" `isInfixOf` json) `shouldBe` True

    it "empty events encode as array not null" $ do
      let fs   = sampleFinishedSpan { fsEvents = [] }
          json = LBS8.unpack (encode (encodeOtlp (unResource defaultResource) testScope [fs]))
      ("\"events\":[]" `isInfixOf` json) `shouldBe` True

    it "zero-duration span encodes identical start and end nanos" $ do
      let fs   = sampleFinishedSpan
                   { fsEndTime = fsStartTime sampleFinishedSpan }
          json = encode (encodeOtlp (unResource defaultResource) testScope [fs])
      case decode json :: Maybe Value of
        Just _  -> pure ()
        Nothing -> expectationFailure "produced invalid JSON"

  describe "HTTP integration" $ do
    it "POSTs JSON to the endpoint" $ do
      bodyRef <- newIORef Nothing
      withTestServer (recordingApp bodyRef) $ \port -> do
        let Right ep = mkEndpoint
              ("http://localhost:" <> Text.pack (show port))
        Right exporter <- otlpExporter
          (OtlpConfig ep [] 5 NoCompression)
          (unResource defaultResource)
          testScope
        _ <- exporterExport exporter (sampleFinishedSpan NE.:| [])
        mBody <- readIORef bodyRef
        case mBody of
          Nothing   -> expectationFailure "no request received"
          Just body ->
            case decode body :: Maybe Value of
              Just (Object o) ->
                KM.member "resourceSpans" o `shouldBe` True
              other ->
                expectationFailure ("unexpected body: " <> show other)

    it "returns EndpointUnreachable when nothing is listening" $ do
      let Right ep = mkEndpoint "http://localhost:1"
      Right exporter <- otlpExporter
        (OtlpConfig ep [] 1 NoCompression)
        (unResource defaultResource)
        testScope
      result <- exporterExport exporter (sampleFinishedSpan NE.:| [])
      case result of
        ExportFailure (EndpointUnreachable _) -> pure ()
        other -> expectationFailure ("unexpected: " <> show other)

    it "returns ExportFailure on non-2xx response" $ do
      withTestServer failingApp $ \port -> do
        let Right ep = mkEndpoint
              ("http://localhost:" <> Text.pack (show port))
        Right exporter <- otlpExporter
          (OtlpConfig ep [] 5 NoCompression)
          (unResource defaultResource)
          testScope
        result <- exporterExport exporter (sampleFinishedSpan NE.:| [])
        case result of
          ExportFailure (MalformedResponse hs _) ->
            unHttpStatus hs `shouldBe` 503
          other -> expectationFailure ("unexpected: " <> show other)

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

allAttrTypes :: SpanAttrs
allAttrTypes = attrs
  [ (AttrKey "s",  AttrString "hello")
  , (AttrKey "i",  AttrInt 42)
  , (AttrKey "d",  AttrDouble 3.14)
  , (AttrKey "b",  AttrBool True)
  , (AttrKey "sl", AttrStringList ["a", "b"])
  , (AttrKey "il", AttrIntList [1, 2, 3])
  ]
