module Trace.Export.Otlp
  ( -- * Endpoint
    Endpoint
  , unEndpoint
  , mkEndpoint
    -- * Configuration
    -- | Note: only 'NoCompression' is currently supported.
    -- 'GzipCompression' is defined but not exported; it will be added
    -- to the public API once implemented.
  , Compression (..)
  , OtlpConfig (..)
    -- * Constructor
  , otlpExporter
    -- * OTLP encoding (exposed for testing)
  , encodeOtlp
  ) where

import Data.ByteString (ByteString)
import Control.Exception (fromException)
import System.IO.Error   (isDoesNotExistError, ioeGetErrorType)
import GHC.IO.Exception  (IOErrorType (NoSuchThing))
import Data.Aeson (Value (..), object, (.=))
import Data.Aeson.Encoding
  ( Encoding
  , encodingToLazyByteString
  , pairs, pair, list, text, integer, bool, double, null_
  )
import qualified Data.Aeson.Encoding as AE
import Data.ByteString.Base16 qualified as Base16
import Data.CaseInsensitive qualified as CI
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time (NominalDiffTime)
import Network.HTTP.Client
  ( HttpException (..)
  , HttpExceptionContent (..)
  , Manager
  , Request
  , RequestBody (..)
  , httpLbs
  , method
  , newManager
  , parseRequest
  , requestBody
  , requestHeaders
  , responseStatus
  , responseTimeout
  , responseTimeoutMicro
  )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (statusCode)
import Network.URI qualified as URI
import UnliftIO.Exception (SomeException, try)

import Trace.Attributes
import Trace.Core
import Trace.Export.Types

-- ---------------------------------------------------------------------------
-- Endpoint
-- ---------------------------------------------------------------------------

-- | A validated HTTP or HTTPS endpoint URL.
-- Constructor hidden; use 'mkEndpoint'.
newtype Endpoint = Endpoint { unEndpoint :: Text }
  deriving stock (Show, Eq)

-- | Validate and construct an 'Endpoint'.
-- Accepts only @http:@ and @https:@ schemes.
mkEndpoint :: Text -> Either ExporterInitError Endpoint
mkEndpoint t =
  case URI.parseAbsoluteURI (Text.unpack t) of
    Nothing  -> Left (ExporterInvalidEndpoint t)
    Just uri ->
      case URI.uriScheme uri of
        "http:"  -> Right (Endpoint t)
        "https:" -> Right (Endpoint t)
        s        -> Left (ExporterUnsupportedScheme (Text.pack s))

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

-- | Whether to compress the request body.
--
-- Currently only 'NoCompression' is supported and exported.
-- 'GzipCompression' exists internally for future use but is intentionally
-- not exported: exporting an unimplemented constructor would allow
-- type-correct code that always fails at runtime (M-3).
data Compression
  = NoCompression
 deriving stock (Show, Eq)

-- | Configuration for the OTLP/HTTP JSON exporter.
data OtlpConfig = OtlpConfig
  { otlpEndpoint    :: !Endpoint
  , otlpHeaders     :: ![(Text, Text)]
  , otlpTimeout     :: !NominalDiffTime
  , otlpCompression :: !Compression
  } deriving stock (Eq)

instance Show OtlpConfig where
  show cfg =
    "OtlpConfig { endpoint = "   <> show (otlpEndpoint cfg)
    <> ", headers = "            <> show (redactHeaders (otlpHeaders cfg))
    <> ", timeout = "            <> show (otlpTimeout cfg)
    <> ", compression = "        <> show (otlpCompression cfg)
    <> " }"

-- | Redact the values of headers whose names suggest they carry credentials.
redactHeaders :: [(Text, Text)] -> [(Text, Text)]
redactHeaders = map redact
  where
    redact (k, v)
      | isSensitive (Text.toLower k) = (k, "<redacted>")
      | otherwise                    = (k, v)
    isSensitive k = any (`Text.isInfixOf` k)
      [ "authorization", "auth", "api-key", "apikey", "api_key"
      , "token", "secret", "password"
      , "x-honeycomb-team", "dd-api-key"
      ]

-- ---------------------------------------------------------------------------
-- Constructor
-- ---------------------------------------------------------------------------

-- | Build an OTLP/HTTP JSON 'SpanExporter' from the given config,
-- stamping every exported batch with the given 'Resource' and
-- 'InstrumentationScope'.
-- Returns 'Left' if any header name is invalid.
otlpExporter
  :: OtlpConfig
  -> SpanAttrs
  -> InstrumentationScope
  -> IO (Either ExporterInitError SpanExporter)
otlpExporter cfg resourceAttrs scope =
  case validateHeaders (otlpHeaders cfg) of
        Left e   -> pure (Left e)
        Right hs -> do
          mgr  <- newManager tlsManagerSettings
          req0 <- parseRequest (Text.unpack (unEndpoint (otlpEndpoint cfg)))
          let req = req0
                { method         = "POST"
                , requestHeaders =
                    (CI.mk "content-type", "application/json") : hs
                , responseTimeout =
                    responseTimeoutMicro
                      (round (otlpTimeout cfg * 1_000_000))
                }
          pure $ Right $ SpanExporter
            { exporterExport   = doExport mgr req resourceAttrs scope
            , exporterFlush    = pure (Right ())
            , exporterShutdown = pure ()
            }

-- | Validate header names: must not contain control characters or colons.
validateHeaders
  :: [(Text, Text)]
  -> Either ExporterInitError [(CI.CI ByteString, ByteString)]
validateHeaders = traverse validateOne
  where
    validateOne (k, v)
      | Text.all validNameChar k =
          Right (CI.mk (TE.encodeUtf8 k), TE.encodeUtf8 v)
      | otherwise =
          Left (ExporterInvalidHeader k "contains invalid characters")
    validNameChar c = c > ' ' && c /= ':'

-- | Map an HTTP exception to a structured 'NetworkFailure' category.
classifyException :: SomeException -> NetworkFailure
classifyException e =
  case fromException e of
    Just (HttpExceptionRequest _ content) ->
      case content of
        ConnectionFailure inner   -> classifyConnectionFailure inner
        ConnectionTimeout         -> RequestTimedOut
        ResponseTimeout           -> RequestTimedOut
        _                         -> OtherNetworkError
    Just (InvalidUrlException _ _) -> OtherNetworkError
    Nothing                        -> OtherNetworkError

-- | Inspect the underlying 'IOError' from a connection failure to distinguish
-- DNS errors (no such host) from refused connections and other failures.
-- Uses structured IOError predicates from 'System.IO.Error' rather than
-- fragile string matching on 'show' output, which is implementation-defined
-- and can change between 'network' package versions or platforms.
classifyConnectionFailure :: SomeException -> NetworkFailure
classifyConnectionFailure inner =
  case fromException inner of
    Just ioErr
      | isDoesNotExistError ioErr -> DnsResolutionFailed
    _                             -> ConnectionRefused
-- ---------------------------------------------------------------------------
-- HTTP export
-- ---------------------------------------------------------------------------

doExport
  :: Manager
  -> Request
  -> SpanAttrs
  -> InstrumentationScope
  -> NonEmpty FinishedSpan
  -> IO ExportResult
-- NEW
doExport mgr req resourceAttrs scope spans = do
  let body = encodingToLazyByteString
               (encodeOtlpE resourceAttrs scope (NE.toList spans))
      req' = req { requestBody = RequestBodyLBS body }

  result <- try (httpLbs req' mgr)
  case result of
    Left (e :: SomeException) ->
      pure (ExportFailure (NetworkError (classifyException e) (Text.pack (show e))))
    Right resp ->
      let st = statusCode (responseStatus resp)
      in case mkHttpStatus st of
           Just hs
             | st >= 200 && st < 300 ->
                 pure (ExportSuccess (NE.length spans))
             | otherwise ->
                 pure (ExportFailure
                   (MalformedResponse hs (Text.pack (show st))))
           Nothing ->
             pure (ExportFailure
               (MalformedResponse
                 (HttpStatus 0)
                 ("non-RFC status: " <> Text.pack (show st))))

-- ---------------------------------------------------------------------------
-- OTLP JSON encoding
-- ---------------------------------------------------------------------------

-- | Encode a list of 'FinishedSpan's as an OTLP @ExportTraceServiceRequest@,
-- embedding the given 'Resource' and 'InstrumentationScope' in the output.
-- Every backend (Jaeger, Tempo, Datadog) uses these fields to identify the
-- service and SDK that produced the spans.
encodeOtlp :: SpanAttrs -> InstrumentationScope -> [FinishedSpan] -> Value
encodeOtlp resourceAttrs scope spans = object
  [ "resourceSpans" .= [ object
      [ "resource"   .= encodeResource resourceAttrs
      , "scopeSpans" .= [ object
          [ "scope"  .= encodeScope scope
          , "spans"  .= map encodeSpan spans
          ]
        ]
      ]
    ]
  ]

encodeResource :: SpanAttrs -> Value
encodeResource r = object
  [ "attributes" .= map encodeKv (Map.toList (unSpanAttrs r)) ]

encodeScope :: InstrumentationScope -> Value
encodeScope s = object $
  [ "name" .= scopeName s ]
  ++ maybe [] (\v -> ["version" .= v]) (scopeVersion s)

encodeSpan :: FinishedSpan -> Value
encodeSpan fs = object
  [ "traceId"           .= hexText (unTraceId (scTraceId (fsContext fs)))
  , "spanId"            .= hexText (unSpanId  (scSpanId  (fsContext fs)))
  , "parentSpanId"      .= maybe "" (hexText . unSpanId)
                             (scParentId (fsContext fs))
  , "name"              .= unSpanName (fsName fs)
  , "kind"              .= encodeKind (fsKind fs)
  , "startTimeUnixNano" .= toUnixNano (fsStartTime fs)
  , "endTimeUnixNano"   .= toUnixNano (fsEndTime   fs)
  , "attributes"        .= map encodeKv
                             (Map.toList (unSpanAttrs (fsAttributes fs)))
  , "events"            .= map encodeEvent (fsEvents fs)
  , "status"            .= encodeStatus (fsStatus fs)
  ]
  where
    hexText bs = TE.decodeUtf8 (Base16.encode bs)
    toUnixNano t =
      floor (utcTimeToPOSIXSeconds t * 1_000_000_000) :: Integer

encodeKind :: SpanKind -> Int
encodeKind = \case
  Internal -> 1
  Server   -> 2
  Client   -> 3
  Producer -> 4
  Consumer -> 5

encodeKv :: (AttrKey, AttrValue) -> Value
encodeKv (AttrKey k, v) = object
  [ "key"   .= k
  , "value" .= encodeAttrValue v
  ]

encodeAttrValue :: AttrValue -> Value
encodeAttrValue = \case
  AttrString t ->
    object ["stringValue" .= t]
  AttrInt n ->
    object ["intValue" .= n]
  AttrDouble d ->
    object ["doubleValue" .= d]
  AttrBool b ->
    object ["boolValue" .= b]
  AttrStringList ts ->
    object ["arrayValue" .= object
      ["values" .= map (\t -> object ["stringValue" .= t]) ts]]
  AttrIntList ns ->
    object ["arrayValue" .= object
      ["values" .= map (\n -> object ["intValue" .= n]) ns]]

encodeEvent :: SpanEvent -> Value
encodeEvent ev = object
  [ "name"         .= eventName ev
  , "timeUnixNano" .= toUnixNano (eventTime ev)
  , "attributes"   .= map encodeKv
                        (Map.toList (unSpanAttrs (eventAttributes ev)))
  ]
  where
    toUnixNano t =
      floor (utcTimeToPOSIXSeconds t * 1_000_000_000) :: Integer

encodeStatus :: SpanStatus -> Value
encodeStatus = \case
  StatusUnset    -> object ["code" .= (0 :: Int)]
  StatusOk       -> object ["code" .= (1 :: Int)]
  StatusError em -> object ["code" .= (2 :: Int), "message" .= unErrorMessage em]

-- NEW — added after encodeStatus

-- | Streaming encoder: same wire format as 'encodeOtlp' but avoids
-- building an intermediate 'Value' AST.
encodeOtlpE
  :: SpanAttrs -> InstrumentationScope -> [FinishedSpan] -> Encoding
encodeOtlpE resourceAttrs scope spans =
  pairs (pair "resourceSpans" (list id [resourceSpanE]))
  where
    resourceSpanE =
      pairs
        (  pair "resource"   (encodeResourceE resourceAttrs)
        <> pair "scopeSpans" (list id [scopeSpanE])
        )
    scopeSpanE =
      pairs
        (  pair "scope" (encodeScopeE scope)
        <> pair "spans" (list encodeSpanE spans)
        )

encodeResourceE :: SpanAttrs -> Encoding
encodeResourceE r =
  pairs (pair "attributes"
    (list encodeKvE (Map.toList (unSpanAttrs r))))

encodeScopeE :: InstrumentationScope -> Encoding
encodeScopeE s =
  pairs
    (  pair "name" (text (scopeName s))
    <> maybe mempty (\v -> pair "version" (text v)) (scopeVersion s)
    )

encodeSpanE :: FinishedSpan -> Encoding
encodeSpanE fs =
  pairs
    (  pair "traceId"           (text (hexText (unTraceId (scTraceId (fsContext fs)))))
    <> pair "spanId"            (text (hexText (unSpanId  (scSpanId  (fsContext fs)))))
    <> pair "parentSpanId"      (text (maybe "" (hexText . unSpanId)
                                         (scParentId (fsContext fs))))
    <> pair "name"              (text (unSpanName (fsName fs)))
    <> pair "kind"              (integer (fromIntegral (encodeKind (fsKind fs))))
    <> pair "startTimeUnixNano" (integer (toUnixNano (fsStartTime fs)))
    <> pair "endTimeUnixNano"   (integer (toUnixNano (fsEndTime   fs)))
    <> pair "attributes"        (list encodeKvE
                                   (Map.toList (unSpanAttrs (fsAttributes fs))))
    <> pair "events"            (list encodeEventE (fsEvents fs))
    <> pair "status"            (encodeStatusE (fsStatus fs))
    )
  where
    hexText bs = TE.decodeUtf8 (Base16.encode bs)
    toUnixNano t =
      floor (utcTimeToPOSIXSeconds t * 1_000_000_000) :: Integer

encodeKvE :: (AttrKey, AttrValue) -> Encoding
encodeKvE (AttrKey k, v) =
  pairs
    (  pair "key"   (text k)
    <> pair "value" (encodeAttrValueE v)
    )

encodeAttrValueE :: AttrValue -> Encoding
encodeAttrValueE = \case
  AttrString t    -> pairs (pair "stringValue" (text t))
  AttrInt n       -> pairs (pair "intValue"    (integer (fromIntegral n)))
  AttrDouble d    -> pairs (pair "doubleValue" (double d))
  AttrBool b      -> pairs (pair "boolValue"   (bool b))
  AttrStringList ts ->
    pairs (pair "arrayValue"
      (pairs (pair "values"
        (list (\t -> pairs (pair "stringValue" (text t))) ts))))
  AttrIntList ns ->
    pairs (pair "arrayValue"
      (pairs (pair "values"
        (list (\n -> pairs (pair "intValue" (integer (fromIntegral n)))) ns))))

encodeEventE :: SpanEvent -> Encoding
encodeEventE ev =
  pairs
    (  pair "name"         (text (eventName ev))
    <> pair "timeUnixNano" (integer (toUnixNano (eventTime ev)))
    <> pair "attributes"   (list encodeKvE
                              (Map.toList (unSpanAttrs (eventAttributes ev))))
    )
  where
    toUnixNano t =
      floor (utcTimeToPOSIXSeconds t * 1_000_000_000) :: Integer

encodeStatusE :: SpanStatus -> Encoding
encodeStatusE = \case
  StatusUnset    -> pairs (pair "code" (integer 0))
  StatusOk       -> pairs (pair "code" (integer 1))
  StatusError em -> pairs (pair "code" (integer 2) <> pair "message" (text (unErrorMessage em)))