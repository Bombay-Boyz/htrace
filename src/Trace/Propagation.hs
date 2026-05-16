module Trace.Propagation
  ( -- * Result types
    PropagationResult (..)
  , PropagationError (..)
    -- * Parsing and emission
  , parseTraceparent
  , emitTraceparent
    -- * Header injection and extraction
  , injectHeaders
  , extractContext
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as Base16
import Data.CaseInsensitive qualified as CI
import Data.Char (ord, chr)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import Data.Word (Word8)
import Network.HTTP.Types (Header)

import Trace.Core

-- ---------------------------------------------------------------------------
-- Result types
-- ---------------------------------------------------------------------------

-- | The outcome of attempting to extract a 'SpanContext' from a header.
data PropagationResult
  = PropagationSuccess !SpanContext
    -- ^ A valid context was parsed.
  | PropagationAbsent
    -- ^ No @traceparent@ header was present.
  | PropagationInvalid !PropagationError
    -- ^ A @traceparent@ header was present but could not be parsed.
  deriving stock (Show, Eq)

-- | Reasons a @traceparent@ header can fail to parse.
data PropagationError
  = InvalidVersion  !Text
  | InvalidTraceId  !Text
  | InvalidSpanId   !Text
  | InvalidFlags    !Text
  | MalformedHeader !Text
  deriving stock (Show, Eq)

-- ---------------------------------------------------------------------------
-- Parsing
-- ---------------------------------------------------------------------------

-- | Parse a @traceparent@ header value.
--
-- Accepts version @00@ and any future two-hex-digit version other than @ff@.
-- When a future version is encountered the first four dash-separated fields
-- are extracted and any additional fields are ignored, per W3C Trace Context
-- specification §4.3 (forward compatibility).
--
-- Version @ff@ is permanently reserved by the W3C and is always rejected.
-- Always emits version @00@ via 'emitTraceparent'.
parseTraceparent :: Text -> PropagationResult
parseTraceparent t =
  case Text.splitOn "-" t of
    (v : tid : sid : flgs : _) ->
      if not (validVersion v)
        then PropagationInvalid (InvalidVersion (Text.toLower v))
        else case decodeHex 16 tid of
          Left _      -> PropagationInvalid (InvalidTraceId tid)
          Right tidBs -> case traceIdFromBytes tidBs of
            Left _        -> PropagationInvalid (InvalidTraceId tid)
            Right traceId -> case decodeHex 8 sid of
              Left _      -> PropagationInvalid (InvalidSpanId sid)
              Right sidBs -> case spanIdFromBytes sidBs of
                Left _      -> PropagationInvalid (InvalidSpanId sid)
                Right spanId -> case parseFlags flgs of
                  Nothing -> PropagationInvalid (InvalidFlags flgs)
                  Just f  -> PropagationSuccess
                               (SpanContext traceId spanId Nothing f)
    _ -> PropagationInvalid (MalformedHeader t)
  where
    -- Accept any two lowercase-hex-digit version except the W3C-reserved "ff".
    -- Normalise to lowercase before comparison so "0F" and "0f" are identical.
    validVersion v =
      Text.length v == 2
        && Text.all isHexDigit v
        && Text.toLower v /= "ff"

    parseFlags f
      | Text.length f == 2 && Text.all isHexDigit f =
          Just (TraceFlags (fromIntegral (hexToInt f)))
      | otherwise = Nothing

-- ---------------------------------------------------------------------------
-- Emission
-- ---------------------------------------------------------------------------

-- | Render a 'SpanContext' as a @traceparent@ header value.
-- Always emits version @00@.
emitTraceparent :: SpanContext -> Text
emitTraceparent ctx = Text.intercalate "-"
  [ "00"
  , TE.decodeUtf8 (Base16.encode (unTraceId (scTraceId ctx)))
  , TE.decodeUtf8 (Base16.encode (unSpanId  (scSpanId  ctx)))
  , Text.pack (word8ToHex2 (unTraceFlags (scTraceFlags ctx)))
  ]

-- ---------------------------------------------------------------------------
-- Header injection and extraction
-- ---------------------------------------------------------------------------

traceparentHeader :: CI.CI ByteString
traceparentHeader = CI.mk "traceparent"

-- | Inject a @traceparent@ header derived from the given 'SpanContext'
-- into a header list, replacing any existing @traceparent@ header.
injectHeaders :: SpanContext -> [Header] -> [Header]
injectHeaders ctx hs =
  (traceparentHeader, TE.encodeUtf8 (emitTraceparent ctx))
  : filter ((/= traceparentHeader) . fst) hs

-- | Extract a 'SpanContext' from a header list by looking up @traceparent@.
-- Case-insensitive lookup per the HTTP spec.
extractContext :: [Header] -> PropagationResult
extractContext hs =
  case lookup traceparentHeader hs of
    Nothing -> PropagationAbsent
    Just bs ->
      case TE.decodeUtf8' bs of
        Left  _  -> PropagationInvalid (MalformedHeader "non-utf8 header value")
        Right t  -> parseTraceparent t

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

-- | Decode exactly @expectedBytes@ bytes from a hex-encoded 'Text'.
-- Normalises to lowercase before decoding so that uppercase hex digits
-- (valid per W3C Trace Context §4.3 forward-compatibility rules) are
-- accepted without error (M-6).
decodeHex :: Int -> Text -> Either Text ByteString
decodeHex expectedBytes t
  | Text.length t /= expectedBytes * 2 =
      Left ("expected " <> Text.pack (show (expectedBytes * 2)) <> " hex chars")
  | otherwise =
      case Base16.decode (TE.encodeUtf8 (Text.toLower t)) of
        Right bs -> Right bs
        Left  e  -> Left (Text.pack e)

isHexDigit :: Char -> Bool
isHexDigit c =
  (c >= '0' && c <= '9')
    || (c >= 'a' && c <= 'f')
    || (c >= 'A' && c <= 'F')

hexToInt :: Text -> Int
hexToInt = Text.foldl' (\acc c -> acc * 16 + hexVal c) 0
  where
    hexVal c
      | c >= '0' && c <= '9' = ord c - ord '0'
      | c >= 'a' && c <= 'f' = ord c - ord 'a' + 10
      | c >= 'A' && c <= 'F' = ord c - ord 'A' + 10
      | otherwise            = 0

-- | Render a 'Word8' as exactly two lowercase hex characters.
word8ToHex2 :: Word8 -> String
word8ToHex2 w = [hexChar (w `div` 16), hexChar (w `mod` 16)]
  where
    hexChar :: Word8 -> Char
    hexChar n
      | n < 10    = chr (ord '0' + fromIntegral n)
      | otherwise = chr (ord 'a' + fromIntegral n - 10)
