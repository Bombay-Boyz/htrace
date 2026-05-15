module Trace.Attributes
  ( -- * Attribute keys and values
    AttrKey (..)
  , AttrValue (..)
    -- * Attribute maps
  , SpanAttrs (..)
  , attrs
  , lookupAttr
    -- * Errors
  , MissingAttr (..)
  ) where

import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as Text

-- ---------------------------------------------------------------------------
-- Attribute key
-- ---------------------------------------------------------------------------

-- | An attribute key. Case-sensitive, non-empty by convention.
newtype AttrKey = AttrKey { unAttrKey :: Text }
  deriving stock (Show, Eq, Ord)

instance IsString AttrKey where
  fromString = AttrKey . Text.pack

-- ---------------------------------------------------------------------------
-- Attribute value
-- ---------------------------------------------------------------------------

-- | The set of value types permitted by the OpenTelemetry attribute spec.
data AttrValue
  = AttrString     !Text
  | AttrInt        !Int64
  | AttrDouble     !Double
  | AttrBool       !Bool
  | AttrStringList ![Text]
  | AttrIntList    ![Int64]
  deriving stock (Show, Eq)

-- ---------------------------------------------------------------------------
-- Attribute map
-- ---------------------------------------------------------------------------

-- | An immutable map of span attributes.
newtype SpanAttrs = SpanAttrs { unSpanAttrs :: Map AttrKey AttrValue }
  deriving stock (Show, Eq)

-- | Left-biased merge: keys in the left operand take precedence,
-- following the standard Haskell 'Semigroup' convention where
-- @x '<>' y@ means @x@ wins on collision.
instance Semigroup SpanAttrs where
  SpanAttrs a <> SpanAttrs b = SpanAttrs (Map.union a b)

instance Monoid SpanAttrs where
  mempty = SpanAttrs Map.empty

-- | Construct a 'SpanAttrs' from a list of key-value pairs.
-- Later entries override earlier ones for duplicate keys.
attrs :: [(AttrKey, AttrValue)] -> SpanAttrs
attrs = SpanAttrs . Map.fromList

-- ---------------------------------------------------------------------------
-- Lookup
-- ---------------------------------------------------------------------------

-- | Returned when a required attribute key is absent.
newtype MissingAttr = MissingAttr { missingAttrKey :: AttrKey }
  deriving stock (Show, Eq)

-- | Look up a key. Returns 'Left MissingAttr' if the key is absent.
lookupAttr :: AttrKey -> SpanAttrs -> Either MissingAttr AttrValue
lookupAttr k (SpanAttrs m) =
  maybe (Left (MissingAttr k)) Right (Map.lookup k m)