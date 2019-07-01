{-# LANGUAGE NoImplicitPrelude #-}

module Data.Aeson.Tagged.Types
(
    -- * Interop between aeson and tagged-aeson
    Aeson,
    TaggedAeson(..), fromTaggedAeson,

    -- * Lifting instances from Aeson
    WithAeson(..),
    WithAeson1(..),
)
where

import BasePrelude
import qualified Data.Set as S
import qualified Data.HashSet as HS
import Data.Hashable (Hashable)
import qualified Data.Aeson as A
import qualified Data.Aeson.Encoding.Internal as E

import Data.Aeson.Tagged.Wrapped
import Data.Aeson.Tagged.Classes
import Data.Aeson.Tagged.Explicit

----------------------------------------------------------------------------
-- Interop between Aeson and tagged-aeson
----------------------------------------------------------------------------

-- | The tag for original Aeson instances. You can use @'parseJSON'
-- \@'Aeson'@ and @'toJSON' \@'Aeson'@ to get Aeson's parsing behavior.
data Aeson

instance A.FromJSON a => FromJSON Aeson a where
    parseJSON = coerce (A.parseJSON @a)
    parseJSONList = coerce (A.parseJSONList @a)

instance A.ToJSON a => ToJSON Aeson a where
    toJSON = coerce (A.toJSON @a)
    toEncoding = coerce (A.toEncoding @a)
    toJSONList = coerce (A.toJSONList @a)
    toEncodingList = coerce (A.toEncodingList @a)

-- | A newtype wrapper to use tagged-aeson instances with functions from
-- Aeson (or @yaml@).
newtype TaggedAeson (tag :: k) a = TaggedAeson a
    deriving (Eq, Ord, Show)

fromTaggedAeson :: forall tag a. TaggedAeson tag a -> a
fromTaggedAeson = coerce

-- | Turns a tagged-aeson instance into an Aeson instance.
instance FromJSON tag a => A.FromJSON (TaggedAeson tag a) where
    parseJSON = coerce @(Value tag -> Parser tag a) parseJSON

    -- TODO: I'm worried that there's too much jumping between FromJSON and
    -- A.FromJSON instances when parsing e.g. @TaggedAeson tag [Foo]@
    parseJSONList = coerce @(Value tag -> Parser tag [a]) parseJSONList

-- | Turns a tagged-aeson instance into an Aeson instance.
instance ToJSON tag a => A.ToJSON (TaggedAeson tag a) where
    toJSON = coerce @(a -> Value tag) toJSON
    toJSONList = coerce @([a] -> Value tag) toJSONList
    toEncoding = coerce @(a -> Encoding tag) toEncoding
    toEncodingList = coerce @([a] -> Encoding tag) toEncodingList

----------------------------------------------------------------------------
-- Lifting instances from Aeson
----------------------------------------------------------------------------

-- |
-- @
-- deriving via WithAeson Foo instance FromJSON Tag Foo
-- @
newtype WithAeson a = WithAeson a

instance A.FromJSON a => FromJSON tag (WithAeson a) where
    parseJSON = coerce (A.parseJSON @a)
    parseJSONList = coerce (A.parseJSONList @a)

instance A.ToJSON a => ToJSON tag (WithAeson a) where
    toJSON = coerce (A.toJSON @a)
    toJSONList = coerce (A.toJSONList @a)
    toEncoding = coerce (A.toEncoding @a)
    toEncodingList = coerce (A.toEncodingList @a)

newtype WithAeson1 f a = WithAeson1 (f a)

instance (FromJSON tag a, A.FromJSON1 f) => FromJSON tag (WithAeson1 f a) where
    parseJSON =
        coerce (A.liftParseJSON @f @a)
            (parseJSON @tag @a)
            (parseJSONList @tag @a)

    parseJSONList =
        coerce (A.liftParseJSONList @f @a)
            (parseJSON @tag @a)
            (parseJSONList @tag @a)

instance (ToJSON tag a, A.ToJSON1 f) => ToJSON tag (WithAeson1 f a) where
    toJSON =
        coerce (A.liftToJSON @f @a)
            (toJSON @tag @a)
            (toJSONList @tag @a)

    toJSONList =
        coerce (A.liftToJSONList @f @a)
            (toJSON @tag @a)
            (toJSONList @tag @a)

    toEncoding =
        coerce (A.liftToEncoding @f @a)
            (toEncoding @tag @a)
            (toEncodingList @tag @a)

    toEncodingList =
        coerce (A.liftToEncodingList @f @a)
            (toEncoding @tag @a)
            (toEncodingList @tag @a)

instance {-# OVERLAPPING #-} (FromJSON tag a, Ord a) =>
         FromJSON tag (WithAeson1 S.Set a) where
    parseJSON = coerce (parseSet @tag)

instance {-# OVERLAPPING #-} (ToJSON tag a, Ord a) =>
         ToJSON tag (WithAeson1 S.Set a) where
    toJSON = coerce (setToJSON @tag)
    toEncoding = coerce (setToEncoding @tag)

instance {-# OVERLAPPING #-} (FromJSON tag a, Eq a, Hashable a) =>
         FromJSON tag (WithAeson1 HS.HashSet a) where
    parseJSON = coerce (parseHashSet @tag)

instance {-# OVERLAPPING #-} (ToJSON tag a, Eq a, Hashable a) =>
         ToJSON tag (WithAeson1 HS.HashSet a) where
    toJSON = coerce (hashSetToJSON @tag)
    toEncoding = coerce (hashSetToEncoding @tag)

-- TODO: PrimArray, Storable.Vector, Primitive.Vector, Unboxed.Vector
-- TODO: do we want to handle Ratio?
