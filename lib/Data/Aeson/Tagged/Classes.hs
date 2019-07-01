{-# LANGUAGE NoImplicitPrelude #-}

-- | Tagged analogs of 'A.FromJSON' and 'A.ToJSON' from Aeson.
module Data.Aeson.Tagged.Classes
(
    -- * Classes
    FromJSON(..),
    ToJSON(..),
    KeyValue(..),

    -- * Combinators
    (.:), (.:?), (.:!),
)
where

import BasePrelude
import qualified Data.Vector as V
import Data.Text
import qualified Data.HashMap.Strict as HM

import qualified Data.Aeson.Types    as A
import qualified Data.Aeson.Internal as A
import qualified Data.Aeson.Encoding          as E
import qualified Data.Aeson.Encoding.Internal as E

import Data.Aeson.Tagged.Wrapped

----------------------------------------------------------------------------
-- FromJSON
----------------------------------------------------------------------------

-- | tagged-aeson does not provide any 'FromJSON' instances. You have
-- several options for writing them:
--
-- __Lift instances from Aeson:__
--
-- @
-- 'using' \@'Aeson' (o '.:' "name")
-- @
--
-- @
-- instance FromJSON Tag Text where
--     'parseJSON' = 'using' \@'Aeson' 'parseJSON'
-- @
--
-- Note that you should not do this with container types (e.g. lists),
-- because then inner elements would also be parsed according to the Aeson
-- instance and not your tagged-aeson instance.
--
-- __Use a decoding helper:__
--
-- @
-- accounts \<- 'parseList' =\<\< (o '.:' "accounts")
-- @
--
-- @
-- instance FromJSON Tag a => FromJSON Tag [a] where
--     'parseJSON' = 'parseList'
-- @
--
-- __Derive instances:__
--
-- @
-- 'Data.Aeson.Tagged.TH.deriveJSON' [t|Tag|] defaultOptions ''Type
-- @
class FromJSON (tag :: k) a where
    parseJSON :: Value any -> Parser tag a

    parseJSONList :: Value any -> Parser tag [a]
    parseJSONList = withArray "[]" $
        zipWithM (parseIndexedJSON parseJSON) [0..] . V.toList

instance FromJSON tag (Value any) where
    parseJSON = pure . coerce
    {-# INLINE parseJSON #-}

----------------------------------------------------------------------------
-- ToJSON
----------------------------------------------------------------------------

class ToJSON (tag :: k) a where
    toJSON :: a -> Value tag

    toEncoding :: a -> Encoding tag
    toEncoding = coerce E.value . toJSON @tag
    {-# INLINE toEncoding #-}

    toJSONList :: [a] -> Value tag
    toJSONList = coerce (A.listValue @a) (toJSON @tag @a)
    {-# INLINE toJSONList #-}

    toEncodingList :: [a] -> Encoding tag
    toEncodingList = coerce (A.listEncoding @a) (toEncoding @tag @a)
    {-# INLINE toEncodingList #-}

instance ToJSON tag (Value any) where
    toJSON = coerce
    {-# INLINE toJSON #-}
    toEncoding = coerce E.value
    {-# INLINE toEncoding #-}

----------------------------------------------------------------------------
-- KeyValue
----------------------------------------------------------------------------

-- | A key-value pair for encoding a JSON object.
class KeyValue (tag :: k) kv | kv -> tag where
    (.=) :: ToJSON tag v => Text -> v -> kv
    infixr 8 .=

instance KeyValue tag (Series tag) where
    name .= value =
        coerce E.pair
            name
            (toEncoding @tag value)
    {-# INLINE (.=) #-}

instance KeyValue tag (Pair tag) where
    name .= value = (name, toJSON value)
    {-# INLINE (.=) #-}

-- | Constructs a singleton 'HM.HashMap'. For calling functions that
--   demand an 'Object' for constructing objects. To be used in
--   conjunction with 'mconcat'. Prefer to use 'object' where possible.
instance KeyValue tag (Object tag) where
    name .= value = HM.singleton name (toJSON value)
    {-# INLINE (.=) #-}

----------------------------------------------------------------------------
-- Combinators
----------------------------------------------------------------------------

-- | Retrieve the value associated with the given key of an 'Object'.
-- The result is 'empty' if the key is not present or the value cannot
-- be converted to the desired type.
--
-- This accessor is appropriate if the key and value /must/ be present
-- in an object for it to be valid.  If the key and value are
-- optional, use '.:?' instead.
(.:) :: forall tag a any. (FromJSON tag a)
     => Object any -> Text -> Parser tag a
(.:) obj key = case HM.lookup key obj of
    Nothing -> fail $ "key " ++ show key ++ " not present"
    Just v  -> parseJSON v <?> A.Key key
{-# INLINE (.:) #-}

-- | Retrieve the value associated with the given key of an 'Object'. The
-- result is 'Nothing' if the key is not present or if its value is @null@,
-- or 'empty' if the value cannot be converted to the desired type.
--
-- This accessor is most useful if the key and value can be absent
-- from an object without affecting its validity.  If the key and
-- value are mandatory, use '.:' instead.
(.:?) :: forall tag a any. (FromJSON tag a)
      => Object any -> Text -> Parser tag (Maybe a)
(.:?) obj key = case HM.lookup key obj of
    Nothing -> pure Nothing
    Just Null -> pure Nothing
    Just v -> Just <$> parseJSON v <?> A.Key key
{-# INLINE (.:?) #-}

-- | Retrieve the value associated with the given key of an 'Object'.
-- The result is 'Nothing' if the key is not present or 'empty' if the
-- value cannot be converted to the desired type.
--
-- This differs from '.:?' by attempting to parse @null@ the same as any
-- other JSON value, instead of interpreting it as 'Nothing'.
(.:!) :: forall tag a any. (FromJSON tag a)
      => Object any -> Text -> Parser tag (Maybe a)
(.:!) obj key = case HM.lookup key obj of
    Nothing -> pure Nothing
    Just v  -> Just <$> parseJSON v <?> A.Key key
{-# INLINE (.:!) #-}
