{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}


module Data.Aeson.Tagged
(
    -- * Classes
    -- $classes
    FromJSON(..),
    ToJSON(..),

    -- * Interop between aeson and tagged-aeson
    Aeson,
    TaggedAeson(..), fromTaggedAeson,

    -- * Defining instances
    -- $generic
    deriveJSON,
    deriveToJSON,
    deriveFromJSON,
    -- ** Internals
    addTag,

    -- * By
    untag,
    retag,
    by,

    -- * Alternative
    using,

    -- * Parsing combinators
    (.:), (.:?), (.:!),
    withObject, withText, withArray, withScientific, withBool,

    -- * Encoding combinators
    KeyValue(..),
    object,

    -- * Internals
    Parser(..),
    Value(..),
    Encoding(..),
)
where


import BasePrelude
import Data.Text (Text)
import Data.Generics.Uniplate.Data (transformBi)
import Language.Haskell.TH
import Data.Scientific (Scientific)
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)

-- aeson
import qualified Data.Aeson          as A
import qualified Data.Aeson.Types    as A
import qualified Data.Aeson.TH       as A
import qualified Data.Aeson.Internal as A
import qualified Data.Aeson.Encoding as E

----------------------------------------------------------------------------
-- Classes
----------------------------------------------------------------------------

-- $classes
--
-- This are tagged analogs of 'A.FromJSON' and 'A.ToJSON' from Aeson. You
-- can write your own instances of these classes, or you can use
-- 'deriveJSON' to autoderive them. Note that generic instances don't work.

class FromJSON (tag :: k) a where
    parseJSON :: Value any -> Parser tag a

    parseJSONList :: Value any -> Parser tag [a]
    parseJSONList =
      withArray "[]" $ \a ->
        zipWithM (parseIndexedJSON parseJSON) [0..] $
        V.toList a

class ToJSON (tag :: k) a where
    toJSON :: a -> Value tag

    toEncoding :: a -> Encoding tag
    toEncoding = coerce E.value . toJSON @tag
    {-# INLINE toEncoding #-}

    toJSONList :: [a] -> Value tag
    toJSONList =
        (coerce @((a -> A.Value) -> [a] -> A.Value)
                @((a -> Value tag) -> [a] -> Value tag)
         A.listValue)
        (toJSON @tag)
    {-# INLINE toJSONList #-}

    toEncodingList :: [a] -> Encoding tag
    toEncodingList =
        (coerce @((a -> A.Encoding) -> [a] -> A.Encoding)
                @((a -> Encoding tag) -> [a] -> Encoding tag)
         A.listEncoding)
        (toEncoding @tag)
    {-# INLINE toEncodingList #-}

----------------------------------------------------------------------------
-- Interop between aeson and tagged-aeson
----------------------------------------------------------------------------

-- | The tag for original @aeson@ instances. You can use @parseJSON \@Aeson@
-- and @toJSON \@Aeson@ to get aeson's parsing behavior.
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
-- @aeson@ (or @yaml@).
newtype TaggedAeson (tag :: k) a = TaggedAeson a
    deriving (Eq, Ord, Show)

fromTaggedAeson :: forall tag a. TaggedAeson tag a -> a
fromTaggedAeson = coerce

instance FromJSON tag a => A.FromJSON (TaggedAeson tag a) where
    parseJSON = coerce @(Value tag -> Parser tag a) parseJSON

    -- TODO: I'm worried that there's too much jumping between FromJSON and
    -- A.FromJSON instances when parsing e.g. @TaggedAeson tag [Foo]@
    parseJSONList = coerce @(Value tag -> Parser tag [a]) parseJSONList

instance ToJSON tag a => A.ToJSON (TaggedAeson tag a) where
    toJSON = coerce @(a -> Value tag) toJSON
    toJSONList = coerce @([a] -> Value tag) toJSONList
    toEncoding = coerce @(a -> Encoding tag) toEncoding
    toEncodingList = coerce @([a] -> Encoding tag) toEncodingList

----------------------------------------------------------------------------
-- Defining instances
----------------------------------------------------------------------------

-- $generic
--
-- We do not support generic instances. Instead of writing
--
-- @
-- instance FromJSON Tag Type
-- instance ToJSON Tag Type
-- @
--
-- you need to use 'deriveJSON':
--
-- @
-- deriveJSON ''Tag defaultOptions ''Type
-- @
--
-- You can also use 'deriveFromJSON' and 'deriveToJSON' to get just one
-- instance or the other.

deriveJSON
    :: Name        -- ^ Name of the tag to use for the instances
    -> A.Options   -- ^ Encoding options
    -> Name        -- ^ Name of the type for which to generate
                   --    'ToJSON' and 'FromJSON' instances
    -> Q [Dec]
deriveJSON tag opts name = addTag tag <$> A.deriveJSON opts name

deriveFromJSON
    :: Name        -- ^ Name of the tag to use for the instance
    -> A.Options   -- ^ Encoding options
    -> Name        -- ^ Name of the type for which to generate
                   --    a 'FromJSON' instance
    -> Q [Dec]
deriveFromJSON tag opts name = addTag tag <$> A.deriveFromJSON opts name

deriveToJSON
    :: Name        -- ^ Name of the tag to use for the instance
    -> A.Options   -- ^ Encoding options
    -> Name        -- ^ Name of the type for which to generate
                   --    a 'ToJSON' instance
    -> Q [Dec]
deriveToJSON tag opts name = addTag tag <$> A.deriveToJSON opts name

-- | This function rewrites instances generated by Aeson (by replacing
-- references to Aeson's classes and methods to our own classes and
-- methods).
--
-- The tag must be a name of a type.
addTag :: Name -> [Dec] -> [Dec]
addTag tag = transformBi rewriteExp . transformBi rewriteType
  where
    rewriteType a = case lookup a types of
        Just taggedA -> AppT taggedA (ConT tag)
        Nothing -> a
    types =
        [ (ConT ''A.ToJSON  , ConT ''ToJSON)
        , (ConT ''A.FromJSON, ConT ''FromJSON) ]

    rewriteExp a = case lookup a exps of
        Just taggedA -> AppTypeE taggedA (ConT tag)
        Nothing -> a
    exps =
        [ (VarE 'A.toJSON        , VarE 'toJSON)
        , (VarE 'A.toJSONList    , VarE 'toJSONList)
        , (VarE 'A.toEncoding    , VarE 'toEncoding)
        , (VarE 'A.toEncodingList, VarE 'toEncodingList)
        , (VarE 'A.parseJSON     , VarE 'parseJSON)
        , (VarE 'A.parseJSONList , VarE 'parseJSONList) ]

{-
keyValuePairWith
(.=)
parseUntaggedValue
parseTaggedObject
parseValue
consToValue
toPair
sumToValue

If the result has any FromJSON/ToJSON constraints from Aeson, it will not compile

todo: check what the different encoding of 'String' will change (will it change tags? I guess it shouldn't)
-}

----------------------------------------------------------------------------
-- By
----------------------------------------------------------------------------

class Tag (tag :: k1) a where
    type Untagged (tag :: k1) a
    type Retagged (tag :: k1) (tag' :: k2) a

    untag :: a -> Untagged tag a
    default untag :: Coercible a (Untagged tag a) => a -> Untagged tag a
    untag = coerce
    {-# INLINE untag #-}

    retag' :: a -> Retagged tag tag' a
    default retag' :: Coercible a (Retagged tag tag' a)
                   => a -> Retagged tag tag' a
    retag' = coerce
    {-# INLINE retag' #-}

    by :: a -> a
    by = id
    {-# INLINE by #-}

instance Tag tag (TaggedAeson tag a) where
    type Untagged tag (TaggedAeson tag a) = a
    type Retagged tag tag' (TaggedAeson tag a) = TaggedAeson tag' a

instance Tag tag (Parser tag a) where
    type Untagged tag (Parser tag a) = A.Parser a
    type Retagged tag tag' (Parser tag a) = Parser tag' a

instance Tag tag (Value tag) where
    type Untagged tag (Value tag) = A.Value
    type Retagged tag tag' (Value tag) = Value tag'

instance Tag tag (Object tag) where
    type Untagged tag (Object tag) = A.Object
    type Retagged tag tag' (Object tag) = Object tag'

instance Tag tag (Encoding tag) where
    type Untagged tag (Encoding tag) = A.Encoding
    type Retagged tag tag' (Encoding tag) = Encoding tag'

instance Tag tag (Series tag) where
    type Untagged tag (Series tag) = A.Series
    type Retagged tag tag' (Series tag) = Series tag'

instance Tag tag (Pair tag) where
    type Untagged tag (Pair tag) = A.Pair
    type Retagged tag tag' (Pair tag) = Pair tag'

-- TODO: can this be moved into the class?
retag :: forall tag tag' a. Tag tag a => a -> Retagged tag tag' a
retag = retag' @tag @a @tag'

class Using tag tag' a a' | a -> tag, a tag' -> a', a' -> tag', a' tag -> a where
    using :: a -> a'
    default using :: Coercible a a' => a -> a'
    using = coerce
    {-# INLINE using #-}

instance Using tag tag' (Parser tag a) (Parser tag' a)
instance Using tag tag' (Value tag) (Value tag')
instance Using tag tag' (Object tag) (Object tag')
instance Using tag tag' (Encoding tag) (Encoding tag')
instance Using tag tag' (Series tag) (Series tag')
instance Using tag tag' (Pair tag) (Pair tag')

instance Using tag tag' (x -> Parser tag a) (x -> Parser tag' a)
instance Using tag tag' (x -> Value tag) (x -> Value tag')
instance Using tag tag' (x -> Object tag) (x -> Object tag')
instance Using tag tag' (x -> Encoding tag) (x -> Encoding tag')
instance Using tag tag' (x -> Series tag) (x -> Series tag')
instance Using tag tag' (x -> Pair tag) (x -> Pair tag')

----------------------------------------------------------------------------
-- Reimplementations
----------------------------------------------------------------------------

newtype Parser (tag :: k) a = Parser (A.Parser a)
    deriving newtype (Functor, Applicative, Alternative, Monad,
                      MonadFail, MonadPlus, Semigroup, Monoid)

newtype Value (tag :: k) = Value A.Value
    deriving newtype (Eq, Read, Show, IsString, NFData, Hashable)
    -- TODO KeyValue instances
    -- TODO FromJSON, ToJSON instances
    -- TODO Generic, Lift, Data instances

newtype Encoding (tag :: k) = Encoding A.Encoding
    deriving newtype (Eq, Ord, Show)

newtype Series (tag :: k) = Series A.Series
    deriving newtype (Semigroup, Monoid)

type Pair tag = (Text, Value tag)

type Object tag = HM.HashMap Text (Value tag)

type Array tag = V.Vector (Value tag)

----------------------------------------------------------------------------
-- Value instances
----------------------------------------------------------------------------

instance FromJSON tag (Value any) where
    parseJSON = pure . coerce
    {-# INLINE parseJSON #-}

instance ToJSON tag (Value any) where
    toJSON = coerce
    {-# INLINE toJSON #-}
    toEncoding = coerce E.value
    {-# INLINE toEncoding #-}

instance A.FromJSON (Value tag) where
    parseJSON = pure . coerce
    {-# INLINE parseJSON #-}

instance A.ToJSON (Value tag) where
    toJSON = coerce
    {-# INLINE toJSON #-}
    toEncoding = coerce E.value
    {-# INLINE toEncoding #-}

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
(.:) = coerce @(A.Object -> Text -> A.Parser (TaggedAeson tag a)) (A..:)
{-# INLINE (.:) #-}

-- | Retrieve the value associated with the given key of an 'Object'. The
-- result is 'Nothing' if the key is not present or if its value is 'Null',
-- or 'empty' if the value cannot be converted to the desired type.
--
-- This accessor is most useful if the key and value can be absent
-- from an object without affecting its validity.  If the key and
-- value are mandatory, use '.:' instead.
(.:?) :: forall tag a any. (FromJSON tag a)
      => Object any -> Text -> Parser tag (Maybe a)
(.:?) = coerce @(A.Object -> Text -> A.Parser (Maybe (TaggedAeson tag a))) (A..:?)
{-# INLINE (.:?) #-}

-- | Retrieve the value associated with the given key of an 'Object'.
-- The result is 'Nothing' if the key is not present or 'empty' if the
-- value cannot be converted to the desired type.
--
-- This differs from '.:?' by attempting to parse 'Null' the same as any
-- other JSON value, instead of interpreting it as 'Nothing'.
(.:!) :: forall tag a any. (FromJSON tag a)
      => Object any -> Text -> Parser tag (Maybe a)
(.:!) = coerce @(A.Object -> Text -> A.Parser (Maybe (TaggedAeson tag a))) (A..:!)
{-# INLINE (.:!) #-}

withObject
    :: forall tag a any
     . String
    -> (Object any -> Parser tag a)
    -> Value any
    -> Parser tag a
withObject =
    coerce @(String -> (A.Object -> A.Parser a) -> A.Value -> A.Parser a)
    A.withObject

withText
    :: forall tag a any
     . String
    -> (Text -> Parser tag a)
    -> Value any
    -> Parser tag a
withText =
    coerce @(String -> (Text -> A.Parser a) -> A.Value -> A.Parser a)
    A.withText

withArray
    :: forall tag a any
     . String
    -> (Array any -> Parser tag a)
    -> Value any
    -> Parser tag a
withArray =
    coerce @(String -> (A.Array -> A.Parser a) -> A.Value -> A.Parser a)
    A.withArray

withScientific
    :: forall tag a any
     . String
    -> (Scientific -> Parser tag a)
    -> Value any
    -> Parser tag a
withScientific =
    coerce @(String -> (Scientific -> A.Parser a) -> A.Value -> A.Parser a)
    A.withScientific

withBool
    :: forall tag a any
     . String
    -> (Bool -> Parser tag a)
    -> Value any
    -> Parser tag a
withBool =
    coerce @(String -> (Bool -> A.Parser a) -> A.Value -> A.Parser a)
    A.withBool

----------------------------------------------------------------------------
-- Encoding combinators
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

object :: [Pair tag] -> Value tag
object = coerce A.object

----------------------------------------------------------------------------
-- Internal
----------------------------------------------------------------------------

parseIndexedJSON :: (Value any -> Parser tag a) -> Int -> Value any -> Parser tag a
parseIndexedJSON p idx value = p value <?> A.Index idx
{-# INLINE parseIndexedJSON #-}

(<?>) :: forall tag a. Parser tag a -> A.JSONPathElement -> Parser tag a
(<?>) = coerce @(A.Parser a -> A.JSONPathElement -> A.Parser a) (A.<?>)
