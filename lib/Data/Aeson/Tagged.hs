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

    -- * Combinators
    (.:), (.:?), (.:!),
    withObject, withText, withArray, withScientific, withBool,

    -- * Internals
    Parser(..),
)
where


import BasePrelude
import Data.Text (Text)
import Data.Generics.Uniplate.Data (transformBi)
import Language.Haskell.TH
import Data.Scientific (Scientific)
import qualified Data.Vector as V

-- aeson
import qualified Data.Aeson          as A
import qualified Data.Aeson.Types    as A
import qualified Data.Aeson.TH       as A
import qualified Data.Aeson.Internal as A
import Data.Aeson (Encoding, Object, Array, Value(..))
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
    parseJSON :: Value -> Parser tag a

    parseJSONList :: Value -> Parser tag [a]
    parseJSONList =
      withArray "[]" $ \a ->
        zipWithM (parseIndexedJSON parseJSON) [0..] $
        V.toList a

class ToJSON (tag :: k) a where
    toJSON :: a -> Value

    toEncoding :: a -> Encoding
    toEncoding = E.value . toJSON @tag
    {-# INLINE toEncoding #-}

    toJSONList :: [a] -> Value
    toJSONList = A.listValue (toJSON @tag)
    {-# INLINE toJSONList #-}

    toEncodingList :: [a] -> Encoding
    toEncodingList = A.listEncoding (toEncoding @tag)
    {-# INLINE toEncodingList #-}

----------------------------------------------------------------------------
-- Interop between aeson and tagged-aeson
----------------------------------------------------------------------------

-- | The tag for original @aeson@ instances. You can use @parseJSON \@Aeson@
-- and @toJSON \@Aeson@ to get aeson's parsing behavior.
data Aeson

instance A.FromJSON a => FromJSON Aeson a where
    parseJSON =
        coerce @(Value -> A.Parser a)
               @(Value -> Parser Aeson a)
        A.parseJSON

    parseJSONList =
        coerce @(Value -> A.Parser [a])
               @(Value -> Parser Aeson [a])
        A.parseJSONList

instance A.ToJSON a => ToJSON Aeson a where
    toJSON = A.toJSON
    toEncoding = A.toEncoding
    toJSONList = A.toJSONList
    toEncodingList = A.toEncodingList

-- | A newtype wrapper to use tagged-aeson instances with functions from
-- @aeson@ (or @yaml@).
newtype TaggedAeson (tag :: k) a = TaggedAeson a
    deriving (Eq, Ord, Show)

fromTaggedAeson :: forall tag a. TaggedAeson tag a -> a
fromTaggedAeson = coerce

instance FromJSON tag a => A.FromJSON (TaggedAeson tag a) where
    parseJSON =
        coerce @(Value -> Parser tag a)
               @(Value -> A.Parser (TaggedAeson tag a))
        parseJSON

    -- TODO: I'm worried that there's too much jumping between FromJSON and
    -- A.FromJSON instances when parsing e.g. @TaggedAeson tag [Foo]@
    parseJSONList =
        coerce @(Value -> Parser tag [a])
               @(Value -> A.Parser [TaggedAeson tag a])
        parseJSONList

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

-- TODO: can this be moved into the class?
retag :: forall tag tag' a. Tag tag a => a -> Retagged tag tag' a
retag = retag' @tag @a @tag'

----------------------------------------------------------------------------
-- Reimplementations
----------------------------------------------------------------------------

newtype Parser (tag :: k) a = Parser (A.Parser a)
    deriving newtype (Functor, Applicative, Alternative, Monad,
                      MonadFail, MonadPlus, Semigroup, Monoid)

-- | Retrieve the value associated with the given key of an 'Object'.
-- The result is 'empty' if the key is not present or the value cannot
-- be converted to the desired type.
--
-- This accessor is appropriate if the key and value /must/ be present
-- in an object for it to be valid.  If the key and value are
-- optional, use '.:?' instead.
(.:) :: forall tag a. (FromJSON tag a)
     => Object -> Text -> Parser tag a
(.:) = coerce @(Object -> Text -> A.Parser (TaggedAeson tag a)) (A..:)
{-# INLINE (.:) #-}

-- | Retrieve the value associated with the given key of an 'Object'. The
-- result is 'Nothing' if the key is not present or if its value is 'Null',
-- or 'empty' if the value cannot be converted to the desired type.
--
-- This accessor is most useful if the key and value can be absent
-- from an object without affecting its validity.  If the key and
-- value are mandatory, use '.:' instead.
(.:?) :: forall tag a. (FromJSON tag a)
      => Object -> Text -> Parser tag (Maybe a)
(.:?) = coerce @(Object -> Text -> A.Parser (Maybe (TaggedAeson tag a))) (A..:?)
{-# INLINE (.:?) #-}

-- | Retrieve the value associated with the given key of an 'Object'.
-- The result is 'Nothing' if the key is not present or 'empty' if the
-- value cannot be converted to the desired type.
--
-- This differs from '.:?' by attempting to parse 'Null' the same as any
-- other JSON value, instead of interpreting it as 'Nothing'.
(.:!) :: forall tag a. (FromJSON tag a)
      => Object -> Text -> Parser tag (Maybe a)
(.:!) = coerce @(Object -> Text -> A.Parser (Maybe (TaggedAeson tag a))) (A..:!)
{-# INLINE (.:!) #-}

withObject
    :: forall tag a
     . String
    -> (Object -> Parser tag a)
    -> Value
    -> Parser tag a
withObject =
    coerce @(String -> (Object -> A.Parser a) -> Value -> A.Parser a)
    A.withObject

withText
    :: forall tag a
     . String
    -> (Text -> Parser tag a)
    -> Value
    -> Parser tag a
withText =
    coerce @(String -> (Text -> A.Parser a) -> Value -> A.Parser a)
    A.withText

withArray
    :: forall tag a
     . String
    -> (Array -> Parser tag a)
    -> Value
    -> Parser tag a
withArray =
    coerce @(String -> (Array -> A.Parser a) -> Value -> A.Parser a)
    A.withArray

withScientific
    :: forall tag a
     . String
    -> (Scientific -> Parser tag a)
    -> Value
    -> Parser tag a
withScientific =
    coerce @(String -> (Scientific -> A.Parser a) -> Value -> A.Parser a)
    A.withScientific

withBool
    :: forall tag a
     . String
    -> (Bool -> Parser tag a)
    -> Value
    -> Parser tag a
withBool =
    coerce @(String -> (Bool -> A.Parser a) -> Value -> A.Parser a)
    A.withBool

----------------------------------------------------------------------------
-- Internal
----------------------------------------------------------------------------

parseIndexedJSON :: (Value -> Parser tag a) -> Int -> Value -> Parser tag a
parseIndexedJSON p idx value = p value <?> A.Index idx
{-# INLINE parseIndexedJSON #-}

(<?>) :: forall tag a. Parser tag a -> A.JSONPathElement -> Parser tag a
(<?>) = coerce @(A.Parser a -> A.JSONPathElement -> A.Parser a) (A.<?>)
