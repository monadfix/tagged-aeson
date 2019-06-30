{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Aeson.Tagged.Wrapped
(
    -- * Type synonyms and newtypes
    Parser(..),
    Value(..),
    Encoding(..),
    Series(..),
    Object,
    Array,
    Pair,

    -- * Converting from and to Aeson versions
    Using(..),

    -- * Patterns for 'Value'
    pattern Object,
    pattern Array,
    pattern String,
    pattern Number,
    pattern Bool,
    pattern Null,

    -- * Functions
    (<?>),
    withObject, withText, withArray, withScientific, withBool,
    object,
    parseIndexedJSON,
)
where

import BasePrelude
import Data.Text (Text)
import Language.Haskell.TH.Lift
import Data.Scientific (Scientific)
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.Aeson.Internal as A
import qualified Data.Aeson.Encoding as E

----------------------------------------------------------------------------
-- Type synonyms and newtypes
----------------------------------------------------------------------------

newtype Parser (tag :: k) a = Parser (A.Parser a)
    deriving newtype (Functor, Applicative, Alternative, Monad,
                      MonadFail, MonadPlus, Semigroup, Monoid)

newtype Value (tag :: k) = Value A.Value
    deriving newtype (Eq, Read, Show, IsString, NFData, Hashable)
    deriving stock (Lift)
    -- TODO KeyValue instances
    -- TODO FromJSON, ToJSON instances
    -- TODO Generic, Data instances

newtype Encoding (tag :: k) = Encoding A.Encoding
    deriving newtype (Eq, Ord, Show)

newtype Series (tag :: k) = Series A.Series
    deriving newtype (Semigroup, Monoid)

type Pair tag = (Text, Value tag)

type Object tag = HM.HashMap Text (Value tag)

type Array tag = V.Vector (Value tag)

----------------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------------

instance A.FromJSON (Value tag) where
    parseJSON = pure . coerce
    {-# INLINE parseJSON #-}

instance A.ToJSON (Value tag) where
    toJSON = coerce
    {-# INLINE toJSON #-}
    toEncoding = coerce E.value
    {-# INLINE toEncoding #-}

----------------------------------------------------------------------------
-- Using
----------------------------------------------------------------------------

class Using tag' tag a' a | a' -> tag', a' tag -> a, a -> tag, a tag' -> a' where
    using :: a' -> a
    default using :: Coercible a' a => a' -> a
    using = coerce
    {-# INLINE using #-}

instance Using tag' tag (Parser tag' a) (Parser tag a)
instance Using tag' tag (Value tag') (Value tag)
instance Using tag' tag (Object tag') (Object tag)
instance Using tag' tag (Encoding tag') (Encoding tag)
instance Using tag' tag (Series tag') (Series tag)
instance Using tag' tag (Pair tag') (Pair tag)

-- TODO: remove these instances?

instance Using tag' tag (x -> Parser tag' a) (x -> Parser tag a)
instance Using tag' tag (x -> Value tag') (x -> Value tag)
instance Using tag' tag (x -> Object tag') (x -> Object tag)
instance Using tag' tag (x -> Encoding tag') (x -> Encoding tag)
instance Using tag' tag (x -> Series tag') (x -> Series tag)
instance Using tag' tag (x -> Pair tag') (x -> Pair tag)

----------------------------------------------------------------------------
-- Patterns
----------------------------------------------------------------------------

-- TODO document really well

pattern Object :: Object tag -> Value tag
pattern Object a <- Value (A.Object (coerce -> a)) where
    Object a = Value (A.Object (coerce a))

pattern Array :: Array tag -> Value tag
pattern Array a <- Value (A.Array (coerce -> a)) where
    Array a = Value (A.Array (coerce a))

pattern String :: Text -> Value tag
pattern String a <- Value (A.String a) where
    String a = Value (A.String a)

pattern Number :: Scientific -> Value tag
pattern Number a <- Value (A.Number a) where
    Number a = Value (A.Number a)

pattern Bool :: Bool -> Value tag
pattern Bool a <- Value (A.Bool a) where
    Bool a = Value (A.Bool a)

pattern Null :: Value tag
pattern Null <- Value A.Null where
    Null = Value A.Null

----------------------------------------------------------------------------
-- Functions
----------------------------------------------------------------------------

(<?>) :: forall tag a. Parser tag a -> A.JSONPathElement -> Parser tag a
(<?>) = coerce @(A.Parser a -> A.JSONPathElement -> A.Parser a) (A.<?>)
{-# INLINE (<?>) #-}

withObject
    :: forall tag a any
     . String
    -> (Object any -> Parser tag a)
    -> Value any
    -> Parser tag a
withObject =
    coerce @(String -> (A.Object -> A.Parser a) -> A.Value -> A.Parser a)
    A.withObject
{-# INLINE withObject #-}

withText
    :: forall tag a any
     . String
    -> (Text -> Parser tag a)
    -> Value any
    -> Parser tag a
withText =
    coerce @(String -> (Text -> A.Parser a) -> A.Value -> A.Parser a)
    A.withText
{-# INLINE withText #-}

withArray
    :: forall tag a any
     . String
    -> (Array any -> Parser tag a)
    -> Value any
    -> Parser tag a
withArray =
    coerce @(String -> (A.Array -> A.Parser a) -> A.Value -> A.Parser a)
    A.withArray
{-# INLINE withArray #-}

withScientific
    :: forall tag a any
     . String
    -> (Scientific -> Parser tag a)
    -> Value any
    -> Parser tag a
withScientific =
    coerce @(String -> (Scientific -> A.Parser a) -> A.Value -> A.Parser a)
    A.withScientific
{-# INLINE withScientific #-}

withBool
    :: forall tag a any
     . String
    -> (Bool -> Parser tag a)
    -> Value any
    -> Parser tag a
withBool =
    coerce @(String -> (Bool -> A.Parser a) -> A.Value -> A.Parser a)
    A.withBool
{-# INLINE withBool #-}

object :: [Pair tag] -> Value tag
object = coerce A.object
{-# INLINE object #-}

parseIndexedJSON :: (Value any -> Parser tag a) -> Int -> Value any -> Parser tag a
parseIndexedJSON p idx value = p value <?> A.Index idx
{-# INLINE parseIndexedJSON #-}
