{-# LANGUAGE NoImplicitPrelude #-}

module Data.Aeson.Tagged.Explicit
(
    -- * Lists
    parseList, parseListWith,
    listToJSON, listToJSONWith,
    listToEncoding, listToEncodingWith,

    -- * 'NE.NonEmpty'
    parseNonEmpty, parseNonEmptyWith,
    nonEmptyToJSON, nonEmptyToJSONWith,
    nonEmptyToEncoding, nonEmptyToEncodingWith,

    -- * 'V.Vector'
    parseVector, parseVectorWith,
    vectorToJSON, vectorToJSONWith,
    vectorToEncoding, vectorToEncodingWith,

    -- * 'S.Set'
    parseSet, parseSetWith,
    setToJSON, setToJSONWith,
    setToEncoding, setToEncodingWith,

    -- * 'HS.HashSet'
    parseHashSet, parseHashSetWith,
    hashSetToJSON, hashSetToJSONWith,
    hashSetToEncoding, hashSetToEncodingWith,
)
where

import BasePrelude
import qualified Data.List.NonEmpty as NE
import qualified Data.Vector as V
import qualified Data.Set as S
import qualified Data.HashSet as HS
import Data.Hashable (Hashable)
import qualified Data.Aeson.Types as A
import qualified Data.Aeson.Encoding.Internal as E

import Data.Aeson.Tagged.Classes
import Data.Aeson.Tagged.Wrapped

----------------------------------------------------------------------------
-- Lists
----------------------------------------------------------------------------

parseList :: forall tag a any. FromJSON tag a => Value any -> Parser tag [a]
parseList = parseListWith parseJSON
{-# INLINE parseList #-}

parseListWith :: forall tag a any. (Value any -> Parser tag a) -> Value any -> Parser tag [a]
parseListWith p = withArray "[]" $
    zipWithM (parseIndexedJSON p) [0..] . V.toList
{-# INLINE parseListWith #-}

-- TODO: warn that it doesn't correspond to 'toJSONList'
listToJSON :: forall tag a. ToJSON tag a => [a] -> Value tag
listToJSON = listToJSONWith toJSON
{-# INLINE listToJSON #-}

listToJSONWith :: forall tag a. (a -> Value tag) -> [a] -> Value tag
listToJSONWith = coerce (A.listValue @a)
{-# INLINE listToJSONWith #-}

listToEncoding :: forall tag a. ToJSON tag a => [a] -> Encoding tag
listToEncoding = listToEncodingWith toEncoding
{-# INLINE listToEncoding #-}

listToEncodingWith :: forall tag a. (a -> Encoding tag) -> [a] -> Encoding tag
listToEncodingWith = coerce (A.listEncoding @a)
{-# INLINE listToEncodingWith #-}

----------------------------------------------------------------------------
-- NonEmpty
----------------------------------------------------------------------------

parseNonEmpty :: forall tag a any. FromJSON tag a => Value any -> Parser tag (NE.NonEmpty a)
parseNonEmpty = parseNonEmptyWith parseJSON
{-# INLINE parseNonEmpty #-}

parseNonEmptyWith :: forall tag a any. (Value any -> Parser tag a) -> Value any -> Parser tag (NE.NonEmpty a)
parseNonEmptyWith p = withArray "NonEmpty" $
    (>>= ne) . sequence .
    zipWith (parseIndexedJSON p) [0..] . V.toList
  where
    ne []     = fail "parsing NonEmpty failed, unpexpected empty list"
    ne (x:xs) = pure (x :| xs)
{-# INLINE parseNonEmptyWith #-}

nonEmptyToJSON :: forall tag a. ToJSON tag a => NE.NonEmpty a -> Value tag
nonEmptyToJSON = nonEmptyToJSONWith toJSON
{-# INLINE nonEmptyToJSON #-}

nonEmptyToJSONWith :: forall tag a. (a -> Value tag) -> NE.NonEmpty a -> Value tag
nonEmptyToJSONWith f = listToJSONWith f . NE.toList
{-# INLINE nonEmptyToJSONWith #-}

nonEmptyToEncoding :: forall tag a. ToJSON tag a => NE.NonEmpty a -> Encoding tag
nonEmptyToEncoding = nonEmptyToEncodingWith toEncoding
{-# INLINE nonEmptyToEncoding #-}

nonEmptyToEncodingWith :: forall tag a. (a -> Encoding tag) -> NE.NonEmpty a -> Encoding tag
nonEmptyToEncodingWith f = listToEncodingWith f . NE.toList
{-# INLINE nonEmptyToEncodingWith #-}

----------------------------------------------------------------------------
-- Vector
----------------------------------------------------------------------------

parseVector :: forall tag a any. FromJSON tag a => Value any -> Parser tag (V.Vector a)
parseVector = parseVectorWith parseJSON
{-# INLINE parseVector #-}

parseVectorWith :: forall tag a any. (Value any -> Parser tag a) -> Value any -> Parser tag (V.Vector a)
parseVectorWith p = withArray "Vector" $
    V.mapM (uncurry $ parseIndexedJSON p) . V.indexed
{-# INLINE parseVectorWith #-}

vectorToJSON :: forall tag a. ToJSON tag a => V.Vector a -> Value tag
vectorToJSON = vectorToJSONWith toJSON
{-# INLINE vectorToJSON #-}

vectorToJSONWith :: forall tag a. (a -> Value tag) -> V.Vector a -> Value tag
vectorToJSONWith f = Array . V.map f
{-# INLINE vectorToJSONWith #-}

vectorToEncoding :: forall tag a. ToJSON tag a => V.Vector a -> Encoding tag
vectorToEncoding = vectorToEncodingWith toEncoding
{-# INLINE vectorToEncoding #-}

vectorToEncodingWith :: forall tag a. (a -> Encoding tag) -> V.Vector a -> Encoding tag
vectorToEncodingWith f = listToEncodingWith f . V.toList
{-# INLINE vectorToEncodingWith #-}

----------------------------------------------------------------------------
-- Set
----------------------------------------------------------------------------

parseSet :: forall tag a any. (Ord a, FromJSON tag a) => Value any -> Parser tag (S.Set a)
parseSet = parseSetWith parseJSON
{-# INLINE parseSet #-}

parseSetWith :: forall tag a any. Ord a => (Value any -> Parser tag a) -> Value any -> Parser tag (S.Set a)
parseSetWith p = fmap S.fromList . parseListWith p
{-# INLINE parseSetWith #-}

setToJSON :: forall tag a. ToJSON tag a => S.Set a -> Value tag
setToJSON = setToJSONWith toJSON
{-# INLINE setToJSON #-}

setToJSONWith :: forall tag a. (a -> Value tag) -> S.Set a -> Value tag
setToJSONWith f = listToJSONWith f . S.toList
{-# INLINE setToJSONWith #-}

setToEncoding :: forall tag a. ToJSON tag a => S.Set a -> Encoding tag
setToEncoding = setToEncodingWith toEncoding
{-# INLINE setToEncoding #-}

setToEncodingWith :: forall tag a. (a -> Encoding tag) -> S.Set a -> Encoding tag
setToEncodingWith f = listToEncodingWith f . S.toList
{-# INLINE setToEncodingWith #-}

----------------------------------------------------------------------------
-- HashSet
----------------------------------------------------------------------------

parseHashSet :: forall tag a any. (Hashable a, Eq a, FromJSON tag a) => Value any -> Parser tag (HS.HashSet a)
parseHashSet = parseHashSetWith parseJSON
{-# INLINE parseHashSet #-}

parseHashSetWith :: forall tag a any. (Hashable a, Eq a) => (Value any -> Parser tag a) -> Value any -> Parser tag (HS.HashSet a)
parseHashSetWith p = fmap HS.fromList . parseListWith p
{-# INLINE parseHashSetWith #-}

hashSetToJSON :: forall tag a. ToJSON tag a => HS.HashSet a -> Value tag
hashSetToJSON = hashSetToJSONWith toJSON
{-# INLINE hashSetToJSON #-}

hashSetToJSONWith :: forall tag a. (a -> Value tag) -> HS.HashSet a -> Value tag
hashSetToJSONWith f = listToJSONWith f . HS.toList
{-# INLINE hashSetToJSONWith #-}

hashSetToEncoding :: forall tag a. ToJSON tag a => HS.HashSet a -> Encoding tag
hashSetToEncoding = hashSetToEncodingWith toEncoding
{-# INLINE hashSetToEncoding #-}

hashSetToEncodingWith :: forall tag a. (a -> Encoding tag) -> HS.HashSet a -> Encoding tag
hashSetToEncodingWith f = listToEncodingWith f . HS.toList
{-# INLINE hashSetToEncodingWith #-}

{- TODO

Bool
Double
Float
Int*
Word*
String
Text
IntSet
Scientific
UTCTime
UUID
Value
Maybe
Tree
tuples
Map
HashMap
Seq

note: "This instance includes a bounds check to prevent maliciously large inputs to fill up the memory of the target system. You can newtype Scientific and provide your own instance using withScientific if you want to allow larger inputs."

be careful: `using @Aeson [Foo]` will use Aeson for Foo as well

TODO: ToJSONKey

TODO: can we do without default Value instances?
-}
