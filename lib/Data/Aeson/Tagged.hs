module Data.Aeson.Tagged
(
    -- * Classes
    FromJSON(..),
    ToJSON(..),

    -- * Interop between aeson and tagged-aeson
    Aeson,
    TaggedAeson(..), fromTaggedAeson,

    -- * Template Haskell
    deriveJSON,
    deriveFromJSON,
    deriveToJSON,

    -- * 'using'
    Using(..),

    -- * Parsing combinators
    (.:), (.:?), (.:!),
    withObject, withText, withArray, withScientific, withBool,
    (<?>),

    -- * Encoding combinators
    KeyValue(..),
    object,

    -- * Lifting instances from Aeson
    WithAeson(..),
    WithAeson1(..),

    -- * Instance-less
    -- ** Lists
    parseList, parseListWith,
    listToJSON, listToJSONWith,
    listToEncoding, listToEncodingWith,
    -- ** 'NE.NonEmpty'
    parseNonEmpty, parseNonEmptyWith,
    nonEmptyToJSON, nonEmptyToJSONWith,
    nonEmptyToEncoding, nonEmptyToEncodingWith,
    -- ** 'V.Vector'
    parseVector, parseVectorWith,
    vectorToJSON, vectorToJSONWith,
    vectorToEncoding, vectorToEncodingWith,
    -- ** 'S.Set'
    parseSet, parseSetWith,
    setToJSON, setToJSONWith,
    setToEncoding, setToEncodingWith,
    -- ** 'HS.HashSet'
    parseHashSet, parseHashSetWith,
    hashSetToJSON, hashSetToJSONWith,
    hashSetToEncoding, hashSetToEncodingWith,

    -- * Patterns
    pattern Object,
    pattern Array,
    pattern String,
    pattern Number,
    pattern Bool,
    pattern Null,

    -- * Internals
    Parser(..),
    Value(..),
    Encoding(..),
    Series(..),
    Object,
    Array,
    Pair,
)
where

import Data.Aeson.Tagged.Wrapped
import Data.Aeson.Tagged.Classes
import Data.Aeson.Tagged.Explicit
import Data.Aeson.Tagged.Types
import Data.Aeson.Tagged.TH

-- TODO: add decode and friends
