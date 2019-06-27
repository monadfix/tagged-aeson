module TaggedSpec (spec) where

import Test.Hspec

spec :: Spec
spec = pure ()

-- Do default definitions for FromJSON and ToJSON methods work?
-- Do "parseJSON @Aeson" and "toJSON @Aeson" work?
-- Do TaggedAeson and fromTaggedAeson work?
-- Do 'deriveJSON', 'deriveFromJSON', 'deriveToJSON' work?
-- Does 'using' work? On Parser, on Value, on functions? With (.:)? With (.=)? With 'object'?
-- Does "deriving via WithAeson" work?
-- Does "deriving via WithAeson1" work? On stuff that has FromJSON1? On Set and HashSet?
-- Does encoding and decoding Rational work?
-- parseList, listToJSON, listToEncoding
-- parseNonEmpty, nonEmptyToJSON, nonEmptyToEncoding
-- parseVector, vectorToJSON, vectorToEncoding
-- parseSet, setToJSON, setToEncoding
-- parseHashSet, hashSetToJSON, hashSetToEncoding
