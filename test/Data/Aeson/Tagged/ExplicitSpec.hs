{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Aeson.Tagged.ExplicitSpec (spec) where

import BasePrelude
import Data.Aeson.Tagged
import qualified Data.List.NonEmpty as NE
import qualified Data.Vector as V
import qualified Data.Set as S
import qualified Data.HashSet as HS
import Data.Aeson.Types (Result(..))

import Test.Hspec
import Utils
import Types

spec :: Spec
spec = describe "explicit decoders/encoders" $ do
    explicitListSpec
    explicitNonEmptySpec
    explicitVectorSpec
    explicitSetSpec
    explicitHashSetSpec

----------------------------------------------------------------------------
-- Tags
----------------------------------------------------------------------------

data Test

----------------------------------------------------------------------------
-- parseList, listToJSON, listToEncoding
----------------------------------------------------------------------------

explicitListSpec :: Spec
explicitListSpec = describe "lists" $ do
    describe "parseList" $ do
        it "uses tagged-aeson for list elements" $ do
            parse (parseList @Test @Int') [value|[1,2]|]
                `shouldBe` Success [1, 2]

    describe "listToJSON" $ do
        it "uses tagged-aeson for list elements" $ do
            listToJSON @Test @Int' [1]
                `shouldBe` [value|[1]|]

    describe "listToEncoding" $ do
        it "uses tagged-aeson for list elements" $ do
            listToEncoding @Test @Int' [1]
                `shouldBe` [encoding|[1]|]

----------------------------------------------------------------------------
-- parseNonEmpty, nonEmptyToJSON, nonEmptyToEncoding
----------------------------------------------------------------------------

explicitNonEmptySpec :: Spec
explicitNonEmptySpec = describe "NonEmpty" $ do
    describe "parseNonEmpty" $ do
        it "uses tagged-aeson for list elements" $ do
            parse (parseNonEmpty @Test @Int') [value|[1,2]|]
                `shouldBe` Success (NE.fromList [1, 2])

        it "does not parse empty lists" $ do
            parse (parseNonEmpty @Test @Int') [value|[]|]
                `shouldBe` Error "parsing NonEmpty failed, unpexpected empty list"

    describe "nonEmptyToJSON" $ do
        it "uses tagged-aeson for list elements" $ do
            nonEmptyToJSON @Test @Int' (NE.fromList [1, 2])
                `shouldBe` [value|[1,2]|]

    describe "nonEmptyToEncoding" $ do
        it "uses tagged-aeson for list elements" $ do
            nonEmptyToEncoding @Test @Int' (NE.fromList [1, 2])
                `shouldBe` [encoding|[1,2]|]

----------------------------------------------------------------------------
-- parseVector, vectorToJSON, vectorToEncoding
----------------------------------------------------------------------------

explicitVectorSpec :: Spec
explicitVectorSpec = describe "Vector" $ do
    describe "parseVector" $ do
        it "uses tagged-aeson for vector elements" $ do
            parse (parseVector @Test @Int') [value|[1,2]|]
                `shouldBe` Success (V.fromList [1, 2])

    describe "vectorToJSON" $ do
        it "uses tagged-aeson for vector elements" $ do
            vectorToJSON @Test @Int' (V.fromList [1, 2])
                `shouldBe` [value|[1,2]|]

    describe "vectorToEncoding" $ do
        it "uses tagged-aeson for vector elements" $ do
            vectorToEncoding @Test @Int' (V.fromList [1, 2])
                `shouldBe` [encoding|[1,2]|]

----------------------------------------------------------------------------
-- parseSet, setToJSON, setToEncoding
----------------------------------------------------------------------------

explicitSetSpec :: Spec
explicitSetSpec = describe "Set" $ do
    describe "parseSet" $ do
        it "uses tagged-aeson for list elements" $ do
            parse (parseSet @Test @Int') [value|[1,2]|]
                `shouldBe` Success (S.fromList [1, 2])

    describe "setToJSON" $ do
        it "uses tagged-aeson for list elements" $ do
            setToJSON @Test @Int' (S.fromList [1, 2])
                `shouldBe` [value|[1,2]|]

    describe "setToEncoding" $ do
        it "uses tagged-aeson for list elements" $ do
            setToEncoding @Test @Int' (S.fromList [1, 2])
                `shouldBe` [encoding|[1,2]|]

----------------------------------------------------------------------------
-- parseHashSet, hashSetToJSON, hashSetToEncoding
----------------------------------------------------------------------------

explicitHashSetSpec :: Spec
explicitHashSetSpec = describe "HashSet" $ do
    describe "parseHashSet" $ do
        it "uses tagged-aeson for list elements" $ do
            parse (parseHashSet @Test @Int') [value|[1,2]|]
                `shouldBe` Success (HS.fromList [1, 2])

    describe "hashSetToJSON" $ do
        it "uses tagged-aeson for list elements" $ do
            hashSetToJSON @Test @Int' (HS.fromList [1, 2])
                `shouldBe` [value|[1,2]|]

    describe "hashSetToEncoding" $ do
        it "uses tagged-aeson for list elements" $ do
            hashSetToEncoding @Test @Int' (HS.fromList [1, 2])
                `shouldBe` [encoding|[1,2]|]
