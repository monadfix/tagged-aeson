{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Aeson.Tagged.ExplicitSpec (spec) where

import BasePrelude
import Data.Aeson.Tagged
import Data.Text (Text)
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
-- parseList, listToJSON, listToEncoding
----------------------------------------------------------------------------

explicitListSpec :: Spec
explicitListSpec = describe "lists" $ do
    describe "parseList" $ do
        it "uses tagged-aeson for list elements" $ do
            parse (parseList @Modded @Text) [value|["a"]|]
                `shouldBe` Error "expected modded text"
            parse (parseList @Modded @Text) [value|["modded:a", "modded:b"]|]
                `shouldBe` Success ["a", "b"]

    describe "listToJSON" $ do
        it "uses tagged-aeson for list elements" $ do
            listToJSON @Modded @Text ["a"]
                `shouldBe` [value|["modded:a"]|]

    describe "listToEncoding" $ do
        it "uses tagged-aeson for list elements" $ do
            listToEncoding @Modded @Text ["a"]
                `shouldBe` [encoding|["modded:a"]|]

----------------------------------------------------------------------------
-- parseNonEmpty, nonEmptyToJSON, nonEmptyToEncoding
----------------------------------------------------------------------------

explicitNonEmptySpec :: Spec
explicitNonEmptySpec = describe "NonEmpty" $ do
    describe "parseNonEmpty" $ do
        it "uses tagged-aeson for list elements" $ do
            parse (parseNonEmpty @Modded @Text) [value|["a"]|]
                `shouldBe` Error "expected modded text"
            parse (parseNonEmpty @Modded @Text) [value|["modded:a", "modded:b"]|]
                `shouldBe` Success (NE.fromList ["a", "b"])

        it "does not parse empty lists" $ do
            parse (parseNonEmpty @Modded @Text) [value|[]|]
                `shouldBe` Error "parsing NonEmpty failed, unpexpected empty list"

    describe "nonEmptyToJSON" $ do
        it "uses tagged-aeson for list elements" $ do
            nonEmptyToJSON @Modded @Text (NE.fromList ["a"])
                `shouldBe` [value|["modded:a"]|]

    describe "nonEmptyToEncoding" $ do
        it "uses tagged-aeson for list elements" $ do
            nonEmptyToEncoding @Modded @Text (NE.fromList ["a"])
                `shouldBe` [encoding|["modded:a"]|]

----------------------------------------------------------------------------
-- parseVector, vectorToJSON, vectorToEncoding
----------------------------------------------------------------------------

explicitVectorSpec :: Spec
explicitVectorSpec = describe "Vector" $ do
    describe "parseVector" $ do
        it "uses tagged-aeson for vector elements" $ do
            parse (parseVector @Modded @Text) [value|["a"]|]
                `shouldBe` Error "expected modded text"
            parse (parseVector @Modded @Text) [value|["modded:a", "modded:b"]|]
                `shouldBe` Success (V.fromList ["a", "b"])

    describe "vectorToJSON" $ do
        it "uses tagged-aeson for vector elements" $ do
            vectorToJSON @Modded @Text (V.fromList ["a"])
                `shouldBe` [value|["modded:a"]|]

    describe "vectorToEncoding" $ do
        it "uses tagged-aeson for vector elements" $ do
            vectorToEncoding @Modded @Text (V.fromList ["a"])
                `shouldBe` [encoding|["modded:a"]|]

----------------------------------------------------------------------------
-- parseSet, setToJSON, setToEncoding
----------------------------------------------------------------------------

explicitSetSpec :: Spec
explicitSetSpec = describe "Set" $ do
    describe "parseSet" $ do
        it "uses tagged-aeson for list elements" $ do
            parse (parseSet @Modded @Text) [value|["a"]|]
                `shouldBe` Error "expected modded text"
            parse (parseSet @Modded @Text) [value|["modded:a", "modded:b"]|]
                `shouldBe` Success (S.fromList ["a", "b"])

    describe "setToJSON" $ do
        it "uses tagged-aeson for list elements" $ do
            setToJSON @Modded @Text (S.fromList ["a"])
                `shouldBe` [value|["modded:a"]|]

    describe "setToEncoding" $ do
        it "uses tagged-aeson for list elements" $ do
            setToEncoding @Modded @Text (S.fromList ["a"])
                `shouldBe` [encoding|["modded:a"]|]

----------------------------------------------------------------------------
-- parseHashSet, hashSetToJSON, hashSetToEncoding
----------------------------------------------------------------------------

explicitHashSetSpec :: Spec
explicitHashSetSpec = describe "HashSet" $ do
    describe "parseHashSet" $ do
        it "uses tagged-aeson for list elements" $ do
            parse (parseHashSet @Modded @Text) [value|["a"]|]
                `shouldBe` Error "expected modded text"
            parse (parseHashSet @Modded @Text) [value|["modded:a", "modded:b"]|]
                `shouldBe` Success (HS.fromList ["a", "b"])

    describe "hashSetToJSON" $ do
        it "uses tagged-aeson for list elements" $ do
            hashSetToJSON @Modded @Text (HS.fromList ["a"])
                `shouldBe` [value|["modded:a"]|]

    describe "hashSetToEncoding" $ do
        it "uses tagged-aeson for list elements" $ do
            hashSetToEncoding @Modded @Text (HS.fromList ["a"])
                `shouldBe` [encoding|["modded:a"]|]
