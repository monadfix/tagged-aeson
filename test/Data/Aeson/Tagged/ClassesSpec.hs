{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Aeson.Tagged.ClassesSpec (spec) where

import BasePrelude
import Data.Aeson.Tagged
import Data.Text (Text)
import Data.Aeson.Types (Result(..))

import Test.Hspec
import Utils
import Types

spec :: Spec
spec = do
    defaultDefinitionsSpec

----------------------------------------------------------------------------
-- FromJSON and ToJSON default definitions
----------------------------------------------------------------------------

defaultDefinitionsSpec :: Spec
defaultDefinitionsSpec = describe "FromJSON and ToJSON default definitions" $ do
    it "parseJSONList" $ do
        parse (parseJSONList @Modded @Text) [value|["a"]|]
            `shouldBe` Error "expected modded text"
        parse (parseJSONList @Modded @Text) [value|["modded:a", "modded:b"]|]
            `shouldBe` Success ["a", "b"]

    it "toEncoding" $ do
        toEncoding @Modded @Text "a"
            `shouldBe` [encoding|"modded:a"|]

    it "toJSONList" $ do
        listToJSON @Modded @Text ["a"]
            `shouldBe` [value|["modded:a"]|]

    it "toEncodingList" $ do
        listToEncoding @Modded @Text ["a"]
            `shouldBe` [encoding|["modded:a"]|]
