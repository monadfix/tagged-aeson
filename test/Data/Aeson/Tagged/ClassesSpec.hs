{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Aeson.Tagged.ClassesSpec (spec) where

import BasePrelude
import Data.Aeson.Tagged
import Data.Aeson.Types (Result(..))

import Test.Hspec
import Utils
import Types

spec :: Spec
spec = do
    defaultDefinitionsSpec

----------------------------------------------------------------------------
-- Tags
----------------------------------------------------------------------------

data Test

----------------------------------------------------------------------------
-- FromJSON and ToJSON default definitions
----------------------------------------------------------------------------

defaultDefinitionsSpec :: Spec
defaultDefinitionsSpec = describe "FromJSON and ToJSON default definitions" $ do
    it "parseJSONList" $ do
        parse (parseJSONList @Test @Int') [value|[1,2]|]
            `shouldBe` Success [1, 2]

    it "toEncoding" $ do
        toEncoding @Test @Int' 1
            `shouldBe` [encoding|1|]

    it "toJSONList" $ do
        listToJSON @Test @Int' [1, 2]
            `shouldBe` [value|[1,2]|]

    it "toEncodingList" $ do
        listToEncoding @Test @Int' [1, 2]
            `shouldBe` [encoding|[1,2]|]
