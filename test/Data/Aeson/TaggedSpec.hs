{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Aeson.TaggedSpec (spec) where

-- TODO: test with all recent Aeson versions. Add strict Aeson bounds.

import BasePrelude
import Data.Aeson.Tagged
import Data.Text (Text)
import qualified Data.Aeson as A
import Data.Aeson.Types (Result(..))

import Test.Hspec
import Utils
import Types

spec :: Spec
spec = do
    defaultDefinitionsSpec
    aesonTypeAnnotationSpec
    taggedAesonSpec

-- Does 'using' work? On Parser, on Value, on functions? With (.:)? With (.=)? With 'object'?
-- Does "deriving via WithAeson" work?
-- Does "deriving via WithAeson1" work? On stuff that has FromJSON1? On Set and HashSet?
-- Does encoding and decoding Rational work?
-- Patterns

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

----------------------------------------------------------------------------
-- parseJSON @Aeson and toJSON @Aeson
----------------------------------------------------------------------------

aesonTypeAnnotationSpec :: Spec
aesonTypeAnnotationSpec = describe "@Aeson type annotation" $ do
    it "parseJSON @Aeson" $ do
        parse (parseJSON @Aeson) [value|"a"|]
            `shouldBe` Success ("a" :: Text)

    it "toJSON @Aeson" $ do
        toJSON @Aeson ("a" :: Text)
            `shouldBe` [value|"a"|]

----------------------------------------------------------------------------
-- TaggedAeson and fromTaggedAeson
----------------------------------------------------------------------------

taggedAesonSpec :: Spec
taggedAesonSpec = describe "TaggedAeson" $ do
    it "works with 'decode'" $ do
        fromTaggedAeson @Modded <$> A.decode "\"modded:a\""
            `shouldBe` Just ("a" :: Text)

    it "works with 'encode'" $ do
        A.encode (TaggedAeson @Modded ("a" :: Text))
            `shouldBe` "\"modded:a\""
