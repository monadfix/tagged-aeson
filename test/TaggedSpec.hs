{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TaggedSpec (spec) where

import BasePrelude
import Data.Aeson.Tagged
import qualified Data.Text as T
import Data.Text (Text)
-- import qualified Data.Aeson          as A
-- import qualified Data.Aeson.Types    as A
-- import qualified Data.Aeson.TH       as A
-- import qualified Data.Aeson.Internal as A
-- import qualified Data.Aeson.Encoding as E
import Data.Aeson.Types (Result(..))
import qualified GHC.TypeLits

import Test.Hspec
import Util

spec :: Spec
spec = do
    liftingListSpec

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

----------------------------------------------------------------------------
-- parseList, listToJSON, listToEncoding
----------------------------------------------------------------------------

liftingListSpec :: Spec
liftingListSpec = do
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
-- Helpers
----------------------------------------------------------------------------

data Modded

instance FromJSON Modded Text where
    parseJSON = withText "Text@Modded" $ \s ->
        case T.stripPrefix "modded:" s of
            Nothing -> fail "expected modded text"
            Just s' -> pure s'

instance ToJSON Modded Text where
    toJSON s = using @Aeson (toJSON ("modded:" <> s))

instance GHC.TypeLits.TypeError
             ('GHC.TypeLits.Text "[]@Modded should never be used")
      => FromJSON Modded [a] where
    parseJSON = undefined

instance GHC.TypeLits.TypeError
             ('GHC.TypeLits.Text "[]@Modded should never be used")
      => ToJSON Modded [a] where
    toJSON = undefined
