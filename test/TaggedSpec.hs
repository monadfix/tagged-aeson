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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TaggedSpec (spec) where

import BasePrelude
import Data.Aeson.Tagged
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.List.NonEmpty as NE
import qualified Data.Vector as V
import Data.Vector (Vector)
import qualified Data.Set as S
import Data.Set (Set)
import qualified Data.HashSet as HS
import Data.HashSet (HashSet)
import qualified Data.Aeson          as A
-- import qualified Data.Aeson.Types    as A
-- import qualified Data.Aeson.TH       as A
-- import qualified Data.Aeson.Internal as A
-- import qualified Data.Aeson.Encoding as E
import Data.Aeson.Types (Result(..))
import qualified GHC.TypeLits as TypeLits

import Test.Hspec
import Util

spec :: Spec
spec = do
    defaultDefinitionsSpec
    aesonTypeAnnotationSpec
    taggedAesonSpec
    liftingListSpec
    liftingNonEmptySpec
    liftingVectorSpec
    liftingSetSpec
    liftingHashSetSpec
    thDerivingSpec

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

----------------------------------------------------------------------------
-- parseList, listToJSON, listToEncoding
----------------------------------------------------------------------------

liftingListSpec :: Spec
liftingListSpec = describe "explicit list parsers" $ do
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

liftingNonEmptySpec :: Spec
liftingNonEmptySpec = describe "explicit NonEmpty parsers" $ do
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

liftingVectorSpec :: Spec
liftingVectorSpec = describe "explicit Vector parsers" $ do
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

liftingSetSpec :: Spec
liftingSetSpec = describe "explicit Set parsers" $ do
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

liftingHashSetSpec :: Spec
liftingHashSetSpec = describe "explicit HashSet parsers" $ do
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

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

-- | A tag for instances that are different from Aeson-provided instances.
data Modded

instance FromJSON Modded Text where
    parseJSON = withText "Text@Modded" $ \s ->
        case T.stripPrefix "modded:" s of
            Nothing -> fail "expected modded text"
            Just s' -> pure s'

instance TypeLits.TypeError ('TypeLits.Text "[]@Modded should never be used")
      => FromJSON Modded [a] where
    parseJSON = undefined
instance TypeLits.TypeError ('TypeLits.Text "NonEmpty@Modded should never be used")
      => FromJSON Modded (NonEmpty a) where
    parseJSON = undefined
instance TypeLits.TypeError ('TypeLits.Text "Vector@Modded should never be used")
      => FromJSON Modded (Vector a) where
    parseJSON = undefined
instance TypeLits.TypeError ('TypeLits.Text "Set@Modded should never be used")
      => FromJSON Modded (Set a) where
    parseJSON = undefined
instance TypeLits.TypeError ('TypeLits.Text "HashSet@Modded should never be used")
      => FromJSON Modded (HashSet a) where
    parseJSON = undefined

instance ToJSON Modded Text where
    toJSON s = using @Aeson (toJSON ("modded:" <> s))

instance TypeLits.TypeError ('TypeLits.Text "[]@Modded should never be used")
      => ToJSON Modded [a] where
    toJSON = undefined
instance TypeLits.TypeError ('TypeLits.Text "NonEmpty@Modded should never be used")
      => ToJSON Modded (NonEmpty a) where
    toJSON = undefined
instance TypeLits.TypeError ('TypeLits.Text "Vector@Modded should never be used")
      => ToJSON Modded (Vector a) where
    toJSON = undefined
instance TypeLits.TypeError ('TypeLits.Text "Set@Modded should never be used")
      => ToJSON Modded (Set a) where
    toJSON = undefined
instance TypeLits.TypeError ('TypeLits.Text "HashSet@Modded should never be used")
      => ToJSON Modded (HashSet a) where
    toJSON = undefined

----------------------------------------------------------------------------
-- Template Haskell deriving (has to be last because of TH sorting)
----------------------------------------------------------------------------

thDerivingSpec :: Spec
thDerivingSpec = describe "Template Haskell deriving" $ do
    -- Note: we are not testing 'deriveFromJSON' and 'deriveToJSON' because
    -- they are more-or-less tested as part of testing 'deriveJSON'
    describe "deriveJSON" $ do
        describe "TH1 NoAeson" $ do
            it "parseJSON works" $ do
                parse (parseJSON @NoAeson @TH) [value|"no-aeson"|]
                    `shouldBe` Success (TH1 NoAeson)

            it "parseJSONList works" $ do
                parse (parseJSONList @NoAeson @TH) [value|["no-aeson", "no-aeson"]|]
                    `shouldBe` Success [TH1 NoAeson, TH1 NoAeson]

            it "toJSON works" $ do
                toJSON @NoAeson (TH1 NoAeson)
                    `shouldBe` [value|"no-aeson"|]

            it "toJSONList works" $ do
                toJSONList @NoAeson [TH1 NoAeson, TH1 NoAeson]
                    `shouldBe` [value|["no-aeson", "no-aeson"]|]

            it "toEncoding works" $ do
                toEncoding @NoAeson (TH1 NoAeson)
                    `shouldBe` [encoding|"no-aeson"|]

            it "toEncodingList works" $ do
                toEncodingList @NoAeson [TH1 NoAeson, TH1 NoAeson]
                    `shouldBe` [encoding|["no-aeson", "no-aeson"]|]

    -- TODO: use more tests from Aeson itself

    -- TODO: test with lists

    -- TODO: test with Foo a = Foo a (will it replace constraints?)

-- | A type that does not have Aeson instances, only @tagged-aeson@
-- instances. Also a tag for @tagged-aeson@ instances.
data NoAeson = NoAeson
    deriving stock (Eq, Show)

instance FromJSON NoAeson NoAeson where
    parseJSON = withText "NoAeson" $ \case
        "no-aeson" -> pure NoAeson
        _ -> fail "expected 'no-aeson'"

instance ToJSON NoAeson NoAeson where
    toJSON NoAeson = using @Aeson (toJSON ("no-aeson" :: Text))

data TH = TH1 NoAeson
    deriving stock (Eq, Show)

deriveJSON [t|NoAeson|] A.defaultOptions ''TH

-- TODO: which instance will be used for lists?
-- TODO: warn that overriding toJSONList and ToJSON [] in different ways will cause trouble
