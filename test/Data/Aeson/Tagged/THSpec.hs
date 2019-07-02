{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Note: all types here must not have Aeson instances.
module Data.Aeson.Tagged.THSpec (spec) where

import BasePrelude
import Data.Aeson.Tagged
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A
import qualified Data.Aeson.Encoding as A
import qualified Data.Aeson.Types as A
import Data.Aeson.Types (Result(..))

import Test.Hspec
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import HaskellWorks.Hspec.Hedgehog

import Utils
import Types

spec :: Spec
spec = describe "Template Haskell deriving" $ do
    -- Note: we are not testing 'deriveFromJSON' and 'deriveToJSON' because
    -- they are more-or-less tested as part of testing 'deriveJSON'
    thSingleSpec
    thEnumSpec
    thADTSpec
    thRecordSpec

-- TODO: use more tests from Aeson itself

-- TODO: other 'Options'
-- TODO: records with optional fields
-- TODO: sumtype record
-- TODO: for fields that are lists, we want to make sure they require and use a [] instance

-- TODO: port all tests from https://github.com/bos/aeson/blob/master/tests/Encoders.hs

-- TODO: which instance will be used for lists?
-- TODO: warn that overriding toJSONList and ToJSON [] in different ways will cause trouble

-- TODO: make sure 'parse2ElemArray' is also exercised

-- TODO: make sure the 'conKey' hack doesn't interfere with parsing of
-- record fields named "conKey"

----------------------------------------------------------------------------
-- Tags
----------------------------------------------------------------------------

-- | A tag for TH-derived instances.
data Derived

-- | A tag for correct instances, either written manually or derived with
-- Aeson's help.
data Golden

----------------------------------------------------------------------------
-- Hedgehog properties
----------------------------------------------------------------------------

-- | Test that JSON generated by a golden instance can be parsed back by a
-- derived instance.
prop_parseJSON
    :: (FromJSON Derived a, ToJSON Golden a, Show a, Eq a)
    => Gen a -> Property
prop_parseJSON gen = property $ do
    a <- forAll gen
    let val = using @Golden (toJSON a)
    parse (parseJSON @Derived) val === Success a

-- | Test that JSON generated by a golden instance can be parsed back by a
-- derived instance via 'parseJSONList'.
prop_parseJSONList
    :: (FromJSON Derived a, ToJSON Golden a, Show a, Eq a)
    => Gen [a] -> Property
prop_parseJSONList gen = property $ do
    a <- forAll gen
    let val = using @Golden (toJSONList a)
    parse (parseJSONList @Derived) val === Success a

-- | Test that JSON generated by a derived instance can be parsed back by a
-- golden instance.
prop_toJSON
    :: (ToJSON Derived a, FromJSON Golden a, Show a, Eq a)
    => Gen a -> Property
prop_toJSON gen = property $ do
    a <- forAll gen
    let val = using @Derived (toJSON a)
    parse (parseJSON @Golden) val === Success a

-- | Test that a JSON list generated by a derived instance can be parsed
-- back by a golden instance.
prop_toJSONList
    :: (ToJSON Derived a, FromJSON Golden a, Show a, Eq a)
    => Gen [a] -> Property
prop_toJSONList gen = property $ do
    a <- forAll gen
    let val = using @Derived (toJSONList a)
    parse (parseJSONList @Golden) val === Success a

-- | Test that 'Encoding' generated by a derived instance can be parsed back
-- by a golden instance.
prop_toEncoding
    :: (ToJSON Derived a, FromJSON Golden a, Show a, Eq a)
    => Gen a -> Property
prop_toEncoding gen = property $ do
    a <- forAll gen
    let bs = coerce A.encodingToLazyByteString (using @Derived (toEncoding a))
        Just val = Value <$> A.decode bs
    parse (parseJSON @Golden) val === Success a

-- | Test that an 'Encoding' list generated by a derived instance can be
-- parsed back by a golden instance.
prop_toEncodingList
    :: (ToJSON Derived a, FromJSON Golden a, Show a, Eq a)
    => Gen [a] -> Property
prop_toEncodingList gen = property $ do
    a <- forAll gen
    let bs = coerce A.encodingToLazyByteString (using @Derived (toEncodingList a))
        Just val = Value <$> A.decode bs
    parse (parseJSONList @Golden) val === Success a

-- | A combination of all tests.
hedgehogSpec
    :: (FromJSON Derived a, ToJSON Derived a,
        FromJSON Golden a, ToJSON Golden a,
        Eq a, Show a)
    => Gen a -> Spec
hedgehogSpec gen = do
    it "derived parseJSON works" $ do
        require $ prop_parseJSON gen
    it "derived parseJSONList works" $ do
        require $ prop_parseJSONList (Gen.list (Range.linear 0 10) gen)
    it "derived toJSON works" $ do
        require $ prop_toJSON gen
    it "derived toJSONList works" $ do
        require $ prop_toJSONList (Gen.list (Range.linear 0 10) gen)
    it "derived toEncoding works" $ do
        require $ prop_toEncoding gen
    it "derived toEncodingList works" $ do
        require $ prop_toEncodingList (Gen.list (Range.linear 0 10) gen)

----------------------------------------------------------------------------
-- Wrapping one field
----------------------------------------------------------------------------

data THSingle a = THSingle a
    deriving stock (Eq, Show)

genTHSingle :: Gen (THSingle Int')
genTHSingle = THSingle <$> genInt'

thSingleSpec :: Spec
thSingleSpec = describe "THSingle (wrapping one field)" $ do
    hedgehogSpec genTHSingle

----------------------------------------------------------------------------
-- Enum
----------------------------------------------------------------------------

data THEnum = THEnum1 | THEnum2
    deriving stock (Eq, Show)

genTHEnum :: Gen THEnum
genTHEnum = Gen.element [THEnum1, THEnum2]

thEnumSpec :: Spec
thEnumSpec = describe "THEnum (enum datatype)" $ do
    hedgehogSpec genTHEnum

----------------------------------------------------------------------------
-- ADT
----------------------------------------------------------------------------

data THADT a b = THADT1 | THADT2 a b
    deriving stock (Eq, Show)

genTHADT :: Gen (THADT Int' Int')
genTHADT = Gen.choice [pure THADT1, THADT2 <$> genInt' <*> genInt']

thADTSpec :: Spec
thADTSpec = describe "THADT (ADT datatype)" $ do
    hedgehogSpec genTHADT

----------------------------------------------------------------------------
-- Record with type variables
----------------------------------------------------------------------------

data THRecord a b = THRecord {fieldA :: a, fieldB :: b}
    deriving stock (Eq, Show)

genTHRecord :: Gen (THRecord Int' Int')
genTHRecord = THRecord <$> genInt' <*> genInt'

thRecordSpec :: Spec
thRecordSpec = describe "THRecord (record with type variables)" $ do
    hedgehogSpec genTHRecord

----------------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------------

deriveJSON [t|Derived|] A.defaultOptions ''THSingle

instance FromJSON Golden (THSingle Int') where
    parseJSON =
        coerce @(A.Value -> A.Parser (THSingle Int))
        $(A.mkParseJSON A.defaultOptions ''THSingle)

instance ToJSON Golden (THSingle Int') where
    toJSON =
        coerce @(THSingle Int -> A.Value)
        $(A.mkToJSON A.defaultOptions ''THSingle)
    toEncoding =
        coerce @(THSingle Int -> A.Encoding)
        $(A.mkToEncoding A.defaultOptions ''THSingle)

--

deriveJSON [t|Derived|] A.defaultOptions ''THEnum

instance FromJSON Golden THEnum where
    parseJSON =
        coerce @(A.Value -> A.Parser THEnum)
        $(A.mkParseJSON A.defaultOptions ''THEnum)

instance ToJSON Golden THEnum where
    toJSON =
        coerce @(THEnum -> A.Value)
        $(A.mkToJSON A.defaultOptions ''THEnum)
    toEncoding =
        coerce @(THEnum -> A.Encoding)
        $(A.mkToEncoding A.defaultOptions ''THEnum)

--

deriveJSON [t|Derived|] A.defaultOptions ''THADT

instance FromJSON Golden (THADT Int' Int') where
    parseJSON =
        coerce @(A.Value -> A.Parser (THADT Int Int))
        $(A.mkParseJSON A.defaultOptions ''THADT)

instance ToJSON Golden (THADT Int' Int') where
    toJSON =
        coerce @(THADT Int Int -> A.Value)
        $(A.mkToJSON A.defaultOptions ''THADT)
    toEncoding =
        coerce @(THADT Int Int -> A.Encoding)
        $(A.mkToEncoding A.defaultOptions ''THADT)

--

deriveJSON [t|Derived|] A.defaultOptions ''THRecord

instance FromJSON Golden (THRecord Int' Int') where
    parseJSON =
        coerce @(A.Value -> A.Parser (THRecord Int Int))
        $(A.mkParseJSON A.defaultOptions ''THRecord)

instance ToJSON Golden (THRecord Int' Int') where
    toJSON =
        coerce @(THRecord Int Int -> A.Value)
        $(A.mkToJSON A.defaultOptions ''THRecord)
    toEncoding =
        coerce @(THRecord Int Int -> A.Encoding)
        $(A.mkToEncoding A.defaultOptions ''THRecord)
