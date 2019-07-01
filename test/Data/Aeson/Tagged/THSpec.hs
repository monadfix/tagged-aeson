{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Aeson.Tagged.THSpec (spec) where

import BasePrelude
import Data.Aeson.Tagged
import qualified Data.Aeson as A
import Data.Aeson.Types (Result(..))

import Test.Hspec
import Utils
import Types

spec :: Spec
spec = describe "Template Haskell deriving" $ do
    -- Note: we are not testing 'deriveFromJSON' and 'deriveToJSON' because
    -- they are more-or-less tested as part of testing 'deriveJSON'
    thSingleSpec
    thListSpec
    thEnumSpec
    thADTSpec
    thRecordSpec

-- TODO: use more tests from Aeson itself

-- TODO: other 'Options'
-- TODO: records with optional fields
-- TODO: sumtype record

-- TODO: which instance will be used for lists?
-- TODO: warn that overriding toJSONList and ToJSON [] in different ways will cause trouble

-- TODO: make sure 'parse2ElemArray' is also exercised

-- TODO: make sure the 'conKey' hack doesn't interfere with parsing of
-- record fields named "conKey"

----------------------------------------------------------------------------
-- Wrapping one field
----------------------------------------------------------------------------

data THSingle = THSingle Int'
    deriving stock (Eq, Show)

thSingleSpec :: Spec
thSingleSpec = describe "THSingle (wrapping one field)" $ do
    it "derived parseJSON works" $ do
        parse (parseJSON @NoAeson) [value|1|]
            `shouldBe` Success (THSingle 1)

    it "derived parseJSONList works" $ do
        parse (parseJSONList @NoAeson) [value|[1,2]|]
            `shouldBe` Success [THSingle 1, THSingle 2]

    it "derived toJSON works" $ do
        toJSON @NoAeson (THSingle 1)
            `shouldBe` [value|1|]

    it "derived toJSONList works" $ do
        toJSONList @NoAeson [THSingle 1, THSingle 2]
            `shouldBe` [value|[1,2]|]

    it "derived toEncoding works" $ do
        toEncoding @NoAeson (THSingle 1)
            `shouldBe` [encoding|1|]

    it "derived toEncodingList works" $ do
        toEncodingList @NoAeson [THSingle 1, THSingle 2]
            `shouldBe` [encoding|[1,2]|]

----------------------------------------------------------------------------
-- Wrapping a list
----------------------------------------------------------------------------

data THList = THList [Int']
    deriving stock (Eq, Show)

-- TODO: test that it actually uses the list instance

thListSpec :: Spec
thListSpec = describe "THList (wrapping one field with a list)" $ do
    it "derived parseJSON works" $ do
        parse (parseJSON @NoAeson) [value|[1]|]
            `shouldBe` Success (THList [1])

    it "derived parseJSONList works" $ do
        parse (parseJSONList @NoAeson) [value|[[1],[2,3]]|]
            `shouldBe` Success [THList [1], THList [2, 3]]

    it "derived toJSON works" $ do
        toJSON @NoAeson (THList [1])
            `shouldBe` [value|[1]|]

    it "derived toJSONList works" $ do
        toJSONList @NoAeson [THList [1], THList [2, 3]]
            `shouldBe` [value|[[1],[2,3]]|]

    it "derived toEncoding works" $ do
        toEncoding @NoAeson (THList [1])
            `shouldBe` [encoding|[1]|]

    it "derived toEncodingList works" $ do
        toEncodingList @NoAeson [THList [1], THList [2, 3]]
            `shouldBe` [encoding|[[1],[2,3]]|]

----------------------------------------------------------------------------
-- Enum
----------------------------------------------------------------------------

data THEnum = THEnum1 | THEnum2
    deriving stock (Eq, Show)

thEnumSpec :: Spec
thEnumSpec = describe "THEnum (enum datatype)" $ do
    it "derived parseJSON works" $ do
        parse (parseJSON @NoAeson) [value|"THEnum1"|]
            `shouldBe` Success THEnum1

    it "derived parseJSONList works" $ do
        parse (parseJSONList @NoAeson) [value|["THEnum1", "THEnum2"]|]
            `shouldBe` Success [THEnum1, THEnum2]

    it "derived toJSON works" $ do
        toJSON @NoAeson THEnum1
            `shouldBe` [value|"THEnum1"|]

    it "derived toJSONList works" $ do
        toJSONList @NoAeson [THEnum1, THEnum2]
            `shouldBe` [value|["THEnum1", "THEnum2"]|]

    it "derived toEncoding works" $ do
        toEncoding @NoAeson THEnum1
            `shouldBe` [encoding|"THEnum1"|]

    it "derived toEncodingList works" $ do
        toEncodingList @NoAeson [THEnum1, THEnum2]
            `shouldBe` [encoding|["THEnum1", "THEnum2"]|]

----------------------------------------------------------------------------
-- ADT
----------------------------------------------------------------------------

data THADT = THADT1 | THADT2 Int' Int'
    deriving stock (Eq, Show)

thADTSpec :: Spec
thADTSpec = describe "THADT (ADT datatype)" $ do
    it "derived parseJSON works" $ do
        parse (parseJSON @NoAeson) [value|{"tag":"THADT1"}|]
            `shouldBe` Success THADT1
        parse (parseJSON @NoAeson) [value|{"tag":"THADT2","contents":[1,2]}|]
            `shouldBe` Success (THADT2 1 2)

    it "derived parseJSONList works" $ do
        parse (parseJSONList @NoAeson)
              [value|[{"tag":"THADT1"},{"tag":"THADT2","contents":[1,2]}]|]
            `shouldBe` Success [THADT1, THADT2 1 2]

    it "derived toJSON works" $ do
        toJSON @NoAeson THADT1
            `shouldBe` [value|{"tag":"THADT1"}|]

    it "derived toJSONList works" $ do
        toJSONList @NoAeson [THADT1, THADT2 1 2]
            `shouldBe` [value|[{"tag":"THADT1"},{"tag":"THADT2","contents":[1,2]}]|]

    it "derived toEncoding works" $ do
        toEncoding @NoAeson THADT1
            `shouldBe` [encoding|{"tag":"THADT1"}|]

    it "derived toEncodingList works" $ do
        toEncodingList @NoAeson [THADT1, THADT2 1 2]
            `shouldBe` [encoding|[{"tag":"THADT1"},{"tag":"THADT2","contents":[1,2]}]|]

----------------------------------------------------------------------------
-- Record with type variables
----------------------------------------------------------------------------

data THRecord a = THRecord {fieldA :: a, fieldB :: Int'}
    deriving stock (Eq, Show)

thRecordSpec :: Spec
thRecordSpec = describe "THRecord (record with type variables)" $ do
    it "derived parseJSON works" $ do
        parse (parseJSON @NoAeson) [value|{"fieldA":1,"fieldB":2}|]
            `shouldBe` Success (THRecord 1 2 :: THRecord Int')

    it "derived parseJSONList works" $ do
        parse (parseJSONList @NoAeson) [value|[{"fieldA":1,"fieldB":2}]|]
            `shouldBe` Success [THRecord 1 2 :: THRecord Int']

    it "derived toJSON works" $ do
        toJSON @NoAeson (THRecord 1 2 :: THRecord Int')
            `shouldBe` [value|{"fieldA":1,"fieldB":2}|]

    it "derived toJSONList works" $ do
        toJSONList @NoAeson [THRecord 1 2 :: THRecord Int']
            `shouldBe` [value|[{"fieldA":1,"fieldB":2}]|]

    it "derived toEncoding works" $ do
        toEncoding @NoAeson (THRecord 1 2 :: THRecord Int')
            `shouldBe` [encoding|{"fieldA":1,"fieldB":2}|]

    it "derived toEncodingList works" $ do
        toEncodingList @NoAeson [THRecord 1 2 :: THRecord Int']
            `shouldBe` [encoding|[{"fieldA":1,"fieldB":2}]|]

----------------------------------------------------------------------------
-- Instance derivation
----------------------------------------------------------------------------

deriveJSON [t|NoAeson|] A.defaultOptions ''THSingle
deriveJSON [t|NoAeson|] A.defaultOptions ''THList
deriveJSON [t|NoAeson|] A.defaultOptions ''THEnum
deriveJSON [t|NoAeson|] A.defaultOptions ''THADT
deriveJSON [t|NoAeson|] A.defaultOptions ''THRecord
