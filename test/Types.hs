{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Types
(
    Modded,
    NoAeson,
    Int'(..),
)
where

import BasePrelude
import Data.Vector (Vector)
import Data.Set (Set)
import Data.HashSet (HashSet)
import Data.Text as T
import Data.Aeson.Tagged
import qualified Data.Aeson as A
import qualified GHC.TypeLits as TypeLits

----------------------------------------------------------------------------
-- Modded
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
-- NoAeson
----------------------------------------------------------------------------

-- | A tag for types that don't have Aeson instances, only @tagged-aeson@
-- instances.
data NoAeson

newtype Int' = Int' Int
    deriving stock (Eq, Show)
    deriving newtype (Num)

instance TypeLits.TypeError ('TypeLits.Text "Int' does not have Aeson instances")
      => A.FromJSON Int' where
    parseJSON = undefined
instance TypeLits.TypeError ('TypeLits.Text "Int' does not have Aeson instances")
      => A.ToJSON Int' where
    toJSON = undefined

deriving via WithAeson Int instance FromJSON NoAeson Int'
deriving via WithAeson Int instance ToJSON NoAeson Int'

deriving via WithAeson [Int] instance FromJSON NoAeson [Int']
deriving via WithAeson [Int] instance ToJSON NoAeson [Int']
