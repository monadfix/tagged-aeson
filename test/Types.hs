{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Types and tags for use in tests.
module Types
(
    Int'(..), genInt',
    Modded,
)
where

import BasePrelude
import Data.Vector (Vector)
import Data.Set (Set)
import Data.HashSet (HashSet)
import Data.Text as T
import Data.Aeson.Tagged
import qualified GHC.TypeLits as TypeLits
import Data.Hashable

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

----------------------------------------------------------------------------
-- Int'
----------------------------------------------------------------------------

-- | An 'Int' wrapper without Aeson instances and with @tagged-aeson@
-- instances for all tags.
newtype Int' = Int' Int
    deriving stock (Eq, Ord, Show)
    deriving newtype (Num, Hashable)

deriving via WithAeson Int instance FromJSON any Int'
deriving via WithAeson Int instance ToJSON any Int'

genInt' :: Gen Int'
genInt' = Int' <$> Gen.int (Range.linear 0 1000)

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
