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
)
where

import BasePrelude
import Data.Aeson.Tagged
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
