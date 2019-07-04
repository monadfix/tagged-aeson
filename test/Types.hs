{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Types for use in tests.
module Types
(
    Int'(..), genInt',
    Bool'(..), genBool',
    Text'(..), genText',
    withLocalAesonInstances,
)
where

import BasePrelude hiding ((\\))
import Data.Text (Text)
import Data.Aeson.Tagged
import Data.Hashable
import qualified Data.Aeson as A
import Data.Constraint
import Data.Constraint.Unsafe

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
-- Bool'
----------------------------------------------------------------------------

-- | A 'Bool' wrapper without Aeson instances and with @tagged-aeson@
-- instances for all tags.
newtype Bool' = Bool' Bool
    deriving stock (Eq, Ord, Show)
    deriving newtype (Hashable)

deriving via WithAeson Bool instance FromJSON any Bool'
deriving via WithAeson Bool instance ToJSON any Bool'

genBool' :: Gen Bool'
genBool' = Bool' <$> Gen.bool

----------------------------------------------------------------------------
-- Text'
----------------------------------------------------------------------------

-- | A 'Text' wrapper without Aeson instances and with @tagged-aeson@
-- instances for all tags.
newtype Text' = Text' Text
    deriving stock (Eq, Ord, Show)
    deriving newtype (Hashable)

deriving via WithAeson Text instance FromJSON any Text'
deriving via WithAeson Text instance ToJSON any Text'

genText' :: Gen Text'
genText' = Text' <$> Gen.text (Range.linear 0 10) Gen.unicode

----------------------------------------------------------------------------
-- Local instances
----------------------------------------------------------------------------

-- | Provide local Aeson instances for 'Int'', 'Bool'', 'Text''.
withLocalAesonInstances
    :: ((A.FromJSON Int', A.ToJSON Int',
         A.FromJSON Bool', A.ToJSON Bool',
         A.FromJSON Text', A.ToJSON Text') => a) -> a
withLocalAesonInstances =
    (\\ (unsafeDerive Text' :: A.ToJSON Text :- A.ToJSON Text')) .
    (\\ (unsafeDerive Text' :: A.FromJSON Text :- A.FromJSON Text')) .
    (\\ (unsafeDerive Bool' :: A.ToJSON Bool :- A.ToJSON Bool')) .
    (\\ (unsafeDerive Bool' :: A.FromJSON Bool :- A.FromJSON Bool')) .
    (\\ (unsafeDerive Int' :: A.ToJSON Int :- A.ToJSON Int')) .
    (\\ (unsafeDerive Int' :: A.FromJSON Int :- A.FromJSON Int'))
