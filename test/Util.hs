{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS_GHC -Wno-unused-foralls #-}  -- bogus warning for 'j'

module Util
(
    parse,
    j,
)
where

import BasePrelude
import Data.Aeson.Tagged
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Aeson          as A
import qualified Data.Aeson.Types    as A
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Lift

parse :: forall tag b a. (a -> Parser tag b) -> a -> A.Result b
parse = coerce (A.parse @a @b)

-- | JSON quasi-quoter.
j :: forall tag. QuasiQuoter
j = QuasiQuoter
    { quoteExp = \txt ->
          case A.eitherDecodeStrict $ T.encodeUtf8 $ T.pack txt of
            Left err  -> error $ "Error in j: " ++ show err
            Right val -> lift (val :: Value tag)
    , quotePat  = const $ error "No quotePat defined for j"
    , quoteType = const $ error "No quoteType defined for j"
    , quoteDec  = const $ error "No quoteDec defined for j"
    }
