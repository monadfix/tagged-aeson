{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utils
(
    parse,
    value,
    encoding,

    -- * Options
    opts2ElemArray,
    optsTaggedObject,
    optsObjectWithSingleField,
)
where

import BasePrelude
import Data.Aeson.Tagged
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Language.Haskell.TH.Quote

parse :: forall tag b a. (a -> Parser tag b) -> a -> A.Result b
parse = coerce (A.parse @a @b)

-- | 'Value' quasi-quoter.
value :: QuasiQuoter
value = QuasiQuoter
    { quoteExp = \txt ->
          case A.eitherDecodeStrict $ T.encodeUtf8 $ T.pack txt of
            Left err -> error $ "Error in 'json': " ++ show err
            Right (val :: A.Value) -> [|Value val|]
    , quotePat  = const $ error "No quotePat defined for json"
    , quoteType = const $ error "No quoteType defined for json"
    , quoteDec  = const $ error "No quoteDec defined for json"
    }

-- | Encoding quasi-quoter.
encoding :: QuasiQuoter
encoding = QuasiQuoter
    { quoteExp = \txt ->
          case A.eitherDecodeStrict $ T.encodeUtf8 $ T.pack txt of
            Left err  -> error $ "Error in 'encoding': " ++ show err
            Right (val :: A.Value) -> [|Encoding (A.toEncoding val)|]
    , quotePat  = const $ error "No quotePat defined for encoding"
    , quoteType = const $ error "No quoteType defined for encoding"
    , quoteDec  = const $ error "No quoteDec defined for encoding"
    }

----------------------------------------------------------------------------
-- Options
----------------------------------------------------------------------------

optsBase :: A.Options
optsBase = A.defaultOptions
    { A.fieldLabelModifier     = map toLower
    , A.constructorTagModifier = map toLower
    }

opts2ElemArray :: A.Options
opts2ElemArray = optsBase
    { A.allNullaryToStringTag = False
    , A.sumEncoding = A.TwoElemArray
    }

optsTaggedObject :: A.Options
optsTaggedObject = optsBase
    { A.allNullaryToStringTag = False
    }

optsObjectWithSingleField :: A.Options
optsObjectWithSingleField = optsBase
    { A.allNullaryToStringTag = False
    , A.sumEncoding           = A.ObjectWithSingleField
    }
