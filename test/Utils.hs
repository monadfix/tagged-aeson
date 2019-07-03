{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utils
(
    parse,
    parseEither,
    value,
    encoding,

    -- * Options
    opts2ElemArray,
    optsTaggedObject,
    optsObjectWithSingleField,
    optsTagSingleConstructors,
    -- ** 'Flavor'
    Flavor(..),
    SFlavor(..),
    mkParseJSONFlavor,
    mkToJSONFlavor,
    mkToEncodingFlavor,
)
where

import BasePrelude
import Data.Aeson.Tagged
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.Aeson.TH as A
import Language.Haskell.TH.Quote
import Language.Haskell.TH

parse :: forall tag b a. (a -> Parser tag b) -> a -> A.Result b
parse = coerce (A.parse @a @b)

parseEither :: forall tag b a. (a -> Parser tag b) -> a -> Either String b
parseEither = coerce (A.parseEither @a @b)

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
--
-- See https://github.com/bos/aeson/blob/master/tests/Options.hs
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

optsTagSingleConstructors :: A.Options
optsTagSingleConstructors = optsBase
    { A.tagSingleConstructors = True
    , A.allNullaryToStringTag = False
    }

----------------------------------------------------------------------------
-- Flavor
----------------------------------------------------------------------------

data Flavor
    = FDefault
    | F2ElemArray
    | FTaggedObject
    | FObjectWithSingleField
    | FTagSingleConstructors

class SFlavor (k :: Flavor) where flavor :: Flavor
instance SFlavor 'FDefault where flavor = FDefault
instance SFlavor 'F2ElemArray where flavor = F2ElemArray
instance SFlavor 'FTaggedObject where flavor = FTaggedObject
instance SFlavor 'FObjectWithSingleField where flavor = FObjectWithSingleField
instance SFlavor 'FTagSingleConstructors where flavor = FTagSingleConstructors

mkParseJSONFlavor :: Name -> Q Exp
mkParseJSONFlavor name =
    [| \case
            FDefault -> $(A.mkParseJSON A.defaultOptions name)
            F2ElemArray -> $(A.mkParseJSON opts2ElemArray name)
            FTaggedObject -> $(A.mkParseJSON optsTaggedObject name)
            FObjectWithSingleField -> $(A.mkParseJSON optsObjectWithSingleField name)
            FTagSingleConstructors -> $(A.mkParseJSON optsTagSingleConstructors name)
     |]

mkToJSONFlavor :: Name -> Q Exp
mkToJSONFlavor name =
    [| \case
            FDefault -> $(A.mkToJSON A.defaultOptions name)
            F2ElemArray -> $(A.mkToJSON opts2ElemArray name)
            FTaggedObject -> $(A.mkToJSON optsTaggedObject name)
            FObjectWithSingleField -> $(A.mkToJSON optsObjectWithSingleField name)
            FTagSingleConstructors -> $(A.mkToJSON optsTagSingleConstructors name)
     |]

mkToEncodingFlavor :: Name -> Q Exp
mkToEncodingFlavor name =
    [| \case
            FDefault -> $(A.mkToEncoding A.defaultOptions name)
            F2ElemArray -> $(A.mkToEncoding opts2ElemArray name)
            FTaggedObject -> $(A.mkToEncoding optsTaggedObject name)
            FObjectWithSingleField -> $(A.mkToEncoding optsObjectWithSingleField name)
            FTagSingleConstructors -> $(A.mkToEncoding optsTagSingleConstructors name)
     |]
