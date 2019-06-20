# tagged-aeson

[![Hackage](https://img.shields.io/hackage/v/tagged-aeson.svg)](https://hackage.haskell.org/package/tagged-aeson)
[![Build status](https://secure.travis-ci.com/monadfix/tagged-aeson.svg)](https://travis-ci.com/monadfix/tagged-aeson)
[![BSD3 license](https://img.shields.io/badge/license-BSD3-blue.svg)](https://github.com/monadfix/tagged-aeson/blob/master/LICENSE)

`tagged-aeson` provides tagged `FromJSON` and `ToJSON` classes and TH
generators that use those instances instead of Aeson's ones.

Any function expecting a normal `FromJSON` or `ToJSON` constraint can work
with `tagged-aeson` instances by means of the `TaggedAeson` newtype wrapper.

## Usecase: avoid orphan instances

You have a [`URI`][URI] in your config type and you want to autoderive a
`FromJSON` instance for the config. Before, you'd write an orphan instance:

[URI]: http://hackage.haskell.org/package/network-uri/docs/Network-URI.html#t:URI

```haskell
{-# OPTIONS_GHC -Wno-orphans #-}

instance FromJSON URI where
    parseJSON = withText "URI" $
        maybe (fail "invalid URI") return . parseURIReference . unpack

data Config = { ... }
deriveFromJSON defaultOptions ''Config

decodeConfig :: ByteString -> Maybe Config
decodeConfig = decode
```

With `tagged-aeson`, you can write a non-orphan instance – and only use it
for `FromJSON Config` without letting it escape into the rest of your
program:

```haskell
instance FromJSON Config URI where
    parseJSON = withText "URI" $
        maybe (fail "invalid URI") return . parseURIReference . unpack

data Config = { ... }
deriveFromJSON ''Config defaultOptions ''Config

decodeConfig :: ByteString -> Maybe Config
decodeConfig = fmap (fromTaggedAeson @Config) . decode
```

## Usecase: mark some instances as dangerous

You have a request type with a plaintext password in one of the fields. You
are prudent and don't have a `ToJSON` instance for your `PlainTextPassword`
type – after all, you only need to parse requests, not generate them.

That is, until you start writing tests.

Before: you would define orphan instances for all your request types in the
testsuite. Or you would write functions `someRequestToJSON`,
`otherRequestToJSON`, etc, and import them when necessary.

After:

```haskell
data TestOnly a

instance FromJSON Api PlainTextPassword
instance ToJSON (TestOnly Api) PlainTextPassword

instance FromJSON Api SomeRequest
instance ToJSON (TestOnly Api) SomeRequest
```

## Usecase: versioned APIs

Coming soon!

## Usecase: modify default Aeson instances

You are working with a weird API that represents `UTCTime` as
`/Date(1302547608878)/`. Aeson provides a newtype to handle this case, but
sometimes you forget to use the newtype.

With `tagged-aeson`, you can change the instance:

```
data WeirdAPI

instance FromJSON WeirdAPI UTCTime where
    parseJSON = fmap fromDotNetTime . using @Aeson parseJSON
```

## Usecase: destroy default Aeson instances

You [really don't like](https://github.com/bos/aeson/issues/376) Aeson
instances for `Maybe`.

The solution is: don't write them! Just don't have them. Ever. With
`tagged-aeson`, you can. Or you can write them – but add a
[`TypeError`][TypeError] so that nobody would be able to use them.

[TypeError]: https://hackage.haskell.org/package/base/docs/GHC-TypeLits.html#t:TypeError

Before: despair.

After: bliss.
