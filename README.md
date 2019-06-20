# tagged-aeson

[![Hackage](https://img.shields.io/hackage/v/tagged-aeson.svg)](https://hackage.haskell.org/package/tagged-aeson)
[![Build status](https://travis-ci.com/monadfix/tagged-aeson.svg?branch=master)](https://travis-ci.com/monadfix/tagged-aeson)
[![BSD3 license](https://img.shields.io/badge/license-BSD3-blue.svg)](https://github.com/monadfix/tagged-aeson/blob/master/LICENSE)

`tagged-aeson` provides tagged `FromJSON` and `ToJSON` classes and TH
generators that use those instances instead of Aeson's ones.

Any function expecting a normal `FromJSON` or `ToJSON` constraint can work
with `tagged-aeson` instances by means of the `TaggedAeson` newtype wrapper.

## Is it usable already?

Somewhat, but it's better to wait until an official release. The API is in
flux, and feedback is welcome!

_See also: [monadfix/jijo](https://github.com/monadfix/jijo), a more radical
instance-less approach to bidirectional JSON encoding/decoding._

## Usecase: avoid orphan instances

You have a [`URI`][URI] in your config type and you want to autoderive a
`FromJSON` instance for the config. Without `tagged-aeson`, you'd write an
orphan instance:

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

Without `tagged-aeson`: you would define orphan instances for all your
request types in the testsuite. Or you would write functions
`someRequestToJSON`, `otherRequestToJSON`, etc, and import them when
necessary.

With `tagged-aeson`:

```haskell
data TestOnly a

instance FromJSON Api PlainTextPassword
instance ToJSON (TestOnly Api) PlainTextPassword

instance FromJSON Api SomeRequest
instance ToJSON (TestOnly Api) SomeRequest
```

## Usecase: versioned APIs

Coming soon!

## Usecase: modify Aeson-provided instances

You are working with a weird API that represents `UTCTime` as
`/Date(1302547608878)/`. By default, the serialization for `UTCTime` is
not what you want, but Aeson provides a newtype to handle this case:

```haskell
data User = User { name :: Text, created :: UTCTime }

instance FromJSON User where
  parseJSON = withObject "User" $ \o ->
    name    <- o .: "name"
    created <- fromDotNetTime <$> o .: "created"
    pure User{..}
```

However, you can forget to use the newtype wrapper, leading to hard-to-find
bugs. With `tagged-aeson`, though, you can just change the instance:

```
data WeirdAPI

instance FromJSON WeirdAPI User where
  parseJSON = withObject "User" $ \o ->
    name    <- o .: "name"
    created <- o .: "created"
    pure User{..}

instance FromJSON WeirdAPI UTCTime where
    parseJSON = fmap fromDotNetTime . using @Aeson parseJSON
```

You can also write a stub instance with a [`TypeError`][TypeError] and
direct users towards one of several existing newtypes.

[TypeError]: https://hackage.haskell.org/package/base/docs/GHC-TypeLits.html#t:TypeError

## Usecase: burn down Aeson-provided instances

Let's say you [really don't like](https://github.com/bos/aeson/issues/376)
Aeson instances for `Maybe`.

`tagged-aeson` provides no built-in instances; you can always lift instances
from Aeson, but you don't have to. Or you can write a stub instance – and,
again, add a [`TypeError`][TypeError] so that nobody would be able to use
it. At last!
