# tagged-aeson

[![Hackage](https://img.shields.io/hackage/v/tagged-aeson.svg)](https://hackage.haskell.org/package/tagged-aeson)
[![Build status](https://secure.travis-ci.org/monadfix/tagged-aeson.svg)](https://travis-ci.org/monadfix/tagged-aeson)
[![BSD3 license](https://img.shields.io/badge/license-BSD3-blue.svg)](https://github.com/monadfix/tagged-aeson/blob/master/LICENSE)

This thing provides tagged `FromJSON` and `ToJSON` classes and TH generators
that use those instances instead of Aeson's ones. This lets you:

  * Avoid orphan instances when working with types that don't have Aeson instances!

  * Easily express things like “all types should be serialized like usually,
    except for this one which should be serialized differently”!

  * Have several instances (e.g. “old API version”, “new API version”) for
    the same type!

  * Decide what should be the default encoding for `UTCTime` for your API!

  * Prevent silly instances (like Aeson's `Maybe`) from being used at all!
