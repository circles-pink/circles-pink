{-
Welcome to your new Dhall package-set!

Below are instructions for how to edit this file for most use
cases, so that you don't need to know Dhall to use it.

## Use Cases

Most will want to do one or both of these options:
1. Override/Patch a package's dependency
2. Add a package not already in the default package set

This file will continue to work whether you use one or both options.
Instructions for each option are explained below.

### Overriding/Patching a package

Purpose:
- Change a package's dependency to a newer/older release than the
    default package set's release
- Use your own modified version of some dependency that may
    include new API, changed API, removed API by
    using your custom git repo of the library rather than
    the package set's repo

Syntax:
where `entityName` is one of the following:
- dependencies
- repo
- version
-------------------------------
let upstream = --
in  upstream
  with packageName.entityName = "new value"
-------------------------------

Example:
-------------------------------
let upstream = --
in  upstream
  with halogen.version = "master"
  with halogen.repo = "https://example.com/path/to/git/repo.git"

  with halogen-vdom.version = "v4.0.0"
  with halogen-vdom.dependencies = [ "extra-dependency" ] # halogen-vdom.dependencies
-------------------------------

### Additions

Purpose:
- Add packages that aren't already included in the default package set

Syntax:
where `<version>` is:
- a tag (i.e. "v4.0.0")
- a branch (i.e. "master")
- commit hash (i.e. "701f3e44aafb1a6459281714858fadf2c4c2a977")
-------------------------------
let upstream = --
in  upstream
  with new-package-name =
    { dependencies =
       [ "dependency1"
       , "dependency2"
       ]
    , repo =
       "https://example.com/path/to/git/repo.git"
    , version =
        "<version>"
    }
-------------------------------

Example:
-------------------------------
let upstream = --
in  upstream
  with benchotron =
      { dependencies =
          [ "arrays"
          , "exists"
          , "profunctor"
          , "strings"
          , "quickcheck"
          , "lcg"
          , "transformers"
          , "foldable-traversable"
          , "exceptions"
          , "node-fs"
          , "node-buffer"
          , "node-readline"
          , "datetime"
          , "now"
          ]
      , repo =
          "https://github.com/hdgarrood/purescript-benchotron.git"
      , version =
          "v7.0.0"
      }
-------------------------------
-}
let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.5-20220110/packages.dhall
        sha256:8dbf71bfc6c7a11043619eebe90ff85f7d884541048aa8cc48eef1ee781cbc0e

in  upstream
  with stadium =
    { dependencies =
      [ "arrays"
      , "console"
      , "effect"
      , "foldable-traversable"
      , "maybe"
      , "node-buffer"
      , "node-fs"
      , "prelude"
      , "psci-support"
      , "test-unit"
      , "tuples"
      , "type-equality"
      , "typelevel-lists"
      , "typelevel-prelude"
      , "undefined"
      , "unsafe-coerce"
      , "variant"
      , "dot-language"
      ]
    , repo = "https://github.com/circles-pink/purescript-stadium.git"
    , version = "7aa2ace1791f9ef02ecff5f9205759a74b5c37fe"
    }
  with dot-language =
    { dependencies =
      [ "arrays"
      , "console"
      , "effect"
      , "foldable-traversable"
      , "maybe"
      , "node-buffer"
      , "node-fs"
      , "prelude"
      , "psci-support"
      , "test-unit"
      , "tuples"
      , "type-equality"
      , "typelevel-lists"
      , "typelevel-prelude"
      , "undefined"
      , "unsafe-coerce"
      , "variant"
      ]
    , repo = "https://github.com/thought2/purescript-dot-language.git"
    , version = "c3a6a831349b4ff2acb5a789557cefd7ca5d1ecd"
    }
  with coroutine-transducers =
    { dependencies = [ "aff", "coroutines", "effect", "maybe", "psci-support" ]
    , repo = "https://github.com/blinky3713/purescript-coroutine-transducers"
    , version = "v1.0.0"
    }
  with eth-core =
    { dependencies =
      [ "argonaut"
      , "bytestrings"
      , "console"
      , "debug"
      , "effect"
      , "foreign-generic"
      , "ordered-collections"
      , "parsing"
      , "prelude"
      , "psci-support"
      , "ring-modules"
      , "simple-json"
      ]
    , repo = "https://github.com/f-o-a-m/purescript-eth-core.git"
    , version = "v8.0.0"
    }
  with tagged =
    { dependencies = [ "identity", "profunctor" ]
    , repo = "https://github.com/kejace/purescript-tagged"
    , version = "d34cf9c65e4da086173e4be6c8888319b5ed07c2"
    }
  with web3 =
    { dependencies =
      [ "free"
      , "bytestrings"
      , "foreign-generic"
      , "profunctor-lenses"
      , "fork"
      , "parsing"
      , "coroutines"
      , "aff"
      , "ring-modules"
      , "typelevel-prelude"
      , "tagged"
      , "eth-core"
      , "transformers"
      , "errors"
      , "partial"
      , "identity"
      , "foreign"
      , "coroutine-transducers"
      , "eth-core"
      , "heterogeneous"
      ]
    , repo = "https://github.com/f-o-a-m/purescript-web3"
    , version = "v5.0.0"
    }
