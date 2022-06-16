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
    , version = "1485a038f602f1e02841be26abe88f47486b6543"
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
  with chance = ./pkgs/purs/chance/spago.dhall as Location
