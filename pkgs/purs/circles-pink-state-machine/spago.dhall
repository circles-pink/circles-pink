{ name = "circles-pink-state-machine"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "argonaut"
  , "arrays"
  , "bifunctors"
  , "chance"
  , "checked-exceptions"
  , "console"
  , "datetime"
  , "debug"
  , "debug-extra"
  , "dot-language"
  , "effect"
  , "either"
  , "errors"
  , "eth-core"
  , "exceptions"
  , "foldable-traversable"
  , "foreign"
  , "foreign-object"
  , "fp-ts"
  , "functions"
  , "graphql-client"
  , "heterogeneous"
  , "http-methods"
  , "identity"
  , "indexed-graph"
  , "integers"
  , "js-timers"
  , "maybe"
  , "milkis"
  , "newtype"
  , "node-buffer"
  , "node-child-process"
  , "node-fs"
  , "node-fs-aff"
  , "node-process"
  , "now"
  , "nullable"
  , "numbers"
  , "optparse"
  , "ordered-collections"
  , "pairs"
  , "parallel"
  , "parsing"
  , "partial"
  , "payload"
  , "prelude"
  , "psci-support"
  , "record"
  , "simple-json"
  , "spec"
  , "stadium"
  , "strings"
  , "sunde"
  , "test-unit"
  , "these"
  , "transformers"
  , "tuples"
  , "typedenv"
  , "typelevel"
  , "typelevel-lists"
  , "typelevel-prelude"
  , "undefined"
  , "unfoldable"
  , "unsafe-coerce"
  , "uri"
  , "variant"
  , "web-html"
  , "web-storage"
  , "web3"
  ]
, packages = ../../../packages.dhall
, sources = [ "./src/**/*.purs", "./test/**/*.purs" ]
}
