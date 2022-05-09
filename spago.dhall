{ name = "circles-statemachine"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "argonaut"
  , "arrays"
  , "bifunctors"
  , "checked-exceptions"
  , "console"
  , "datetime"
  , "debug"
  , "dot-language"
  , "effect"
  , "either"
  , "eth-core"
  , "exceptions"
  , "foldable-traversable"
  , "foreign"
  , "foreign-object"
  , "functions"
  , "http-methods"
  , "identity"
  , "js-timers"
  , "maybe"
  , "milkis"
  , "newtype"
  , "node-buffer"
  , "node-fs"
  , "node-process"
  , "now"
  , "optparse"
  , "ordered-collections"
  , "prelude"
  , "psci-support"
  , "record"
  , "spec"
  , "stadium"
  , "strings"
  , "test-unit"
  , "transformers"
  , "tuples"
  , "typedenv"
  , "typelevel"
  , "typelevel-lists"
  , "typelevel-prelude"
  , "undefined"
  , "unordered-collections"
  , "unsafe-coerce"
  , "uri"
  , "variant"
  , "web3"
  ]
, packages = ./packages.dhall
, sources = [ "./pkgs/purs/src/**/*.purs", "./pkgs/purs/test/**/*.purs" ]
}
