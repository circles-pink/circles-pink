{ name = "all-tests"
, dependencies =
  [ "aff"
  , "debug-extra"
  , "effect"
  , "foldable-traversable"
  , "maybe"
  , "ordered-collections"
  , "prelude"
  , "spec"
  , "spec-discovery"
  , "tuples"
  ]
, packages = ../../../packages.dhall
, sources = [ "./src/**/*.purs", "./test/**/*.purs" ]
}
