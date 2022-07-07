{ name = "graph"
, dependencies =
  [ "aff"
  , "arrays"
  , "debug-extra"
  , "effect"
  , "foldable-traversable"
  , "maybe"
  , "ordered-collections"
  , "pairs"
  , "partial"
  , "prelude"
  , "spec"
  , "spec-discovery"
  , "tuples"
  , "unfoldable"
  ]
, packages = ../../../packages.dhall
, sources = [ "./src/**/*.purs", "./test/**/*.purs" ]
}
