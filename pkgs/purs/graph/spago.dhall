{ name = "graph"
, dependencies =
  [ "aff"
  , "arrays"
  , "debug-extra"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "maybe"
  , "ordered-collections"
  , "pairs"
  , "partial"
  , "prelude"
  , "spec"
  , "spec-discovery"
  , "tuples"
  , "typelevel-prelude"
  , "unfoldable"
  , "variant"
  ]
, packages = ../../../packages.dhall
, sources = [ "./src/**/*.purs", "./test/**/*.purs" ]
}
