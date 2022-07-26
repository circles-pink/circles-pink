{ name = "graph"
, dependencies =
  [ "aff"
  , "arrays"
  , "debug-extra"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "free"
  , "maybe"
  , "ordered-collections"
  , "pairs"
  , "partial"
  , "prelude"
  , "profunctor-lenses"
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
