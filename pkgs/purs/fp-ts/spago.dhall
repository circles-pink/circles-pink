{ name = "fp-ts"
, dependencies =
  [ "aff"
  , "effect"
  , "heterogeneous"
  , "maybe"
  , "pairs"
  , "prelude"
  , "spec"
  , "spec-discovery"
  , "undefined"
  , "unsafe-coerce"
  ]
, packages = ../../../packages.dhall
, sources = [ "./src/**/*.purs", "./test/**/*.purs" ]
}
