{ name = "fp-ts"
, dependencies =
  [ "aff"
  , "effect"
  , "maybe"
  , "prelude"
  , "spec"
  , "spec-discovery"
  , "undefined"
  , "unsafe-coerce"
  ]
, packages = ../../../packages.dhall
, sources = [ "./src/**/*.purs", "./test/**/*.purs" ]
}
