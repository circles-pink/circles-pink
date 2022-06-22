{ name = "chance"
, dependencies =
  [ "aff"
  , "effect"
  , "option"
  , "prelude"
  , "spec"
  , "spec-discovery"
  , "unsafe-coerce"
  ]
, packages = ../../../packages.dhall
, sources = [ "./src/**/*.purs", "./test/**/*.purs" ]
}
