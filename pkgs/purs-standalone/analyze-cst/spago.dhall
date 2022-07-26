{ name = "analyze-cst"
, dependencies =
  [ "prelude"
  ]
, packages = ../../../packages.dhall
, sources = [ "./src/**/*.purs", "./test/**/*.purs" ]
}
