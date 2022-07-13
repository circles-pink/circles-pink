{ name = "purs-ts"
, dependencies =
  [ "prelude"
  ]
, packages = ../../../../../packages.dhall
, sources = [ "./src/**/*.purs", "./test/**/*.purs" ]
}
