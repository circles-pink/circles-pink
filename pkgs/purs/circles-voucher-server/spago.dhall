{ name = "circles-voucher-server"
, dependencies =
  [ "aff"
  , "effect"
  , "option"
  , "payload"
  , "prelude"
  , "spec"
  , "spec-discovery"
  , "unsafe-coerce"
  ]
, packages = ../../../packages.dhall
, sources = [ "./src/**/*.purs", "./test/**/*.purs" ]
}
