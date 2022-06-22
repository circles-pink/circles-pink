{ name = "debug-extra"
, dependencies = [ "prelude", "spec", "spec-discovery", "undefined" ]
, packages = ../../../packages.dhall
, sources = [ "./src/**/*.purs", "./test/**/*.purs" ]
}
