{ name = "graph"
, dependencies = [ "debug-extra", "prelude", "spec", "spec-discovery" ]
, packages = ../../../packages.dhall
, sources = [ "./src/**/*.purs", "./test/**/*.purs" ]
}
