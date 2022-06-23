{ name = "indexed-graph"
, dependencies = [ "debug-extra", "graph", "prelude", "spec", "spec-discovery" ]
, packages = ../../../packages.dhall
, sources = [ "./src/**/*.purs", "./test/**/*.purs" ]
}
