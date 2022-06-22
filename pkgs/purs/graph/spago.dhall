{ name = "graph"
, dependencies =
  [ "aff", "debug-extra", "effect", "prelude", "spec", "spec-discovery" ]
, packages = ../../../packages.dhall
, sources = [ "./src/**/*.purs", "./test/**/*.purs" ]
}
