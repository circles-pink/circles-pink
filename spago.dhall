let circles-pink-state-machine =
      ./pkgs/purs/circles-pink-state-machine/spago.dhall

in  { name = "circles-pink"
    , dependencies = circles-pink-state-machine.dependencies
    , packages = ./packages.dhall
    , sources = circles-pink-state-machine.sources
    }
