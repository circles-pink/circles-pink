let circles-pink-state-machine =
      ./pkgs/purs/circles-pink-state-machine/spago.dhall

let chance = ./pkgs/purs/chance/spago.dhall

in  { name = "circles-pink"
    , dependencies =
        circles-pink-state-machine.dependencies # chance.dependencies
    , packages = ./packages.dhall
    , sources =
      [ "./pkgs/purs/circles-pink-state-machine/src/**/*.purs"
      , "./pkgs/purs/circles-pink-state-machine/test/**/*.purs"
      , "./pkgs/purs/chance/src/**/*.purs"
      , "./pkgs/purs/chance/test/**/*.purs"
      ]
    }
