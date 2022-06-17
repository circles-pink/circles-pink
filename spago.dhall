let circles-pink-state-machine =
      ./pkgs/purs/circles-pink-state-machine/spago.dhall

let chance = ./pkgs/purs/chance/spago.dhall

let fp-ts = ./pkgs/purs/fp-ts/spago.dhall

in  { name = "circles-pink"
    , dependencies =
          circles-pink-state-machine.dependencies
        # chance.dependencies
        # fp-ts.dependencies
    , packages = ./packages.dhall
    , sources =
      [ "./pkgs/purs/circles-pink-state-machine/src/**/*.purs"
      , "./pkgs/purs/circles-pink-state-machine/test/**/*.purs"
      , "./pkgs/purs/chance/src/**/*.purs"
      , "./pkgs/purs/chance/test/**/*.purs"
      , "./pkgs/purs/fp-ts/src/**/*.purs"
      , "./pkgs/purs/fp-ts/test/**/*.purs"
      ]
    }
