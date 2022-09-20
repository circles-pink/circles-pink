let circles-pink-state-machine =
      ./pkgs/purs/circles-pink-state-machine/spago.dhall

let chance = ./pkgs/purs/chance/spago.dhall

-- let circles-voucher-server = ./pkgs/purs/circles-voucher-server/spago.dhall

let graph = ./pkgs/purs/graph/spago.dhall

let indexed-graph = ./pkgs/purs/indexed-graph/spago.dhall

let debug-extra = ./pkgs/purs/debug-extra/spago.dhall

let all-tests = ./pkgs/purs/all-tests/spago.dhall

in  { name = "circles-pink"
    , dependencies =
          circles-pink-state-machine.dependencies
        # chance.dependencies
        -- # circles-voucher-server.dependencies
        # graph.dependencies
        # indexed-graph.dependencies
        # debug-extra.dependencies
        # all-tests.dependencies
    , packages = ./packages.dhall
    , sources =
      [ "./pkgs/purs/circles-pink-state-machine/src/**/*.purs"
      , "./pkgs/purs/circles-pink-state-machine/test/**/*.purs"
      , "./pkgs/purs/chance/src/**/*.purs"
      , "./pkgs/purs/chance/test/**/*.purs"
      -- , "./pkgs/purs/circles-voucher-server/src/**/*.purs"
      -- , "./pkgs/purs/circles-voucher-server/test/**/*.purs"
      , "./pkgs/purs/graph/src/**/*.purs"
      , "./pkgs/purs/graph/test/**/*.purs"
      , "./pkgs/purs/indexed-graph/src/**/*.purs"
      , "./pkgs/purs/indexed-graph/test/**/*.purs"
      , "./pkgs/purs/debug-extra/src/**/*.purs"
      , "./pkgs/purs/debug-extra/test/**/*.purs"
      , "./pkgs/purs/all-tests/src/**/*.purs"
      , "./pkgs/purs/all-tests/test/**/*.purs"
      ]
    }
