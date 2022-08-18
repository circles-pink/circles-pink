{ pkgs, nodeModules }:

let
  inherit (pkgs.lib) recursiveUpdate pipe;
  inherit (builtins) removeAttrs;

  spago2nix-extra = import ./spago2nix-extra.nix { inherit pkgs; };

  withTS = pursOutput: pkgs.runCommand "pursOutputWithTS"
    {
      buildInputs = [ pkgs.purescript-tsd-gen pkgs.circles-pink.patchTsTypes ];
    }
    ''
      cp -r ${pursOutput} $out
      chmod -R +w $out
      
      DIR=`mktemp -d`;
      mv $out/CirclesPink.EnvVars -t $DIR;
      mv $out/CirclesPink.Garden.ApiScript -t $DIR;
      mv $out/CirclesPink.Garden.TS -t $DIR;
      mv $out/Test.CirclesPinkStateMachine.Main -t $DIR;
	    mv $out/Test.AllTests.Main -t $DIR;
      mv $out/Payload.* $out/VoucherServer.* $out/CirclesPink.Garden.EnvControlAff -t $DIR;
      mv $DIR/VoucherServer.Types -t $out;

      purs-tsd-gen --directory $out;
      mv $DIR/* -t $out; 

      patchTsTypes $out

      
    '';

  builts = pipe
    (import ../../materialized/spago2nix/default.nix) [
    (recursiveUpdate {
      circles-pink-state-machine = {
        censorCodes = [ ];
        inherit nodeModules;
      };

      chance = {
        censorCodes = [ ];
      };

      graph = {
        censorCodes = [ ];
      };

      indexed-graph = {
        censorCodes = [ ];
      };
    })
    (x: removeAttrs x [ "all-tests" ])
    spago2nix-extra.buildMonorepo
    (x: recursiveUpdate x
      { circles-pink-state-machine.default = withTS x.circles-pink-state-machine.output; }
    )
  ];

in
builts
