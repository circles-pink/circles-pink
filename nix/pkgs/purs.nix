{ pkgs, nodeModules }:

let
  inherit (pkgs.lib) recursiveUpdate pipe;
  inherit (builtins) removeAttrs;

  spago2nix-extra = import ./spago2nix-extra.nix { inherit pkgs; };

  withTS = pursOutput: pkgs.runCommand "pursOutputWithTS"
    {
      buildInputs = [
        pkgs.circles-pink.generate-tsd
        pkgs.nodePackages.prettier
      ];
    }
    ''
      cp -r ${pursOutput} $out
      chmod -R +w $out
      generate-tsd --output-dir $out
    '';

  builts = pipe
    (import ../../materialized/spago2nix/default.nix) [
    (recursiveUpdate {
      circles-pink-state-machine = {
        censorCodes = [ "WildcardInferredType" ];
        inherit nodeModules;
      };

      chance = {
        censorCodes = [ ];
      };

      graph = {
        censorCodes = [ "WildcardInferredType" ];
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
