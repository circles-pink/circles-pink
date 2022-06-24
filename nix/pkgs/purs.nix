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
      purs-tsd-gen -d $out
      patchTsTypes $out
    '';

  builts = pipe
    (import ../../materialized/spago2nix/default.nix) [
    (recursiveUpdate {
      circles-pink-state-machine = {
        censorCodes = [
          "UnusedImport"
          "UnusedExplicitImport"
          "UnusedName"
          "MissingKindDeclaration"
          "UnusedDeclaration"
          "UnusedDctorImport"
          "UserDefinedWarning"
          "UnusedTypeVar"
        ];
        inherit nodeModules;
      };

      chance = {
        censorCodes = [
          "UnusedImport"
          "UnusedDeclaration"
        ];
      };

      graph = {
        censorCodes = [
          "UnusedExplicitImport"
          "ShadowedName"
        ];
      };

      indexed-graph = {
        censorCodes = [
          "UserDefinedWarning"
        ];
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
