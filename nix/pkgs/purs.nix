{ pkgs, nodeModules }:

let
  inherit (pkgs.lib) recursiveUpdate pipe;

  spagoPkgs = import ../../materialized/spago-packages.nix { inherit pkgs; };
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
    })
    spago2nix-extra.buildMonorepo
    (x: recursiveUpdate x
      { circles-pink-state-machine.default = withTS x.circles-pink-state-machine.output; }
    )
  ];

in
builts
