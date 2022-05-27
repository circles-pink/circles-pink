{ pkgs, nodeModules }:

let
  inherit (pkgs.lib) recursiveUpdate pipe mapAttrs;

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
      circles-pink-state-machine.censorCodes = [
        "UnusedImport"
        "UnusedExplicitImport"
        "UnusedName"
        "MissingKindDeclaration"
        "UnusedDeclaration"
        "UnusedDctorImport"
        "UserDefinedWarning"
        "UnusedTypeVar"
      ];
    })
    (mapAttrs (_: { spagoPkgs, meta, censorCodes ? [ ] }: spago2nix-extra.build {
      name = meta.name;
      spagoPkgs = spagoPkgs { inherit pkgs; };
      sources = ../../pkgs/purs/circles-pink-state-machine/src;
      testSources = ../../pkgs/purs/circles-pink-state-machine/test;
      inherit censorCodes nodeModules;
    }))
    (x: recursiveUpdate
      { circles-pink-state-machine.default = withTS x.circles-pink-state-machine.output; }
      x
    )
  ];

in
builts
