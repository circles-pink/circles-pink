{ pkgs, nodeModules }:

let
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


in
{
  circles-pink-state-machine =
    let
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

      built = spago2nix-extra.build {
        name = "state-machine";
        sources = ../../pkgs/purs/circles-pink-state-machine/src;
        testSources = ../../pkgs/purs/circles-pink-state-machine/test;
        inherit spagoPkgs censorCodes nodeModules;
      };

      default = withTS built.output;
    in
    built // { inherit default; };
}


