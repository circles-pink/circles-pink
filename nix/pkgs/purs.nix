{ pkgs, nodeModules }:

let
  spagoPkgs = import ../../materialized/spago-packages.nix { inherit pkgs; };
  spago2nix-extra = import ./spago2nix-extra.nix { inherit pkgs; };
in
rec {
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

  dependencies = spago2nix-extra.buildDependencies "state-machine" spagoPkgs;
  projectDir = spago2nix-extra.mkProjectDir "state-machine" { inherit dependencies sources testSources; };
  projectOut = spago2nix-extra.buildProject "state-machine" { inherit projectDir spagoPkgs censorCodes; };
  projectTests = spago2nix-extra.testProject "state-machine" { inherit projectOut spagoPkgs nodeModules; };

  docs = spago2nix-extra.genDocs "state-machine" { inherit projectOut spagoPkgs projectDir; };

  sources = ../../pkgs/purs/circles-pink-state-machine/src;
  testSources = ../../pkgs/purs/circles-pink-state-machine/test;

  pursOutput = projectOut;

  pursOutputWithTS = pkgs.runCommand "pursOutputWithTS"
    {
      buildInputs = [ pkgs.purescript-tsd-gen ];
    }
    ''
      tmp=`mktemp -d`
      cd $tmp
      cp -r ${pursOutput}/* -t .
      chmod -R +w $tmp
      purs-tsd-gen -d .

      mkdir $out
      cp -r $tmp/* -t $out
    '';

  pursOutputWithTSPatched = pkgs.runCommand "pursOutputWithTSPatched"
    {
      buildInputs = [
        pkgs.circles-pink.patchTsTypes
      ];
    }
    ''
      tmp=`mktemp -d`
      cp -r ${pursOutputWithTS}/* -t $tmp
      chmod -R +w $tmp
      patchTsTypes $tmp
      
      mkdir $out
      cp -r $tmp/* -t $out
    '';

  default = pursOutputWithTSPatched;
}


