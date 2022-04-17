{ pkgs, nodeModules }:

let
  spagoPkgs = import ../../materialized/spago-packages.nix { inherit pkgs; };
  spago2nix-extra = import ./spago2nix-extra.nix { inherit pkgs; };
in
rec {
  dependencies = spago2nix-extra.buildDependencies "circles" spagoPkgs;
  projectDir = spago2nix-extra.mkProjectDir "circles" { inherit dependencies sources testSources; };
  projectOut = spago2nix-extra.buildProject "circles" { inherit projectDir spagoPkgs; };
  projectTests = spago2nix-extra.testProject "circles" { inherit projectOut spagoPkgs nodeModules; };


  sources = ../../pkgs/purs/src;
  testSources = ../../pkgs/purs/test;

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


