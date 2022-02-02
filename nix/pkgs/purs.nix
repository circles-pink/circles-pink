{ pkgs }:

let
  spagoPkgs = import ../../materialized/spago-packages.nix { inherit pkgs; };
in
rec {
  pursOutput = pkgs.stdenv.mkDerivation {
    name = "pursOutput";
    buildInputs = [
      spagoPkgs.installSpagoStyle
      spagoPkgs.buildSpagoStyle
    ];
    nativeBuildInputs = [
      pkgs.purescript
      pkgs.spago
    ];
    src = ./.;
    unpackPhase = ''
      cp ${../../packages.dhall} ./packages.dhall
      cp ${../../spago.dhall} ./spago.dhall
      mkdir -p pkgs/purs/src
      cp -r ${../../pkgs/purs/src} -t pkgs/purs/src
      install-spago-style
    '';
    buildPhase = ''
      build-spago-style "./pkgs/purs/src/**/*.purs"
    '';
    installPhase = ''
      mkdir $out
      mv output/* -t $out
    '';
  };

  pursOutputWithTS = pkgs.runCommand "pursOutputWithTS"
    {
      buildInputs = [ pkgs.purescript-tsd-gen ];
    }
    ''
      tmp=`mktemp -d`
      cd $tmp
      cp -r ${pursOutput}/* -t .
      chmod -R 777 $tmp
      purs-tsd-gen -d .

      mkdir $out
      cp -r $tmp/* -t $out
    '';

  pursOutputWithTSPatched = pkgs.runCommand "pursOutputWithTSPatched"
    {
      buildInputs = [ pkgs.patchTsTypes ];
    }
    ''
      mkdir $out
      cp -r ${pursOutputWithTS}/* -t $out
      patchTsTypes $out
    '';

  default = pursOutputWithTSPatched;
}
