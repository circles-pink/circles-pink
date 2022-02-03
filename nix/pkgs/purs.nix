{ pkgs }:

let
  spagoPkgs = import ../../materialized/spago-packages.nix { inherit pkgs; };
in
rec {
  dependencies = pkgs.runCommand "dependencies"
    {
      buildInputs = [
        spagoPkgs.buildSpagoStyle
        spagoPkgs.installSpagoStyle
      ];
      nativeBuildInputs = [
        pkgs.purescript
        pkgs.spago
      ];
    } ''
    mkdir $out    
    cd $out
    install-spago-style
    build-spago-style
  '';

  pursOutput = pkgs.runCommand "pursOutput"
    {
      buildInputs = [
        spagoPkgs.buildSpagoStyle
      ];
      nativeBuildInputs = [
        pkgs.purescript
        pkgs.spago
      ];
    }
    ''
      tmp=`mktemp -d`
      cp ${../../packages.dhall} $tmp/packages.dhall
      cp ${../../spago.dhall} $tmp/spago.dhall
      cp -r ${dependencies}/.spago $tmp/.spago
      cp -r ${dependencies}/output $tmp/output
      
      mkdir -p $tmp/pkgs/purs
      cp -r ${../../pkgs/purs/src} $tmp/pkgs/purs/src
      
      chmod -R +w $tmp
      
      cd $tmp
      build-spago-style "./pkgs/purs/src/**/*.purs"
      
      cp -r $tmp/output $out
    '';


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
      buildInputs = [ pkgs.circles-pink.patchTsTypes ];
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
