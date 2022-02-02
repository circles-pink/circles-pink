{ pkgs, pursOutput, ... }:
rec {
  result = import ./../../materialized/node2nix/default.nix {
    inherit pkgs;
  };

  sourceWithNodeModules = pkgs.runCommand "sourceWithNodeModules" { } ''
    cp -r ${result.package}/lib/node_modules/circles-pink $out
  '';

  inRepo = pkgs.runCommand "sourceWithNodeModules" { } ''
    mkdir -p $out/pkgs
    cp -r ${sourceWithNodeModules} $out/pkgs/ts
    mkdir -p $out/generated
    cp -r ${pursOutput} $out/generated/output
  '';

  mono = pkgs.runCommand "mono" { buildInputs = [ pkgs.nodejs ]; } ''
    tmp=`mktemp -d`
    cp -r ${inRepo} $tmp/project
    chmod -R 777 $tmp/project
    cd $tmp/project/pkgs/ts
    export OUTPUT_DIR=$tmp/dist; npm run build

    cp -r $tmp/dist $out
  '';

  default = mono;
}
