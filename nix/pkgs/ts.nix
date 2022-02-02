{ pkgs, ... }:
rec {
  result = import ./../../materialized/node2nix/default.nix {
    inherit pkgs;
  };

  mono = pkgs.runCommand "mono" { buildInputs = [ pkgs.nodejs ]; } ''
    tmp=`mktemp -d`
    cd $tmp
    cp -r ${result.package}/* -t .
    chmod -R 777 $tmp
    cd lib/node_modules/circles-pink
    export OUTPUT_DIR=dist
    npm run build

    cp -r $tmp/lib/node_modules/circles-pink/dist $out
  '';

  default = mono;
}
