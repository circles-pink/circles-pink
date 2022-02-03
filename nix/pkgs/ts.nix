{ pkgs, pursOutput, ... }:
rec {
  nodeDependencies = (import ./../../materialized/node2nix/default.nix {
    inherit pkgs;
  }).nodeDependencies;

  mono = pkgs.runCommand "mono" { buildInputs = [ pkgs.nodejs ]; } ''
    tmp=`mktemp -d`
    
    mkdir -p $tmp/pkgs/ts/src/generated
    cp -r ${pursOutput} $tmp/pkgs/ts/src/generated/output

    cp -r ${../../pkgs/ts} $tmp/pkgs/ts
    chmod -R +w $tmp

    cp -r ${nodeDependencies}/lib/node_modules $tmp/pkgs/ts/node_modules
    chmod -R +w $tmp

    # mkdir -p $tmp/pkgs/ts/src/generated/Affjax/node_modules
    # cp -r ${nodeDependencies}/lib/node_modules/xhr2 $tmp/pkgs/ts/src/generated/Affjax/node_modules/xhr2
    # chmod -R +w $tmp

    cp -r 
    export OUTPUT_DIR=$tmp/dist; npm run build --prefix $tmp/pkgs/ts

    cp -r $tmp/dist $out
  '';

  default = mono;
}
