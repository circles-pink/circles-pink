{ lib }:
let
  string = import ./string.nix;

  fp = {
    inherit string;
  };

in
fp
