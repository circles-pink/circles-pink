let
  flake = import ./default.nix;
  pkgs = flake.packages.x86_64-linux.pkgs;
in
{
  x86_64-linux = pkgs.recurseIntoAttrs {
    hello = flake.defaultPackage.x86_64-linux;
  };
}
