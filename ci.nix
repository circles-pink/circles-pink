let
  flake = import ./default.nix;
in
{
  x86_64-linux = pkgs.recurseIntoAttrs {
    hello = flake.hello;
  };
}
