let
  url = "https://github.com/NixOS/nixpkgs/archive/2255f292063ccbe184ff8f9b35ce475c04d5ae69.tar.gz";
  pkgs = import (builtins.fetchTarball url) { system = "x86_64-linux"; };
in
{
  x86_64-linux = pkgs.recurseIntoAttrs {
    hello = pkgs.hello;
  };
}
