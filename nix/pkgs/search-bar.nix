{ pkgs, ... }:
let
  scope = "circles-pink";

  engines = {
    "Hoogle" = [ "hoogle" "https://hoogle.haskell.org/?hoogle=%s&scope=set%3Astackage" ];
    "Nixpkgs" = [ "nixpkgs" "https://search.nixos.org/packages?query=%s" ];
    "Pursuit" = [ "pursuit" "https://pursuit.purescript.org/search?q=%s" ];
    "Nixos" = [ "nixos" "https://search.nixos.org/options?channel=21.11&from=0&size=50&sort=relevance&type=packages&query=%s" ];
  };

in
pkgs.search-bar-patcher { inherit pkgs scope engines; }
