{ pkgs, ... }: {
  yarn2nix = import ./yarn2nix/default.nix { inherit pkgs; };
}
