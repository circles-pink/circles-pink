let
  # flake = import ./default.nix;
  # pkgs = flake.packages.x86_64-linux.pkgs;


  nixpkgs = builtins.fetchTarball
    "https://github.com/NixOS/nixpkgs/archive/a6a3a368dda.tar.gz";
  pkgs = import nixpkgs {
    system = "x86_64-linux";
    overlays = [
      (import "${effectsSrc}/overlay.nix")
    ];
  };

  # update hash if desired or use different pinning solution
  effectsSrc = builtins.fetchTarball
    "https://github.com/hercules-ci/hercules-ci-effects/archive/b67cfbbb31802389e1fb6a9c75360968d201693b.tar.gz";

  inherit (pkgs.effects) runNixOps runIf;
  inherit (pkgs) lib;
in
{
  # x86_64-linux = pkgs.recurseIntoAttrs {
  #   hello = flake.defaultPackage.x86_64-linux;
  # };

  neat-network = runIf true (
    runNixOps {
      name = "foo";
      src = lib.cleanSource ./.;
      networkFiles = [ "networks/network.nix" ];
    }
  );
}
