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
    "https://github.com/hercules-ci/hercules-ci-effects/archive/2e165352d92782e7ae149f4f1a9b3174f718a3af.tar.gz";

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
