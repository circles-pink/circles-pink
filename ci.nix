let
  # update hash if desired or use different pinning solution
  effectsSrc = builtins.fetchTarball
    "https://github.com/hercules-ci/hercules-ci-effects/archive/2e165352d92782e7ae149f4f1a9b3174f718a3af.tar.gz";


  nixpkgs = builtins.fetchTarball
    "https://github.com/NixOS/nixpkgs/archive/1caf78f4bf5cba45eb04c45a3c9b46bde8fa50e0.tar.gz";


  pkgs = import nixpkgs {
    system = "x86_64-linux";
    overlays = [
      (import "${effectsSrc}/overlay.nix")
    ];
  };

  inherit (pkgs.effects) runNixOps2 runIf;
  #   inherit (pkgs) lib;
in
{

  neat-network = runIf true (
    runNixOps2 {
      name = "foo";
      flake = ./flake.nix;

      #   src = lib.cleanSource ./.;
      #   networkFiles = [ "networks/network.nix" ];
      userSetupScript = ''
        writeSSHKey
      '';
      inputs = [ pkgs.openssh ];
      secretsMap = {
        "ssh" = "default-ssh";
      };
    }
  );
}
