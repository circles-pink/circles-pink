{ pkgs ? (import ../default.nix).outputs.packages.x86_64-linux.pkgs, ... }:

{

  webserver =
    {
      imports = [
        (import ./modules/webserver.nix { inherit pkgs; })
      ];

      deployment.targetEnv = "virtualbox";
      deployment.virtualbox.memorySize = 1024 * 4; # megabytes
      deployment.virtualbox.vcpu = 2; # number of cpus
      deployment.virtualbox.headless = true;
      # virtualisation.virtualbox.guest.enable = pkgs.lib.mkForce false
    };
}
