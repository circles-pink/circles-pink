{

  webserver =
    { config, lib, ... }:
    let
      pkgs = (import ../default.nix).outputs.packages.x86_64-linux.pkgs;

      secrets = import (builtins.readFile ../secrets.json);
    in
    {
      imports = [
        (import ./modules/webserver.nix { inherit pkgs config lib secrets; })
      ];

      deployment.targetEnv = "virtualbox";
      deployment.virtualbox.memorySize = 1024 * 4; # megabytes
      deployment.virtualbox.vcpu = 2; # number of cpus
      deployment.virtualbox.headless = true;
      # virtualisation.virtualbox.guest.enable = pkgs.lib.mkForce false
    };
}
