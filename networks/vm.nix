{

  webserver =
    { config, ... }:
    let
      pkgs = (import ../default.nix).outputs.packages.x86_64-linux.pkgs;

      secrets = builtins.fromJSON (builtins.readFile ../secrets.json);

      lib = pkgs.lib;

    in
    {
      imports = [
        (import ./modules/webserver.nix { inherit pkgs config lib secrets; })
      ];

      env.url = {
        domain = "circles";
        topLevelDomain = "local";
        protocol = "http";
      };

      deployment.targetEnv = "virtualbox";
      deployment.virtualbox.memorySize = 1024 * 4; # megabytes
      deployment.virtualbox.vcpu = 2; # number of cpus
      deployment.virtualbox.headless = true;
      # virtualisation.virtualbox.guest.enable = pkgs.lib.mkForce false
    };
}
