{

  webserver =
    { config, ... }:
    let
      pkgs = (import ../default.nix).outputs.packages.x86_64-linux.pkgs;

      secrets = (builtins.fromJSON (builtins.readFile ../secrets.json)).secrets.data;

      lib = pkgs.lib;

    in
    {
      imports = [
        (import ./modules/webserver.nix { inherit pkgs config lib secrets; })
      ];

      network-config.webserver.services.storybook.enable = true;

      env.url = {
        domain = "circles";
        topLevelDomain = "local";
        protocol = "http";
      };

      env.envVars = {
        gardenApi = "http://api.circles.local";
        gardenApiUsers = "http://api.circles.local/api/users";
        gardenGraphApi = "http://graph.circles.local";
        gardenSubgraphName = "CirclesUBI/circles-subgraph";
        gardenRelay = "http://relay.circles.local";
        gardenHubAddress = "0xCfEB869F69431e42cdB54A4F4f105C19C080A601";
        gardenProxyFactoryAddress = "0xD833215cBcc3f914bD1C9ece3EE7BF8B14f841bb";
        gardenSafeMasterAddress = "0xC89Ce4735882C9F0f0FE26686c53074E09B0D550";
        gardenEthereumNodeWebSocket = "ws://ganache:8545";
      };

      deployment.targetEnv = "virtualbox";
      deployment.virtualbox.memorySize = 1024 * 4; # megabytes
      deployment.virtualbox.vcpu = 2; # number of cpus
      deployment.virtualbox.headless = true;
      # virtualisation.virtualbox.guest.enable = pkgs.lib.mkForce false
    };
}
