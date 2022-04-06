{ pkgs, ... }:
{

  webserver = { config, ... }:

    let
      lib = pkgs.lib;

      mockSecrets = {
        "notion-token" = "";
        "directus" = {
          "adminToken" = "";
          "dbHost" = "";
          "dbUser" = "";
          "dbName" = "";
          "dbPassword" = "";
          "secret" = "";
          "key" = "";
          "initialAdminEmail" = "";
          "initialAdminPassword" = "";
        };
      };

      secretsFile = /secrets.json;

      secrets =
        if builtins.pathExists secretsFile
        then (builtins.fromJSON (builtins.readFile secretsFile)).secrets.data
        else mockSecrets;

    in
    {
      boot.tmpOnTmpfs = false;

      imports = [
        ./modules/qemu-guest.nix
        (import ./modules/webserver.nix { inherit pkgs config lib secrets; })
      ];

      env.url = { domain = "circles"; topLevelDomain = "pink"; };

      nixpkgs.pkgs = pkgs;
      nixpkgs.system = "x86_64-linux";

      boot.loader.grub.device = "/dev/sda";
      boot.initrd.kernelModules = [ "nvme" ];
      fileSystems."/" = { device = "/dev/sda1"; fsType = "ext4"; };

      deployment.targetHost = "circles.pink";

      security.acme.acceptTerms = true;
      security.acme.email = "circles.pink@protonmail.com";

      env.envVars = {
        gardenApi = "https://api.circles.garden";
        gardenApiUsers = "https://api.circles.garden/api/users";
        gardenGraphApi = "https://api.thegraph.com";
        gardenSubgraphName = "CirclesUBI/circles-subgraph";
        gardenRelay = "https://relay.circles.garden";
        gardenHubAddress = "0xCfEB869F69431e42cdB54A4F4f105C19C080A601";
        gardenProxyFactoryAddress = "0xD833215cBcc3f914bD1C9ece3EE7BF8B14f841bb";
        gardenSafeMasterAddress = "0xC89Ce4735882C9F0f0FE26686c53074E09B0D550";
        gardenEthereumNodeWebSocket = "wss://dark-frosty-field.xdai.quiknode.pro";
      };

      services.nginx.virtualHosts = lib.mapAttrs'
        (_: value: {
          name = lib.mkDomain value.url;
          value = {
            forceSSL = true;
            enableACME = true;
          };
        })
        config.env.services;
    };
}
