{ pkgs, ... }:
{

  webserver = { config, ... }:

    let
      lib = pkgs.lib;

      mockSecrets = {
        "notion-token" = "MOCKED-SECRET";
        "directus" = {
          "adminToken" = "MOCKED-SECRET";
          "dbHost" = "MOCKED-SECRET";
          "dbUser" = "MOCKED-SECRET";
          "dbName" = "MOCKED-SECRET";
          "dbPassword" = "MOCKED-SECRET";
          "secret" = "MOCKED-SECRET";
          "key" = "MOCKED-SECRET";
          "initialAdminEmail" = "MOCKED-SECRET";
          "initialAdminPassword" = "MOCKED-SECRET";
        };
        "xbge" = {
          "voucherCodeSecret" = "MOCKED-SECRET";
          "authSecret" = "MOCKED-SECRET";
          "endpoint" = "MOCKED-SECRET";
          "safeAddress" = "MOCKED-SECRET";
          "privKey" = "MOCKED-SECRET";
        };
      };

      secretsFile = /secrets.json;

      secrets =
        if builtins.pathExists secretsFile
        then
          let
            json = builtins.fromJSON (builtins.readFile secretsFile);
          in
          json.secrets.data
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
        gardenSubgraphName = "azf20/circles-ubi";
        gardenRelay = "https://relay.circles.garden";
        gardenHubAddress = "0x29b9a7fBb8995b2423a71cC17cf9810798F6C543";
        gardenProxyFactoryAddress = "0x8b4404DE0CaECE4b966a9959f134f0eFDa636156";
        gardenSafeMasterAddress = "0x2CB0ebc503dE87CFD8f0eCEED8197bF7850184ae";
        gardenEthereumNodeWebSocket = "wss://dark-frosty-field.xdai.quiknode.pro";
        xbgeSafeAddress = secrets.xbge.safeAddress;
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
