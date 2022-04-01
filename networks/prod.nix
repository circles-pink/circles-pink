{ pkgs, ... }:
{

  webserver = { config, ... }:

    let
      lib = pkgs.lib;
      secrets = (builtins.fromJSON (builtins.readFile /secrets.json)).secrets.data;
    in
    {
      boot.tmpOnTmpfs = false;

      imports = [
        ./modules/qemu-guest.nix
        (import ./modules/webserver.nix { inherit pkgs config lib secrets; })
      ];

      env.url = { domain = "circles"; topLevelDomain = "pink"; };

      nixpkgs.pkgs = pkgs;

      boot.loader.grub.device = "/dev/sda";
      boot.initrd.kernelModules = [ "nvme" ];
      fileSystems."/" = { device = "/dev/sda1"; fsType = "ext4"; };

      deployment.targetHost = "circles.pink";

      security.acme.acceptTerms = true;
      security.acme.email = "circles.pink@protonmail.com";

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
