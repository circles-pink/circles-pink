{ pkgs, ... }:
{

  webserver = { config, lib, ... }:

    let
      secrets = builtins.fromJSON (builtins.readFile /secrets.json);
    in
    {
      boot.tmpOnTmpfs = false;

      imports = [
        ./modules/qemu-guest.nix
        (import ./modules/webserver.nix { inherit pkgs config lib secrets; })
      ];

      env.domain = "circles.pink";

      nixpkgs.pkgs = pkgs;

      boot.loader.grub.device = "/dev/sda";
      boot.initrd.kernelModules = [ "nvme" ];
      fileSystems."/" = { device = "/dev/sda1"; fsType = "ext4"; };

      deployment.targetHost = "circles.pink";


      security.acme.acceptTerms = true;
      security.acme.email = "circles.pink@protonmail.com";

      services.nginx.virtualHosts = {
        "circles.pink" = {
          forceSSL = true;
          enableACME = true;
        };
      };
    };

}
