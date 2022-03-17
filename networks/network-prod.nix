{ pkgs, ... }:


{

  webserver =
    {
      boot.tmpOnTmpfs = false;

      imports = [ ./qemu-guest.nix ];

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
