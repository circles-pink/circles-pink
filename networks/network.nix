let
  key = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQCpd/8INly7Mnj+QyRme+M98o+J+hrYOUJiB4znDtptl2qgjN4FJzicJUEOMbVwtan8fsFtuRhmsoQnfxLaX1L7EOSHMmbvobQkvdzHW8nKx3oK49zkmLXQ2VDRZ3D10/x1fXN+1r7PPeKn/2BuGrOjFRAuA4Nbg2S0hHQNAoUhsoI+FbFTYxo/r74BbA/ppodhaMfVU+rOSIY3/9PoHcMT6ofW4F2CdG/iO2GM1zb5Afm8spxgyrXjzUD0kMXj8Y7V65eq8R/9K3Ql9c28sZamKQBYAJOpYzbxc4g5b7YH+Ga9IOTD6OSUAZpsSuj6NZMtZGiPKj17WlduaOo1OOHcKJD+ReHlPAjK3JbU46PmQ173jk7CqNTBw+EiVjOsQRMBbmeUBMU7ERTV5k40V3titLvFIAO9DAD7c7JtWZal6fOH9CPTWuxAA0E4MW8MORJqjMQZ39kCwzcM7NybAGeStinhMp+KhDZNfzodxhoxmG4aEFmiXMvIB+Uh86U6gUs= circles-pink";
  teal = import ../default.nix;
in
{
  network.description = "Circles Pink Network";

  webserver =
    { config, pkgs, modulesPath, ... }:
    {
      services.httpd.enable = true;
      services.httpd.adminAddr = "admin@teal.ooo";

      services.httpd.virtualHosts = {
        "circles.pink" = {
          listen = [{ port = 80; }];
          documentRoot = teal.entries;
        };
      };

      environment.systemPackages = [ pkgs.busybox ];

      boot.cleanTmpDir = true;
      networking.hostName = "circles-pink";

      services.openssh.enable = true;

      users.users.root.openssh.authorizedKeys.keys = [
        key
      ];

      imports = [ (modulesPath + "/profiles/qemu-guest.nix") ];
      boot.loader.grub.device = "/dev/sda";
      boot.initrd.kernelModules = [ "nvme" ];
      fileSystems."/" = { device = "/dev/sda1"; fsType = "ext4"; };

      networking.firewall.allowedTCPPorts = [ 80 22 ];

      deployment.targetHost = "circles.pink";
    };
}
