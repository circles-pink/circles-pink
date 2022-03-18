{ pkgs ? (import ../default.nix).outputs.packages.x86_64-linux.pkgs, ... }:

let
  key = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQCpd/8INly7Mnj+QyRme+M98o+J+hrYOUJiB4znDtptl2qgjN4FJzicJUEOMbVwtan8fsFtuRhmsoQnfxLaX1L7EOSHMmbvobQkvdzHW8nKx3oK49zkmLXQ2VDRZ3D10/x1fXN+1r7PPeKn/2BuGrOjFRAuA4Nbg2S0hHQNAoUhsoI+FbFTYxo/r74BbA/ppodhaMfVU+rOSIY3/9PoHcMT6ofW4F2CdG/iO2GM1zb5Afm8spxgyrXjzUD0kMXj8Y7V65eq8R/9K3Ql9c28sZamKQBYAJOpYzbxc4g5b7YH+Ga9IOTD6OSUAZpsSuj6NZMtZGiPKj17WlduaOo1OOHcKJD+ReHlPAjK3JbU46PmQ173jk7CqNTBw+EiVjOsQRMBbmeUBMU7ERTV5k40V3titLvFIAO9DAD7c7JtWZal6fOH9CPTWuxAA0E4MW8MORJqjMQZ39kCwzcM7NybAGeStinhMp+KhDZNfzodxhoxmG4aEFmiXMvIB+Uh86U6gUs= circles-pink";
  # teal = import ../default.nix;
in
{

  webserver =
    let
      system = "x86_64-linux";
    in
    {
      environment.systemPackages = [ pkgs.busybox ];

      networking.hostName = "circles-pink";

      services.openssh.enable = true;

      users.users.root.openssh.authorizedKeys.keys = [
        key
      ];

      systemd.mounts = [ ];

      networking.firewall.allowedTCPPorts = [ 80 22 443 4000 8055 ];

      services.nginx = {
        enable = true;
        virtualHosts = {
          "circles.pink" = {
            serverAliases = [ "circles.pink" ];
            locations."/" = {
              root = pkgs.circles-pink.publicDir;
            };
          };
        };
      };

      systemd.user.services.circles-directus = {
        description = "Circles Directus";
        serviceConfig = {
          Type = "forking";
          ExecStart = "${pkgs.circles-pink.circles-directus}/bin/circles-directus";
          # ExecStop = "pkill ipfs";
          # Restart = "on-failure";
        };
        wantedBy = [ "default.target" ];
      };

      services.postgresql = {
        enable = true;
        port = 5100;
        package = pkgs.postgresql_10;
        enableTCPIP = true;
        settings = {
          listen_addresses = "*";
        };
        authentication = pkgs.lib.mkOverride 10 ''
          local all all trust
          host all all ::1/128 trust
          host all all 0.0.0.0/0 md5
        '';
        initialScript = pkgs.writeText "backend-initScript" ''
          CREATE ROLE postgres WITH LOGIN PASSWORD 'secret' SUPERUSER;
          CREATE DATABASE directus;
          GRANT ALL PRIVILEGES ON DATABASE directus TO postgres;
        '';
      };

    };
}
