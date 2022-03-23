{ pkgs, config, lib, secrets, ... }:

let
  key = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQCpd/8INly7Mnj+QyRme+M98o+J+hrYOUJiB4znDtptl2qgjN4FJzicJUEOMbVwtan8fsFtuRhmsoQnfxLaX1L7EOSHMmbvobQkvdzHW8nKx3oK49zkmLXQ2VDRZ3D10/x1fXN+1r7PPeKn/2BuGrOjFRAuA4Nbg2S0hHQNAoUhsoI+FbFTYxo/r74BbA/ppodhaMfVU+rOSIY3/9PoHcMT6ofW4F2CdG/iO2GM1zb5Afm8spxgyrXjzUD0kMXj8Y7V65eq8R/9K3Ql9c28sZamKQBYAJOpYzbxc4g5b7YH+Ga9IOTD6OSUAZpsSuj6NZMtZGiPKj17WlduaOo1OOHcKJD+ReHlPAjK3JbU46PmQ173jk7CqNTBw+EiVjOsQRMBbmeUBMU7ERTV5k40V3titLvFIAO9DAD7c7JtWZal6fOH9CPTWuxAA0E4MW8MORJqjMQZ39kCwzcM7NybAGeStinhMp+KhDZNfzodxhoxmG4aEFmiXMvIB+Uh86U6gUs= circles-pink";
  # teal = import ../default.nix;

  Makefile = pkgs.writeText "MakeFile" (builtins.readFile ./webserver/Makefile);

  serviceUrls = builtins.mapAttrs (name: value: pkgs.circles-pink.lib.mkUrl value.url) config.env.services;
in

{

  imports = [
    (import ./tasks-explorer.nix { inherit pkgs config lib; })
    (import ./env.nix { inherit pkgs config lib; })
  ];

  env.services = {
    storybook = {
      url = config.env.url;
      port = 80;
    };
    tasks = {
      url = (config.env.url // { subdomain = "tasks"; });
      port = 5000;
    };
    directus = {
      url = (config.env.url // { subdomain = "directus"; });
      port = 8055;
    };
    mysql = {
      url = config.env.url; # REMOVE!!
      port = 5100;
    };
  };


  environment.systemPackages = [
    pkgs.busybox
    pkgs.gnumake
    pkgs.circles-pink.circles-directus
  ];

  environment.shellInit = ''cp ${Makefile} /root/Makefile'';

  networking.hostName = "circles-pink";

  services.openssh.enable = true;

  users.users.root.openssh.authorizedKeys.keys = [
    key
  ];

  systemd.mounts = [ ];

  networking.firewall.allowedTCPPorts = [ 80 22 443 4000 config.env.services.directus.port ];

  services.nginx = {
    enable = true;
    virtualHosts = {
      "${pkgs.circles-pink.lib.mkDomain config.env.services.storybook.url}" = {
        locations."/" = {
          root = pkgs.circles-pink.publicDir { inherit serviceUrls; };
        };
      };
      "${pkgs.circles-pink.lib.mkDomain config.env.services.tasks.url}" = {
        locations."/" = {
          proxyPass = "http://127.0.0.1:${toString config.env.services.tasks.port}";
        };
      };
    };
  };

  systemd.user.services.circles-directus = {
    description = "Circles Directus";
    serviceConfig = {
      Type = "forking";
      ExecStart = "${pkgs.circles-pink.circles-directus}/bin/circles-directus start";
      # ExecStop = "pkill ipfs";
      # Restart = "on-failure";
    };
    wantedBy = [ "default.target" ];
  };

  services.mysql =
    let
      user = "directus";
      password = "secret";
      database = "directus";
      host = "localhost";
    in
    {
      enable = true;
      package = pkgs.mariadb;
      initialDatabases = [{ name = database; }];
      settings = {
        mysqld = {
          innodb_buffer_pool_size = "10M";
          port = config.env.services.mysql.port;
        };
      };
      initialScript = pkgs.writeText "initDB"
        ''
          CREATE USER '${user}'@'${host}' IDENTIFIED BY '${password}';
          GRANT ALL PRIVILEGES ON ${database}. * TO '${user}'@'${host}';
        '';
    };

  services.tasks-explorer = {
    enable = true;
    port = config.env.services.tasks.port;
    notion-token = secrets.notion-token;
    dev = true;
  };

}
