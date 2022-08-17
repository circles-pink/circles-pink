{ pkgs, config, lib, secrets, ... }:

let
  key = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQCpd/8INly7Mnj+QyRme+M98o+J+hrYOUJiB4znDtptl2qgjN4FJzicJUEOMbVwtan8fsFtuRhmsoQnfxLaX1L7EOSHMmbvobQkvdzHW8nKx3oK49zkmLXQ2VDRZ3D10/x1fXN+1r7PPeKn/2BuGrOjFRAuA4Nbg2S0hHQNAoUhsoI+FbFTYxo/r74BbA/ppodhaMfVU+rOSIY3/9PoHcMT6ofW4F2CdG/iO2GM1zb5Afm8spxgyrXjzUD0kMXj8Y7V65eq8R/9K3Ql9c28sZamKQBYAJOpYzbxc4g5b7YH+Ga9IOTD6OSUAZpsSuj6NZMtZGiPKj17WlduaOo1OOHcKJD+ReHlPAjK3JbU46PmQ173jk7CqNTBw+EiVjOsQRMBbmeUBMU7ERTV5k40V3titLvFIAO9DAD7c7JtWZal6fOH9CPTWuxAA0E4MW8MORJqjMQZ39kCwzcM7NybAGeStinhMp+KhDZNfzodxhoxmG4aEFmiXMvIB+Uh86U6gUs= circles-pink";

  Makefile = pkgs.writeText "MakeFile" (builtins.readFile ./webserver/Makefile);

  mkServiceUrl = name: value:
    let
      url = lib.mkUrl value.url;
    in
    if value.locations == null
    then url
    else builtins.mapAttrs (_: r: url + r) value.locations;

  internalServiceUrls = builtins.mapAttrs mkServiceUrl config.env.services;

  envVars = internalServiceUrls // config.env.envVars;

  seed-with-credentials = pkgs.writeShellScriptBin' "seed-with-credentials"
    {
      onPath = [ pkgs.circles-pink.seed-db ];
      env = {
        DIRECTUS_ADMIN_TOKEN = secrets.directus.adminToken;
      };
    } ''
    seed-db $@
  '';
in

{

  imports = [
    (import ./tasks-explorer.nix { inherit pkgs config lib; })
    (import ./voucher-server.nix { inherit pkgs config lib; })
    (import ./directus.nix { inherit pkgs config lib; })
    (import ./env.nix { inherit pkgs config lib; })
    (import ./network-config.nix { inherit pkgs config lib; })
  ];

  env.services = {
    storybook = {
      url = config.env.url;
      port = 80;
    };
    docs = {
      url = (config.env.url // { subdomain = "docs"; });
    };
    tasks = {
      url = (config.env.url // { subdomain = "tasks"; });
      port = 5000;
    };
    directus = {
      url = (config.env.url // { subdomain = "directus"; });
      locations = {
        "/graphql" = "/graphql";
        "/graphql/system" = "/graphql/system";
      };
      port = 8055;
    };
    mysql = {
      url = config.env.url; # REMOVE!!
      port = 3306;
    };
    voucher-server = {
      url = (config.env.url // { subdomain = "voucher-server"; });
      port = 4000;
    };
  };

  environment.systemPackages = [
    pkgs.busybox
    pkgs.gnumake
    seed-with-credentials
  ];

  environment.shellInit = ''cp ${Makefile} /root/Makefile'';

  networking.hostName = "circles-pink";

  services.openssh.enable = true;

  users.users.root.openssh.authorizedKeys.keys = [
    key
  ];

  systemd.mounts = [ ];

  networking.firewall.allowedTCPPorts = [ 80 22 443 ];

  services.nginx = {
    enable = true;
    virtualHosts =
      # if config.network-config.webserver.services.storybook.enable then {
      #   "${lib.mkDomain config.env.services.storybook.url}" = {
      #     locations."/" = {
      #       root = pkgs.circles-pink.publicDir { inherit envVars; };
      #     };
      #   };
      # } else { }
      #   //
      {
        "${lib.mkDomain config.env.services.storybook.url}" = pkgs.lib.mkIf config.network-config.webserver.services.storybook.enable {
          locations."/" = {
            root = pkgs.circles-pink.publicDir { inherit envVars; };
          };
        };
        "${lib.mkDomain config.env.services.docs.url}" = {
          locations."/" = {
            root = pkgs.circles-pink.purs.circles-pink-state-machine.docs;
          };
        };
        "${lib.mkDomain config.env.services.tasks.url}" = {
          locations."/" = {
            proxyPass = "http://127.0.0.1:${toString config.env.services.tasks.port}";
          };
        };
        "${lib.mkDomain config.env.services.voucher-server.url}" =
          let
            clientUrl =
              if config.env.isDev
              then "http://circles.local"
              else "https://circles.pink";
          in
          {
            locations."/" = {
              proxyPass = "http://127.0.0.1:${toString config.env.services.voucher-server.port}";
              extraConfig = ''
                proxy_set_header HOST $host;
                proxy_set_header X-Real-IP $remote_addr;

                add_header 'Access-Control-Allow-Origin' '${clientUrl}' always;
                add_header 'Access-Control-Allow-Credentials' 'true' always;
                add_header 'Access-Control-Allow-Headers' 'DNT,X-CustomHeader,Keep-Alive,User-Agent,X-Requested-With,If-Modified-Since,Cache-Control,Content-Type,Origin,X-Auth-Token,Authorization,Accept,Client-Security-Token' always;
                add_header 'Access-Control-Allow-Methods' 'OPTIONS, GET, POST, PATCH, PUT, DELETE' always;
              '';
            };
          };
        "${lib.mkDomain config.env.services.directus.url}" = {
          # locations."/" = {
          #   proxyPass = "http://127.0.0.1:${toString config.env.services.directus.port}";
          # };
          locations."/" = {
            proxyPass = "http://127.0.0.1:${toString config.env.services.directus.port}";
            extraConfig = ''
              proxy_http_version 1.1;
              proxy_set_header Upgrade $http_upgrade;
              proxy_set_header Connection 'upgrade';
              proxy_set_header Host $host;
              proxy_cache_bypass $http_upgrade;
            '';
          };
        };
      };
  };

  services.directus = {
    enable = true;
    port = config.env.services.directus.port;
    mysqlPort = config.env.services.mysql.port;
    schemaJson = ./webserver/schema.json;
    directusSecrets = secrets.directus;
  };

  services.tasks-explorer = {
    enable = true;
    port = config.env.services.tasks.port;
    notion-token = secrets.notion-token;
    dev = true;
  };

  services.voucher-server = {
    enable = true;
    port = config.env.services.voucher-server.port;
    xbgeSecrets = secrets.xbge;
    gardenEnv = {
      gardenApi = config.env.envVars.gardenApi;
      gardenApiUsers = config.env.envVars.gardenApiUsers;
      gardenGraphApi = config.env.envVars.gardenGraphApi;
      gardenSubgraphName = config.env.envVars.gardenSubgraphName;
      gardenRelay = config.env.envVars.gardenRelay;
      gardenHubAddress = config.env.envVars.gardenHubAddress;
      gardenProxyFactoryAddress = config.env.envVars.gardenProxyFactoryAddress;
      gardenSafeMasterAddress = config.env.envVars.gardenSafeMasterAddress;
      gardenEthereumNodeWebSocket = config.env.envVars.gardenEthereumNodeWebSocket;
    };
  };

}
