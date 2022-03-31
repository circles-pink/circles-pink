{ pkgs, config, lib, ... }:
with lib;
let
  cfg = config.services.directus;
  constants = {
    dbHost = "localhost";
    dbUser = "directus";
    dbName = "directus";
    dbPassword = "secret";
    secret = "abcdef";
    key = "xxxxxxx-xxxxxx-xxxxxxxx-xxxxxxxxxx";
    initialAdminEmail = "admin@admin.com";
    initialAdminPassword = "admin";
  };

  directusEnv = {
    PORT = "${toString cfg.port}";
    PUBLIC_URL = "http://localhost:${toString cfg.port}";
    LOG_LEVEL = "info";
    LOG_STYLE = "pretty";
    DB_CLIENT = "mysql";
    DB_HOST = constants.dbHost;
    DB_PORT = toString cfg.mysqlPort;
    DB_DATABASE = constants.dbName;
    DB_USER = constants.dbUser;
    DB_PASSWORD = constants.dbPassword;
    KEY = constants.key;
    SECRET = constants.secret;
    ACCESS_TOKEN_TTL = "15m";
    REFRESH_TOKEN_TTL = "7d";
    REFRESH_TOKEN_COOKIE_SECURE = "false";
    REFRESH_TOKEN_COOKIE_SAME_SITE = "lax";
    REFRESH_TOKEN_COOKIE_NAME = "directus_refresh_token";
    CORS_ENABLED = "true";
    CORS_ORIGIN = "true";
  };

  directusInitTables = pkgs.writeShellScriptBin' "directus-init-tables"
    {
      onPath = [ pkgs.directus pkgs.mariadb ];
      env = directusEnv;
    }
    ''
      mysql -e "DROP DATABASE IF EXISTS ${constants.dbName};"
      mysql -e "CREATE DATABASE ${constants.dbName};"
      mysql -e "GRANT ALL PRIVILEGES ON ${constants.dbName}. * TO '${constants.dbUser}'@'${constants.dbHost}';"
      
      export ADMIN_EMAIL="${constants.initialAdminEmail}";
      export ADMIN_PASSWORD="${constants.initialAdminPassword}";
      directus bootstrap
      directus schema apply --yes ${cfg.schemaJson}

      mysql -e 'USE directus; UPDATE directus_users SET token = "${cfg.directusAdminToken}" WHERE first_name = "Admin";'
    '';

  directus = pkgs.writeShellScriptBin' "directus"
    {
      onPath = [ pkgs.directus ];
      env = directusEnv;
    } ''
    directus $@
  '';

  directusDumpSchema = pkgs.writeShellScriptBin' "directus-dump-schema"
    {
      onPath = [ pkgs.directus pkgs.mariadb pkgs.graphql-zeus ];
      env = directusEnv;
    }
    ''
      set -e
      directus schema snapshot --yes --format json $PWD/directus-schema.json;
      zeus http://localhost:${toString cfg.port}/graphql . --graphql directus-api-public.graphql; rm -rf zeus;
      zeus http://localhost:${toString cfg.port}/graphql/?access_token=${cfg.directusAdminToken} . --graphql directus-api-admin.graphql; rm -rf zeus;
    '';
in
{
  options.services.directus = {
    enable = mkEnableOption "directus service";
    port = mkOption {
      type = types.int;
    };
    mysqlPort = mkOption {
      type = types.int;
    };
    schemaJson = mkOption {
      type = types.path;
    };
    directusAdminToken = mkOption {
      type = types.str;
    };
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [
      directusInitTables
      directusDumpSchema
      directus
      pkgs.mariadb
      pkgs.graphql-zeus
    ];


    systemd.user.services.directus = {
      after = [ "mysql.service" ];
      description = "Circles Directus";
      serviceConfig = {
        Type = "simple";
        ExecStart = "${pkgs.directus}/bin/directus start";
        ExecStop = "pkill directus";
        Restart = "on-failure";
      };
      wantedBy = [ "default.target" ];
      environment = directusEnv;
    };

    services.mysql =
      {
        enable = true;
        package = pkgs.mariadb;
        ensureDatabases = [ constants.dbName ];
        ensureUsers = [{
          name = constants.dbUser;
          ensurePermissions =
            {
              "database.${constants.dbName}" = "ALL PRIVILEGES";
            };
        }];

        settings = {
          mysqld = {
            innodb_buffer_pool_size = "10M";
            port = cfg.mysqlPort;
          };
        };
        initialScript = pkgs.writeText "initDB"
          ''
            CREATE USER '${constants.dbUser}'@'${constants.dbHost}' IDENTIFIED BY '${constants.dbPassword}';
            GRANT ALL PRIVILEGES ON ${constants.dbName}. * TO '${constants.dbUser}'@'${constants.dbHost}';
          '';
      };
  };
}
