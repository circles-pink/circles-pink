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
  };
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
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [ pkgs.directus ];


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
      environment = {
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
      };
    };

    services.mysql =
      {
        enable = true;
        package = pkgs.mariadb;
        initialDatabases = [{ name = constants.dbName; }];
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
