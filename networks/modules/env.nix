{ pkgs, config, lib, ... }:
with lib;
with types;
let
  cfg = config.env;

  typeUrl = {
    protocol = mkOption {
      type = str;
      default = "https";
    };
    domain = mkOption {
      type = str;
    };
    topLevelDomain = mkOption {
      type = str;
    };
  };

  typeUrl' = {
    subdomain = mkOption {
      type = nullOr str;
      default = null;
    };
    path = mkOption {
      type = nullOr str;
      default = null;
    };
  };
in
{
  options.env = {
    url = typeUrl;

    services = mkOption {
      type = attrsOf
        (submodule {
          options = {
            url = mkOption {
              type = submodule {
                options = typeUrl // typeUrl';
              };
            };
            port = mkOption { type = int; };
            locations = mkOption {
              type = nullOr (attrsOf str);
              default = null;
            };
          };
        });
    };

    envVars = mkOption {
      type = attrsOf str;
    };

  };

  config = mkIf cfg.enable { };
}
