{ pkgs, config, lib, ... }:
with lib;
with types;
let
  cfg = config.env;

  typeUrl = {
    protocol = mkOption {
      type = string;
      default = "https";
    };
    domain = mkOption {
      type = string;
    };
    topLevelDomain = mkOption {
      type = string;
    };
  };

  typeUrl' = {
    subdomain = mkOption {
      type = nullOr string;
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
          };
        });
    };

  };

  config = mkIf cfg.enable { };
}
