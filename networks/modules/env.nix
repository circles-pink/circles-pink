{ pkgs, config, lib, ... }:
with lib;
let
  cfg = config.env;
in
{
  options.env = {
    url = {
      protocol = mkOption {
        type = types.string;
        default = "https";
      };
      domain = mkOption {
        type = types.string;
      };
      topLevelDomain = mkOption {
        type = types.string;
      };
    };
  };

  config = mkIf cfg.enable { };
}
