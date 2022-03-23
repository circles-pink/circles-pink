{ pkgs, config, lib, ... }:
with lib;
let
  cfg = config.env;
in
{
  options.env = {
    domain = mkOption {
      type = types.string;
    };
  };

  config = mkIf cfg.enable { };
}
