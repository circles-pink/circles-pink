{ pkgs, config, lib, ... }:
with lib;
let
  cfg = config.network-config;

in
{
  options.network-config = with types; {

    webserver = {
      services.storybook.enable = mkOption {
        type = bool;
        default = true;
      };
    };
  };

  config = { };
}
