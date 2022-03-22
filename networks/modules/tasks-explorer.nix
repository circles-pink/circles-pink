{ pkgs, config, lib, ... }:
with lib;
let
  cfg = config.services.tasks-explorer;
in
{
  options.services.tasks-explorer = {
    enable = mkEnableOption "tasks-explorer service";
    port = mkOption {
      type = types.int;
    };
    notion-token = mkOption {
      type = types.string;
    };
    dev = mkOption {
      type = types.bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    networking.firewall.allowedTCPPorts = [ cfg.port ];

    environment.systemPackages = [ pkgs.circles-pink.tasks-explorer-server ];

    systemd.services.tasks-explorer = {
      description = "Tasks Explorer";
      serviceConfig = {
        Type = "simple";
        ExecStart = "${pkgs.circles-pink.tasks-explorer-server}/bin/tasks-explorer-server";
        ExecStop = "pkill tasks-explorer-server";
        Restart = "on-failure";
      };
      wantedBy = [ "default.target" ];
      environment = {
        PORT = "${toString cfg.port}";
        NOTION_TOKEN = cfg.notion-token;
      } //
      (if cfg.dev then {
        DEV = "1";
      } else { });
    };
  };
}
