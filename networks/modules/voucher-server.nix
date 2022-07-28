{ pkgs, config, lib, ... }:
with lib;
let
  cfg = config.services.voucher-server;
in
{
  options.services.voucher-server = {
    enable = mkEnableOption "voucher-server service";
    port = mkOption {
      type = types.int;
    };
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [ pkgs.circles-pink.voucher-server ];

    systemd.services.voucher-server = {
      description = "voucher-server";
      serviceConfig = {
        Type = "simple";
        ExecStart = "${pkgs.circles-pink.voucher-server}/bin/voucher-server";
        ExecStop = "pkill voucher-server";
        Restart = "on-failure";
      };
      wantedBy = [ "default.target" ];
      environment = {
        PORT = "${toString cfg.port}";
      };
    };
  };
}
