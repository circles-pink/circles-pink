{ pkgs, config, lib, ... }:
with lib;
let
  cfg = config.services.voucher-server;
in
with types;
{
  options.services.voucher-server = {
    enable = mkEnableOption "voucher-server service";
    port = mkOption {
      type = types.int;
    };
    xbgeSecrets = mkOption {
      type = submodule
        {
          options = {
            voucherCodeSecret = mkOption { type = str; };
            authSecret = mkOption { type = str; };
            endpoint = mkOption { type = str; };
            safeAddress = mkOption { type = str; };
            privKey = mkOption { type = str; };
          };
        };
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
        VOUCHER_CODE_SECRET = "${toString cfg.xbgeSecrets.voucherCodeSecret}";
        XBGE_AUTH_SECRET = "${toString cfg.xbgeSecrets.authSecret}";
        XBGE_ENDPOINT = "${toString cfg.xbgeSecrets.endpoint}";
        XBGE_SAFE_ADDRESS = "${toString cfg.xbgeSecrets.safeAddress}";
        XBGE_KEY = "${toString cfg.xbgeSecrets.privKey}";
      };
    };
  };
}
