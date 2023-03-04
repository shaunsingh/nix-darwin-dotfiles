{
  config,
  pkgs,
  lib,
  ...
}:
with lib; let
  cfg = config.services.hardware.tp-auto-kbbl;
in {
  options.services.hardware.tp-auto-kbbl.enable = mkEnableOption "tp-auto-kbbl, a tool to automatically toggle keyboard backlight";

  config = mkIf cfg.enable {
    systemd.user.services.tp-auto-kbbl = {
      Unit = {
        Description = "automatically toggle keyboard backlight";
        After = ["dbus.service"];
      };
      Service = {
        ExecStart = "${pkgs.tp-auto-kbbl}/bin/tp-auto-kbbl -d /dev/input/event1 -n -l -b 255 -t 0";
        Restart = "on-failure";
        Type = "simple";
      };
      Install.WantedBy = ["session.target"];
    };
  };
}
