{ pkgs
, lib
, config
, ...
}:
let
  cfg = config.services.xremap;
  configFile = pkgs.writeTextFile {
    name = "xremap-config.yml";
    text = pkgs.lib.generators.toYAML { } cfg.config;
  };
in
{
  config = lib.mkIf (cfg.serviceMode == "user") {
    hardware.uinput.enable = true;
    /*
      services.udev.extraRules =
    */
    /*
      ''
    */
    /*
      KERNEL=="uinput", GROUP="input", MODE="0660"
    */
    /*
      '';
    */
    # Uinput group owns the /uinput
    users.groups.uinput.members = [
      cfg.userName
    ];
    # To allow access to /dev/input
    users.groups.input.members = [
      cfg.userName
    ];
    systemd.user.services.xremap = {
      description = "xremap user service";
      path = [ cfg.package ];
      wantedBy = [ "graphical-session.target" ];
      serviceConfig = {
        KeyringMode = "private";
        SystemCallArchitectures = [ "native" ];
        RestrictRealtime = true;
        SystemCallFilter = map (x: "~@${x}") [ "clock" "debug" "module" "reboot" "swap" "cpu-emulation" "obsolete" "privileged" "resources" ];
        LockPersonality = true;
        UMask = "077";
        RestrictAddressFamilies = "AF_UNIX";
        ExecStart = ''
          ${cfg.package}/bin/xremap ${
            if cfg.deviceName != ""
            then "--device \"${cfg.deviceName}\""
            else ""
          } ${
            if cfg.watch
            then "--watch"
            else ""
          } ${configFile}
        '';
      };
    };
  };
}
