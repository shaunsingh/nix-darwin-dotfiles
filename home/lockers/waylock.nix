{
  pkgs,
  lib,
  config,
  ...
}: let
  waylockWrapper = with config.lib.base16.theme;
    pkgs.writeShellScriptBin "waylock"
    "${pkgs.waylock}/bin/waylock -init-color 0x${base00-hex} -input-color 0x${base04-hex} -fail-color 0x${base0A-hex} $@";
  waylockCommand = "${waylockWrapper}/bin/waylock";
in {
  systemd.user.services.waylock-autolock = {
    Unit = {
      PartOf = ["graphical-session.target"];
      Description = "Lock screen automatically after 15 minutes";
      ConditionPathExistsGlob = ["%t/wayland-*"];
    };
    Service = {
      ExecStart = ''
        ${pkgs.swayidle}/bin/swayidle \
          -w timeout 300 ${waylockCommand} \
          before-sleep ${waylockCommand} \
          timeout 330 "${pkgs.wlopm}/bin/wlopm --off \*" \
          resume "${pkgs.wlopm}/bin/wlopm --on \*" \
          lock ${waylockCommand}
      '';
      Restart = "on-failure";
      RestartSec = 1;
    };
    Install.WantedBy = ["graphical-session.target"];
  };
}
