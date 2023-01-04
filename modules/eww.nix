{ config
, pkgs
, inputs
, lib
, ...
}:
let
  dependencies = with pkgs; [
    # deps
  ];
in
{
  programs.eww = {
    enable = true;
    package = pkgs.eww;
    configDir = ../configs/eww;
  };

  systemd.user.services.eww = {
    Unit = {
      Description = "Eww Daemon";
      PartOf = [ "graphical-session.target" ];
    };
    Service = {
      Environment = "PATH=/run/wrappers/bin:${lib.makeBinPath dependencies}";
      ExecStart = "${config.programs.eww.package}/bin/eww daemon --no-daemonize";
      ExecStartPost = "${config.programs.eww.package}/bin/eww open bar";
      ExecStopPre = "${config.programs.eww.package}/bin/eww close bar";
      Restart = "on-failure";
    };
    Install.WantedBy = [ "graphical-session.target" ];
  };
}
