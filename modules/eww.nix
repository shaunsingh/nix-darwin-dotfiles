{ config
, pkgs
, inputs
, lib
, ...
}: {
  programs.eww = {
    enable = true;
    package = inputs.eww.packages.${pkgs.system}.eww-wayland;
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
      Restart = "on-failure";
    };
    Install.WantedBy = [ "graphical-session.target" ];
  };
}
