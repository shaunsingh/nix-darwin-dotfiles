{ pkgs
, lib
, inputs
, config
, ...
}: {
  systemd.user.services.tp-auto-kbbl = {
    Unit = {
      Description = "Auto toggle keyboard backlight";
      After = ["dbus.service"];
    };
 
    Service = {
      ExecStart = "${pkgs.tp-auto-kbbl}/bin/tp-auto-kbbl -d /dev/input/event1 -n -l -b 255 -t 0";
      Restart = "on-failure";
      Type = "simple";
    };
    Install.WantedBy = ["session.target"];
  };
}
