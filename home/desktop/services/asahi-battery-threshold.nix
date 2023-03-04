{
  pkgs,
  lib,
  inputs,
  config,
  ...
}: {
  systemd.user.services.battery-threshold = {
    Unit = {
      Description = "Asahi battery threshold";
      PartOf = ["default.target"];
    };
    Service = {
      ExecStart = "${pkgs.asahi-battery-threshold}/bin/asahi-battery-threshold";
      ExecStop = "echo auto > /sys/class/power_supply/macsmc-battery/charge_behaviour";
      Restart = "on-failure";
    };
    Install.WantedBy = ["default.target"];
  };
}
