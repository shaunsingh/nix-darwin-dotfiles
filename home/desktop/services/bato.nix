{
  pkgs,
  lib,
  inputs,
  config,
  ...
}: {
  xdg.configFile."bato/bato.yaml".text = ''
    critical_level: 7
    low_level: 25
    critical:
      summary: Battery critical
      body: Please connect to AC power
    low:
      summary: Battery low
    full:
      summary: Battery full
    charging:
      summary: Battery
      body: Charging
    discharging:
      summary: Battery
      body: Discharging
  '';
  systemd.user.services.bato = {
    Unit = {
      Description = "Bato daemon";
      PartOf = ["default.target"];
    };
    Service = {
      ExecStart = "${pkgs.bato}/bin/bato";
      Restart = "on-failure";
    };
    Install.WantedBy = ["default.target"];
  };
}
