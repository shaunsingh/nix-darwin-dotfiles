{ pkgs
, lib
, config
, ...
}: {
  home.packages = with pkgs; [ swaylock-effects ];

  programs.swaylock = {
    settings = with config.lib.base16.theme; {
      screenshots = true;
      clock = true;
      indicator = true;
      indicator-radius = 200;
      indicator-thickness = 20;
      grace = 2;
      grace-no-mouse = true;
      grace-no-touch = true;
      line-uses-ring = false;
      ignore-empty-password = true;
      show-failed-attempts = false;

      font = "Inter";
      timestr = "%I:%M %p";
      datestr = "%d / %m / %y";
      fade-in = "0.2";
      effect-blur = "8x5";
      effect-vignette = "0.5:0.5";

      color = "00000000";

      bs-hl-color = "#${base08-hex}";
      key-hl-color = "#${base0C-hex}";

      inside-color = "#${base01-hex}";
      inside-clear-color = "#${base01-hex}";
      inside-ver-color = "#${base01-hex}";
      inside-wrong-color = "#${base01-hex}";

      line-color = "#${base00-hex}";
      line-ver-color = "#${base00-hex}";
      line-clear-color = "#${base00-hex}";
      line-wrong-color = "#${base00-hex}";

      ring-color = "#${base03-hex}";
      ring-clear-color = "#${base0C-hex}";
      ring-ver-color = "#${base0C-hex}";
      ring-wrong-color = "#${base08-hex}";

      separator-color = "00000000";

      text-color = "#${base06-hex}";
      text-clear-color = "#${base05-hex}";
      text-ver-color = "#${base04-hex}";
      text-wrong-color = "#${base08-hex}";
    };
  };

  services.swayidle = {
    enable = true;
    events = [
      {
        event = "before-sleep";
        command = "${pkgs.swaylock-effects}/bin/swaylock -fF";
      }
      {
        event = "lock";
        command = "${pkgs.swaylock-effects}/bin/swaylock -fF";
      }
    ];

    timeouts = [
      {
        timeout = 300;
        command = "${pkgs.sway}/bin/swaymsg \"output * dpms off\"";
        resumeCommand = "${pkgs.sway}/bin/swaymsg \"output * dpms on\"";
      }
      {
        timeout = 310;
        command = "${pkgs.systemd}/bin/loginctl lock-session";
      }
    ];
  };
}
