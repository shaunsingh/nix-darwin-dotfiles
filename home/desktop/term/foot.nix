{ pkgs
, lib
, inputs
, config
, ...
}: {
  programs.foot = {
    enable = true;
    settings = {
      main = {
        font = "Liga SFMono Nerd Font:size=11";
        pad = "27x27";
        dpi-aware = "no";
        notify = "${pkgs.libnotify}/bin/notify-send -a foot -i foot \${title} \${body}";
      };
      mouse.hide-when-typing = "yes";
      scrollback.lines = 32768;
      url.launch = "${pkgs.xdg-utils}/bin/xdg-open \${url}";
      tweak.grapheme-shaping = "yes";
      cursor.style = "beam";
      colors = with config.lib.base16.theme; {
        background = "${base00-hex}";
        foreground = "${base06-hex}";
        regular0 = "${base00-hex}";
        regular1 = "${base0B-hex}";
        regular2 = "${base0C-hex}";
        regular3 = "${base0D-hex}";
        regular4 = "${base07-hex}";
        regular5 = "${base0F-hex}";
        regular6 = "${base09-hex}";
        regular7 = "${base04-hex}";
        bright0 = "${base03-hex}";
        bright1 = "${base0B-hex}";
        bright2 = "${base0C-hex}";
        bright3 = "${base0D-hex}";
        bright4 = "${base07-hex}";
        bright5 = "${base0F-hex}";
        bright6 = "${base09-hex}";
        bright7 = "${base06-hex}";
      };
    };
  };
}
