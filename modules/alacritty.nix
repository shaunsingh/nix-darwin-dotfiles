{ pkgs
, lib
, inputs
, config
, ...
}: {
  programs.alacritty = {
    enable = true;
    package = pkgs.alacritty-ligatures;
    settings = with config.lib.base16.theme; {
      window.padding.x = 45;
      window.padding.y = 45;
      window.decorations = "none";
      mouse.hide_when_typing = true;
      use_thin_strokes = true;
      cursor.style = "Beam";

      font = {
        size = 15;
        normal.family = "Liga SFMono Nerd Font";
        normal.style = "Light";
        bold.family = "Liga SFMono Nerd Font";
        bold.style = "Bold";
        italic.family = "Liga SFMono Nerd Font";
        italic.style = "Italic";
      };

      colors = {
        cursor.cursor = "#${base04-hex}";
        primary.background = "#${base00-hex}";
        primary.foreground = "#${base06-hex}";
        normal = {
          black = "#${base00-hex}";
          red = "#${base0B-hex}";
          green = "#${base0C-hex}";
          yellow = "#${base0D-hex}";
          blue = "#${base07-hex}";
          magenta = "#${base0F-hex}";
          cyan = "#${base09-hex}";
          white = "#${base04-hex}";
        };
        bright = {
          black = "#${base03-hex}";
          red = "#${base0B-hex}";
          green = "#${base0C-hex}";
          yellow = "#${base0D-hex}";
          blue = "#${base07-hex}";
          magenta = "#${base0F-hex}";
          cyan = "#${base09-hex}";
          white = "#${base06-hex}";
        };
      };
    };
  };
}
