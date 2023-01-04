{ pkgs
, lib
, inputs
, config
, ...
}: {
  home.packages = with pkgs; [
    wofi
    wl-clipboard
    brightnessctl
  ];
  wayland.windowManager.sway = {
    enable = true;
    package = pkgs.sway-unwrapped;
    config = {
      output = {
        "*" = {
          bg = "\ `find ${../extra/wallpapers/IBM_Developer_Posters.jpg} | shuf -n 1\` fill";
          scale = "2";
        };
      };
      seat."*".hide_cursor = "when-typing disable";
      input = {
        "*".xkb_layout = "us";
        "type:keyboard".xkb_options = "caps:super";
        "type:touchpad".tap = "enabled";
      };
      colors = with config.lib.base16.theme; {
        focused = {
          background = "#${base0E-hex}";
          indicator = "#${base0E-hex}";
          border = "#${base0E-hex}";
          text = "#${base0E-hex}";
          childBorder = "#${base0E-hex}";
        };
        focusedInactive = {
          background = "#${base02-hex}";
          indicator = "#${base02-hex}";
          border = "#${base02-hex}";
          text = "#${base02-hex}";
          childBorder = "#${base02-hex}";
        };
        unfocused = {
          background = "#${base01-hex}";
          indicator = "#${base01-hex}";
          border = "#${base01-hex}";
          text = "#${base01-hex}";
          childBorder = "#${base01-hex}";
        };
        urgent = {
          background = "#${base0A-hex}";
          indicator = "#${base0A-hex}";
          border = "#${base0A-hex}";
          text = "#${base0A-hex}";
          childBorder = "#${base0A-hex}";
        };
      };
      terminal = "${pkgs.foot}/bin/foot";
      menu = "${pkgs.wofi}/bin/wofi --show drun";
      gaps.inner = 18;
      gaps.left = 36;
      bars = [ ];
    };
    extraConfig = ''
      # Remove text on decorations
      for_window [title="."] title_format " "
      default_border normal 0
      default_floating_border normal 0

      for_window [title="."] title_format " "
      default_border normal 0
      default_floating_border normal 0

      # Brightness
      bindsym XF86MonBrightnessDown exec "${pkgs.brightnessctl}/bin/brightnessctl set 5%-"
      bindsym XF86MonBrightnessUp exec "${pkgs.brightnessctl}/bin/brightnessctl set +5%"
    '';
    extraSessionCommands = ''
      export XDG_CURRENT_DESKTOP=sway;
      export MOZ_ENABLE_WAYLAND=1
      export PAN_MESA_DEBUG = "gl3";
      export _JAVA_AWT_WM_NONREPARENTING=1
    '';
  };
}
