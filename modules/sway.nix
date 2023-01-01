{ pkgs
, lib
, inputs
, ...
}: {
  home.packages = with pkgs; [
    wofi
    wl-clipboard
    brightnessctl
  ];
  wayland.windowManager.sway = {
    enable = true;
    package = pkgs.sway-borders-git;
    config = {
      output = {
        "*" = {
          bg = "\ `find ${../extra/wallpapers/IBM_Developer_Posters.jpg} | shuf -n 1\` fill";
        };
      };
      input = {
        "*".xkb_layout = "us";
        "type:keyboard".xkb_options = "caps:super";
        "type:touchpad".tap = "enabled";
      };
      colors = with config.lib.base16.theme; {
        focused = {
          border = "#${base01-hex}";
          background = "#${base03-hex}";
          text = "#${base03-hex}";
          indicator = "#${base03-hex}";
          childBorder = "#${base03-hex}";
        };
        focusedInactive = {
          border = "#${base02-hex}";
          background = "#${base0A-hex}";
          text = "#${base0A-hex}";
          indicator = "#${base0A-hex}";
          childBorder = "#${base0A-hex}";
        };
        unfocused = {
          border = "#${base01-hex}";
          background = "#${base02-hex}";
          text = "#${base02-hex}";
          indicator = "#${base02-hex}";
          childBorder = "#${base02-hex}";
        };
        urgent = {
          border = "#${base03-hex}";
          background = "#${base0C-hex}";
          text = "#${base0C-hex}";
          indicator = "#${base0C-hex}";
          childBorder = "#${base0C-hex}";
        };
      };
      terminal = "${pkgs.alacritty-ligatures}/bin/alacritty";
      menu = "${pkgs.wofi}/bin/wofi --show drun";
      gaps.inner = 18;
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

      # Border Images: needs sway-borders which is finicky at the moment
      border_images.focused ${../configs/borders/shadows.png}
      border_images.focused_inactive ${../configs/borders/shadows.png}
      border_images.unfocused ${../configs/borders/shadows.png}
      border_images.urgent ${../configs/borders/shadows.png}
    '';
    extraSessionCommands = ''
      export XDG_CURRENT_DESKTOP=sway;
      export WLR_NO_HARDWARE_CURSORS=1
      export SDL_VIDEODRIVER=wayland
      export MOZ_ENABLE_WAYLAND=1
      export _JAVA_AWT_WM_NONREPARENTING=1
    '';
  };
}
