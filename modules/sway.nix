{ config, options, pkgs, lib, ... }: {
  home.packages = with pkgs; [
    # Build Emacs with Vterm
    ((emacsPackagesNgGen emacsPgtk).emacsWithPackages (epkgs: [ epkgs.vterm ]))
    # Wayland tools
    wofi
    grim
    wl-clipboard
    brightnessctl
    inputs.eww.packages.x86_64.eww
  ];
  wayland.windowManager.sway = {
    enable = true;
    # Sway borders has some issues with my cursor dissapearing (or is sway HEAD, no idea). Anyways, disabled for now
    # package = pkgs.sway-borders;
    wrapperFeatures.gtk = true;
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
      terminal = "${pkgs.foot}/bin/foot";
      menu = "${pkgs.wofi}/bin/wofi --show drun";
      gaps.inner = 18;
      bars = [];
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
      bindsym XF86MonBrightnessDown exec "brightnessctl set 5%-"
      bindsym XF86MonBrightnessUp exec "brightnessctl set +5%"

      # Border Images: needs sway-borders which is finicky at the moment
#     border_images.focused ${../configs/borders/shadows.png}
#     border_images.focused_inactive ${../configs/borders/shadows.png}
#     border_images.unfocused ${../configs/borders/shadows.png}
#     border_images.urgent ${../configs/borders/shadows.png}
    '';
    extraSessionCommands = ''
      export XDG_CURRENT_DESKTOP=sway;
      export WLR_NO_HARDWARE_CURSORS=1
      export SDL_VIDEODRIVER=wayland
      export MOZ_ENABLE_WAYLAND=1
      export _JAVA_AWT_WM_NONREPARENTING=1
    '';
  };
  # foot is a nice wayland native terminal, and I was having openGL issues with alacritty
  programs.foot = {
    enable = true;
    settings = with config.lib.base16.theme; {
      main = {
        font = "Liga SFMono Nerd Font:size=12";
        dpi-aware = "no";
      };
      cursor = {
        style = "beam";
      };
      mouse = {
        hide-when-typing = "yes";
      };
      colors = {
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
  # We only want a wayland-native firefox package on wayland, obviously
  programs.firefox.package = pkgs.wrapFirefox pkgs.firefox-unwrapped {
     forceWayland = true;
     extraPolicies = {
       ExtensionSettings = {};
     };
  };
}
