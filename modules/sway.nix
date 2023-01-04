{ pkgs
, lib
, inputs
, config
, ...
}:
let
  modifier = "Mod4";
  terminal = "${pkgs.foot}/bin/foot";
  menu = "${pkgs.rofi-wayland}/bin/rofi -no-lazy-grab -show drun -theme index";
  ocrScript =
    let
      inherit (pkgs) grim libnotify slurp tesseract5 wl-clipboard;
      _ = lib.getExe;
    in
    pkgs.writeShellScriptBin "wl-ocr" ''
      ${_ grim} -g "$(${_ slurp})" -t ppm - | ${_ tesseract5} - - | ${wl-clipboard}/bin/wl-copy
      ${_ libnotify} "$(${wl-clipboard}/bin/wl-paste)"
    '';
in
{
  home.pointerCursor = {
    name = "phinger-cursors";
    package = pkgs.phinger-cursors;
    size = 32;
  };
  home.packages = with pkgs; [ ocrScript ];
  wayland.windowManager.sway = {
    enable = true;
    systemdIntegration = true;
    package = pkgs.sway-unwrapped;
    seat."*".hide_cursor = "when-typing disable";
    config = {
      input = {
        "keyboard" = {
          xkb_layout = "us";
          xkb_options = "caps:super";
        };
        "type:mouse" = {
          dwt = "disabled";
          accel_profile = "flat";
        };
        "type:touchpad" = {
          dwt = "disabled";
          tap = "enabled";
          accel_profile = "flat";
        };
      };
      gaps.inner = 18;
      gaps.left = 45;
      bars = [ ];
      output = {
        "*" = {
          bg = "${../extra/wallpapers/IBM_Developer_Posters.jpg} fill";
          scale = "2";
        };
      };
      keybindings = {
        "${modifier}+Return" = "exec ${terminal}";
        "${modifier}+x" = "exec power-menu";
        "${modifier}+d" = "exec ${menu}";
        "${modifier}+p" = "exec ${pkgs.grim}/bin/grim 'screenshot-$(date +%Y%m%d%H%M%S).png'";
        "${modifier}+Shift+p" = "exec ${pkgs.grim}/bin/grim - | ${pkgs.wl-clipboard}/bin/wl-copy";
        "${modifier}+g" = "exec ${pkgs.grim}/bin/grim -g \'$(${pkgs.slurp}/bin/slurp)' 'screenshot-$(date +%Y%m%d%H%M%S).png";
        "${modifier}+Shift+g" = "exec ${pkgs.grim}/bin/grim -g '$(${pkgs.slurp}/bin/slurp)' - | ${pkgs.wl-clipboard}/bin/wl-copy";
        "${modifier}+Shift+o" = "exec wl-ocr";
        "${modifier}+Shift+c" = "reload";
        "${modifier}+Shift+e" = "exit";
        "${modifier}+Shift+q" = "kill";
        "${modifier}+r" = ''mode "resize"'';
        "${modifier}+v" = "exec ${pkgs.pamixer}/bin/pamixer -i 5";
        "${modifier}+b" = "exec ${pkgs.pamixer}/bin/pamixer -d 5";
        "${modifier}+Shift+v" = "exec ${pkgs.pamixer}/bin/pamixer -t";
        "${modifier}+Shift+b" = "exec ${pkgs.playerctl}/bin/playerctl play-pause";
        "${modifier}+n" = "exec ${pkgs.brightnessctl}/bin/brightnessctl set 5%-";
        "${modifier}+m" = "exec ${pkgs.brightnessctl}/bin/brightnessctl set +5%";
        "${modifier}+Shift+n" = "exec ${pkgs.playerctl}/bin/playerctl previous";
        "${modifier}+Shift+m" = "exec ${pkgs.playerctl}/bin/playerctl next";
        "${modifier}+h" = "focus left";
        "${modifier}+j" = "focus down";
        "${modifier}+k" = "focus up";
        "${modifier}+l" = "focus right";
        "${modifier}+Shift+h" = "move left";
        "${modifier}+Shift+j" = "move down";
        "${modifier}+Shift+k" = "move up";
        "${modifier}+Shift+l" = "move right";
        "${modifier}+1" = "workspace 1";
        "${modifier}+2" = "workspace 2";
        "${modifier}+3" = "workspace 3";
        "${modifier}+4" = "workspace 4";
        "${modifier}+5" = "workspace 5";
        "${modifier}+6" = "workspace 6";
        "${modifier}+7" = "workspace 7";
        "${modifier}+8" = "workspace 8";
        "${modifier}+9" = "workspace 9";
        "${modifier}+0" = "workspace 10";
        "${modifier}+Shift+1" = "move container to workspace 1";
        "${modifier}+Shift+2" = "move container to workspace 2";
        "${modifier}+Shift+3" = "move container to workspace 3";
        "${modifier}+Shift+4" = "move container to workspace 4";
        "${modifier}+Shift+5" = "move container to workspace 5";
        "${modifier}+Shift+6" = "move container to workspace 6";
        "${modifier}+Shift+7" = "move container to workspace 7";
        "${modifier}+Shift+8" = "move container to workspace 8";
        "${modifier}+Shift+9" = "move container to workspace 9";
        "${modifier}+Shift+0" = "move container to workspace 10";
        "${modifier}+Ctrl+Shift+right" = "move workspace to output right";
        "${modifier}+Ctrl+Shift+left" = "move workspace to output left";
        "${modifier}+Ctrl+Shift+up" = "move workspace to output up";
        "${modifier}+Ctrl+Shift+down" = "move workspace to output down";
        "${modifier}+s" = "layout stacking";
        "${modifier}+w" = "layout tabbed";
        "${modifier}+e" = "layout toggle split";
        "${modifier}+f" = "fullscreen";
        "${modifier}+Shift+space" = "floating toggle";
        "${modifier}+Shift+s" = "sticky toggle";
        "${modifier}+space" = "focus mode_toggle";
        "${modifier}+a" = "focus parent";
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
    };
    extraConfig = ''
      # Remove text on decorations
      for_window [title="."] title_format " "
      default_border normal 0
      default_floating_border normal 0

      for_window [title="."] title_format " "
      default_border normal 0
      default_floating_border normal 0
    '';
    extraSessionCommands = ''
      export XDG_CURRENT_DESKTOP=sway;
      export XDG_SESSION_TYPE = "wayland";
      export XDG_SESSION_DESKTOP =sway;
      export PAN_MESA_DEBUG = "gl3";
      export DISABLE_QT5_COMPAT = "0";
      export QT_AUTO_SCREEN_SCALE_FACTOR = "1";
      export QT_QPA_PLATFORM = "wayland";
      export QT_QPA_PLATFORMTHEME = "qt5ct";
      export QT_STYLE_OVERRIDE = "kvantum";
      export QT_WAYLAND_DISABLE_WINDOWDECORATION = "1";
      export SDL_VIDEODRIVER = "wayland";
      export _JAVA_AWT_WM_NONREPARENTING = "1";
      export CLUTTER_BACKEND = "wayland";
      export GDK_BACKEND = "wayland";
      export MOZ_ENABLE_WAYLAND = "1";
    '';
  };
}
