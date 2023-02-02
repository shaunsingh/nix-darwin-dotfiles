{ pkgs
, lib
, inputs
, config
, ...
}:
let
  gsettings = "${pkgs.glib}/bin/gsettings";
  gsettingsScript = pkgs.writeShellScript "gsettings-auto.sh" ''
    expression=""
    for pair in "$@"; do
      IFS=:; set -- $pair
      expressions="$expressions -e 's:^$2=(.*)$:${gsettings} set org.gnome.desktop.interface $1 \1:e'"
    done
    IFS=
    echo "" >/tmp/gsettings.log
    echo exec sed -E $expressions "''${XDG_CONFIG_HOME:-$HOME/.config}"/gtk-3.0/settings.ini &>>/tmp/gsettings.log
    eval exec sed -E $expressions "''${XDG_CONFIG_HOME:-$HOME/.config}"/gtk-3.0/settings.ini &>>/tmp/gsettings.log
  '';
  gsettingsCommand = ''
    ${gsettingsScript} \
      gtk-theme:gtk-theme-name \
      icon-theme:gtk-icon-theme-name \
      cursor-theme:gtk-cursor-theme-name
  '';
in
{
  home.packages = with pkgs; [ wl-clipboard ];
  services.kanshi.systemdTarget = "sway-session.target";
  wayland.windowManager.sway = {
    enable = true;
    systemdIntegration = true;
    wrapperFeatures.tk = true;
    package = pkgs.swayfx-unwrapped;
    config = {
      startup = [
        {
          always = true;
          command = "${gsettingsCommand}";
        }
      ];
      seat."*".hide_cursor = "when-typing disable";
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
      bars = [{ command = "${pkgs.eww}/bin/eww open bar"; }];
      gaps.inner = 18;
      defaultWorkspace = "workspace 1";
      keybindings = let 
        modifier = "Mod4";
        terminal = "${pkgs.foot}/bin/foot";
        menu = "${pkgs.rofi-wayland}/bin/rofi -no-lazy-grab -show drun -theme index";
        concatAttrs = attrList: lib.fold (x: y: x // y) {} attrList;
        tagBinds = concatAttrs
          (map
            (i: {
              "${modifier}+${toString i}" = "exec 'swaymsg workspace ${toString i} && ${pkgs.eww}/bin/eww update active-tag=${toString i}'";
              "${modifier}+Shift+${toString i}" = "exec 'swaymsg move container to workspace ${toString i}'";
            })
            (lib.range 0 9));
      in tagBinds // {
        "${modifier}+Return" = "exec ${terminal}";
        "${modifier}+x" = "exec power-menu";
        "${modifier}+d" = "exec ${menu}";
        "${modifier}+p" = "exec ${pkgs.screenshot}/bin/screenshot";
        "${modifier}+Shift+p" = "exec ${pkgs.ocrScript}/bin/wl-ocr";
        "${modifier}+q" = "kill";
        "${modifier}+r" = ''mode "resize"'';
        "${modifier}+v" = "exec ${pkgs.volume}/bin/volume -d 5";
        "${modifier}+b" = "exec ${pkgs.volume}/bin/volume -i 5";
        "${modifier}+Shift+v" = "exec ${pkgs.playerctl}/bin/playerctl play-pause";
        "${modifier}+Shift+b" = "exec ${pkgs.volume}/bin/volume -t";
        "${modifier}+n" = "exec ${pkgs.brightness}/bin/brightness set 5%-";
        "${modifier}+m" = "exec ${pkgs.brightness}/bin/brightness set 5%+";
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
        "${modifier}+s" = "layout stacking";
        "${modifier}+w" = "layout tabbed";
        "${modifier}+e" = "layout toggle split";
        "${modifier}+f" = "fullscreen";
        "${modifier}+space" = "floating toggle";
        "${modifier}+Shift+s" = "sticky toggle";
        "${modifier}+Shift+space" = "focus mode_toggle";
        "${modifier}+a" = "focus parent";
        "${modifier}+Shift+c" = "reload";
        "${modifier}+Shift+e" = "exit";
      };
      colors = with config.lib.base16.theme; {
        focused = {
          background = "#${base01-hex}";
          indicator = "#${base01-hex}";
          border = "#${base01-hex}";
          text = "#${base01-hex}";
          childBorder = "#${base01-hex}";
        };
        focusedInactive = {
          background = "#${base01-hex}";
          indicator = "#${base01-hex}";
          border = "#${base01-hex}";
          text = "#${base01-hex}";
          childBorder = "#${base01-hex}";
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

      # swayfx 
      # shadows on
      # shadow_blur_radius 27
      corner_radius 3
    '';
    extraSessionCommands = ''
      export XDG_CURRENT_DESKTOP=sway;
    '';
  };
}
