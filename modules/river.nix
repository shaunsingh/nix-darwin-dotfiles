{ pkgs
, lib
, inputs
, config
, ...
}:
let
  terminal = "${pkgs.foot}/bin/foot";
  menu = "${pkgs.rofi-wayland}/bin/rofi -no-lazy-grab -show drun -theme index";
in
{
  imports = [ ./home-manager/wayland/windowManager/river ];
  home.packages = with pkgs; [ wl-clipboard ];
  wayland.windowManager.river = {
    enable = true;
    systemdIntegration = true;
    extraSessionVariables = {
      XDG_SESSION_DESKTOP = "river";
    };
    config = with config.lib.base16.theme; {
      backgroundColor = "${base05}";
      border = {
        color = {
          focused = "${base0C}";
          unfocused = "${base01}";
        };
        width = 0;
      };
      focusFollowsCursor = false;
      layoutGenerator.arguments = "-view-padding 6 -outer-padding 12";
      keybindings =
      let
        mod = "Mod4";
        pwr = base: exp: lib.foldl (x: _: x * base) 1 (lib.range 1 exp);
        allTags = (pwr 2 32) - 1;
        concatAttrs = attrList: lib.fold (x: y: x // y) {} attrList;
        tagBinds = concatAttrs
          (map
            (i: let
              tags = pwr 2 (i - 1);
            in {
              "${mod} ${toString i}" = "spawn 'riverctl set-focused-tags ${toString tags} && ${pkgs.eww}/bin/eww update active-tag=${toString tags}'";
              "${mod}+Shift ${toString i}" = "spawn 'riverctl set-view-tags ${toString tags} && ${pkgs.eww}/bin/eww update passive-tag=${toString tags}'";
              "${mod}+Control ${toString i}" = "spawn 'riverctl toggle-focused-tags ${toString tags} && ${pkgs.eww}/bin/eww update active-tag=${toString tags}'";
              "${mod}+Shift+Control ${toString i}" = "spawn 'riverctl toggle-view-tags ${toString tags} && ${pkgs.eww}/bin/eww update passive-tag=${toString tags}'";
            })
            (lib.range 1 9));
      in {
        normal = tagBinds // {
          "${mod} Return" = "spawn '${terminal}'"; 
          "${mod} Q" = "close";
          "${mod}+Shift Return" = "zoom";
          "${mod} Z" = "focus-output next";
          "${mod}+Shift Z" = "send-to-output next";
          "${mod} Space" = "toggle-float";
          "${mod} F" = "toggle-fullscreen";
          "${mod} D" = "spawn '${menu}'";
          "${mod} X" = "spawn 'power-menu'";

          "${mod} J" = "focus-view next";
          "${mod} K" = "focus-view previous";
          "${mod}+Shift J" = "swap next";
          "${mod}+Shift K" = "swap previous";

          "${mod} H" = ''send-layout-cmd rivertile "main-ratio -0.05"'';
          "${mod} L" = ''send-layout-cmd rivertile "main-ratio +0.05"'';
          "${mod}+Shift H" = ''send-layout-cmd rivertile "main-count +1"'';
          "${mod}+Shift L" = ''send-layout-cmd rivertile "main-count -1"'';

          "${mod} P" = "spawn '${pkgs.screenshot}/bin/screenshot'";
          "${mod}+Shift o" = "spawn '${pkgs.ocrScript}/bin/wl-ocr'";

          "${mod} B" = "spawn '${pkgs.volume}/bin/volume -i 5 && eww update volume-level=`${pkgs.pamixer}/bin/pamixer --get-volume`'";
          "${mod} V" = "spawn '${pkgs.volume}/bin/volume -d 5 && eww update volume-level=`${pkgs.pamixer}/bin/pamixer --get-volume`'";
          "${mod}+Shift B" = "spawn '${pkgs.volume}/bin/volume -t'";
          "${mod}+Shift V" = "spawn '${pkgs.microphone}/bin/microphone -t'";

          "${mod} M" = "spawn '${pkgs.brightness}/bin/brightness set 5%+ && ${pkgs.eww}/bin/eww update brightness-level=`${pkgs.brightnessctl}/bin/brightnessctl -m -d apple-panel-bl | cut -d, -f4 | tr -d %`'";
          "${mod} N" = "spawn '${pkgs.brightness}/bin/brightness set 5%- && ${pkgs.eww}/bin/eww update brightness-level=`${pkgs.brightnessctl}/bin/brightnessctl -m -d apple-panel-bl | cut -d, -f4 | tr -d %`'";
          "${mod}+Shift M" = "spawn '${pkgs.playerctl}/bin/playerctl play-pause'";
          "${mod}+Shift N" = "spawn '${pkgs.playerctl}/bin/playerctl next'";
          "${mod}+Shift P" = "spawn '${pkgs.playerctl}/bin/playerctl previous'";
  
          "${mod} 0" = "set-focused-tags ${toString allTags}";
          "${mod}+Shift 0" = "set-view-tags ${toString allTags}";
  
          "${mod} F11" = "enter-mode passthrough";
        };
        pointer = {
          "${mod} BTN_LEFT" = "move-view";
          "${mod} BTN_RIGHT" = "resize-view";
        };
        passthrough = {
          "${mod} F11" = "enter-mode normal";
        };
        locked = {};
      };
    };
    extraConfig = '' 
      # configure floats and popups 
      riverctl float-filter-add app-id 'float'
      riverctl float-filter-add app-id 'popup'
      
      # input config
      configure_pointers() {
          riverctl list-inputs \
              | grep 'type: pointer' -B 1 \
              | grep -vE 'type: pointer|^--$' \
              | xargs -I '{}' riverctl input '{}' $@
      }
     
      # Touchpad config
      configure_pointers tap enabled
      configure_pointers accel-profile flat
      configure_pointers natural-scroll enabled
      configure_pointers disable-while-typing disabled
      riverctl hide-cursor when-typing disabled

      # exec "gsettings set org.gnome.desktop.wm.preferences button-layout '''"
    '';
  };
  services.kanshi.systemdTarget = "river-session.target";
}
