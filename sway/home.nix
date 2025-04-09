{
  inputs,
  lib,
  config,
  pkgs,
  ...
}:

/*
home-manager configuration
Useful links:
- Home Manager Manual: https://nix-community.gitlab.io/home-manager/
- Appendix A. Configuration Options: https://nix-community.gitlab.io/home-manager/options.html
*/

{
  ### -- home
  home = {
    packages = builtins.attrValues {
      inherit
        (pkgs)
        wayland-utils
        xdg-utils
        grim
        slurp
        swaybg
        swayidle
        swaylock
        wf-recorder
        wl-clipboard
        wlogout
        doublecmd
        legcord
        chromium
        gh
        ;
    };

    sessionPath = [
      "${config.xdg.configHome}/scripts"
      "${config.home.homeDirectory}/.local/bin"
    ];

    stateVersion = "23.05";
  };

  ### - cursor 
  home.pointerCursor = {
    enable = true;
    gtk.enable = true;
    sway.enable = true;
    name = "macOS-Monterey";
    package = pkgs.apple-cursor;
    size = 64;
  }; 

  ### -- sway
  wayland.windowManager.sway = {
    enable = true;
    # package = pkgs.swayfx-unwrapped;
    systemd.enable = true;
    extraSessionCommands = ''
      export NIXOS_OZONE_WL=1
      export XDG_SESSION_DESKTOP=sway
      export SDL_VIDEODRIVER=wayland
      export QT_QPA_PLATFORMTHEME=qt5ct
      export QT_WAYLAND_DISABLE_WINDOWDECORATION="1"
      export CLUTTER_BACKEND=wayland
      export ECORE_EVAS_ENGINE=wayland-egl
      export ELM_ENGINE=wayland_egl
      export NO_AT_BRIDGE=1
      export _JAVA_AWT_WM_NONREPARENTING=1
    '';
    wrapperFeatures.gtk = true;
    config = {
      window = {
        titlebar = true;
        border = 0;
      };
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
          tap = "enabled";
          natural_scroll = "enabled";
          accel_profile = "adaptive";
          scroll_factor = "0.45";
          pointer_accel = "0.27";
        };
      };
      output = {
        "*" = {
          background = "#000000 solid_color";
        };
      };
      bars = lib.mkForce [];
      defaultWorkspace = "workspace 1";
      keybindings = let
        modifier = "Mod4";
        concatAttrs = lib.fold (x: y: x // y) {};
        tagBinds =
          concatAttrs
          (map
            (i: {
              "${modifier}+${toString i}" = "exec 'swaymsg workspace ${toString i}'";
              "${modifier}+Shift+${toString i}" = "exec 'swaymsg move container to workspace ${toString i}'";
            })
            (lib.range 0 9));
        screenshot = pkgs.writeShellScriptBin "screenshot" ''
          ${pkgs.grim}/bin/grim -g "$(${pkgs.slurp}/bin/slurp)" - -t png | ${pkgs.wl-clipboard}/bin/wl-copy -t image/png
        '';
        ocrScript = let
          inherit (pkgs) grim libnotify slurp tesseract5 wl-clipboard;
          _ = pkgs.lib.getExe;
        in
          pkgs.writeShellScriptBin "wl-ocr" ''
            ${_ grim} -g "$(${_ slurp})" -t ppm - | ${_ tesseract5} - - | ${wl-clipboard}/bin/wl-copy
            ${_ libnotify} "$(${wl-clipboard}/bin/wl-paste)"
          '';
        volume = pkgs.writeShellScriptBin "volume" ''
          #!/bin/sh

          ${pkgs.pamixer}/bin/pamixer "$@"
          volume="$(${pkgs.pamixer}/bin/pamixer --get-volume)"

          if [ $volume = 0 ]; then
              ${pkgs.libnotify}/bin/notify-send -r 69 \
                  -a "Volume" \
                  "Muted" \
                  -t 888 \
                  -u low
          else
              ${pkgs.libnotify}/bin/notify-send -r 69 \
                  -a "Volume" "Currently at $volume%" \
                  -h int:value:"$volume" \
                  -t 888 \
                  -u low
          fi
        '';
        microphone = pkgs.writeShellScriptBin "microphone" ''
          #!/bin/sh

          ${pkgs.pamixer}/bin/pamixer --default-source "$@"
          mic="$(${pkgs.pamixer}/bin/pamixer --default-source --get-volume-human)"

          if [ "$mic" = "muted" ]; then
              ${pkgs.libnotify}/bin/notify-send -r 69 \
                  -a "Microphone" \
                  "Muted" \
                  -t 888 \
                  -u low
          else
            ${pkgs.libnotify}/bin/notify-send -r 69 \
                  -a "Microphone" "Currently at $mic" \
                  -h int:value:"$mic" \
                  -t 888 \
                  -u low
          fi
        '';
        brightness = let
          brightnessctl = pkgs.brightnessctl + "/bin/brightnessctl";
        in
          pkgs.writeShellScriptBin "brightness" ''
            #!/bin/sh

            ${brightnessctl} "$@"
            brightness=$(echo $(($(${brightnessctl} g) * 100 / $(${brightnessctl} m))))

            ${pkgs.libnotify}/bin/notify-send -r 69 \
                -a "Brightness" "Currently at $brightness%" \
                -h int:value:"$brightness" \
                -t 888 \
                -u low
          '';
      in
        tagBinds
        // {
          "${modifier}+Return" = "exec ${pkgs.foot}/bin/foot";
          # "${modifier}+p" = "exec ${screenshot}/bin/screenshot";
          "${modifier}+p" = "exec ${pkgs.grim}/bin/grim";
          "${modifier}+Shift+p" = "exec ${ocrScript}/bin/wl-ocr";
          "${modifier}+v" = "exec ${volume}/bin/volume -d 5";
          "${modifier}+b" = "exec ${volume}/bin/volume -i 5";
          "${modifier}+Shift+v" = "exec ${pkgs.playerctl}/bin/playerctl play-pause";
          "${modifier}+Shift+b" = "exec ${volume}/bin/volume -t";
          "${modifier}+n" = "exec ${brightness}/bin/brightness set 5%-";
          "${modifier}+m" = "exec ${brightness}/bin/brightness set 5%+";
          "${modifier}+Shift+n" = "exec ${pkgs.playerctl}/bin/playerctl previous";
          "${modifier}+Shift+m" = "exec ${pkgs.playerctl}/bin/playerctl next";
          "${modifier}+q" = "kill";
          "${modifier}+r" = ''mode "resize"'';
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
      colors = {
        focused = {
          background = "#f2f4f8";
          indicator = "#f2f4f8";
          border = "#f2f4f8";
          text = "#f2f4f8";
          childBorder = "#f2f4f8";
        };
        focusedInactive = {
          background = "#262626";
          indicator = "#262626";
          border = "#262626";
          text = "#262626";
          childBorder = "#262626";
        };
        unfocused = {
          background = "#262626";
          indicator = "#262626";
          border = "#262626";
          text = "#262626";
          childBorder = "#262626";
        };
        urgent = {
          background = "#ee5396";
          indicator = "#ee5396";
          border = "#ee5396";
          text = "#ee5396";
          childBorder = "#ee5396";
        };
      };
    };
    extraConfig = ''
      default_border none
      default_floating_border none

      bindgesture swipe:3:right workspace next
      bindgesture swipe:3:left workspace prev

      # swayfx
      # shadows enable
    '';
  };
  services.kanshi.systemdTarget = "sway-session.target";

  ### -- display
  services.kanshi = {
    enable = true;
    settings = [
      {
        profile = {
          name = "undocked";
          outputs = [
            {
              criteria = "eDP-1";
              status = "enable";
              scale = 2.0; 
            }
            {
              criteria = "$middleLG34";
              status = "enable";
            }
            {
              criteria = "$rightLG27";
              status = "enable";
            }
          ];
        };
      }
    ];
  };

  ### -- terminal
  programs.foot = {
    enable = true;
    settings = {
      main = {
        font = "Liga SFMono Nerd Font:size=11";
        pad = "27x27";
        dpi-aware = "no";
      };
      mouse.hide-when-typing = "yes";
      scrollback.lines = 32768;
      url.launch = "${pkgs.xdg-utils}/bin/xdg-open \${url}";
      tweak.grapheme-shaping = "yes";
      cursor.style = "beam";
      colors = {
        background = "161616";
        foreground = "ffffff";
        regular0 = "161616";
        regular1 = "78a9ff";
        regular2 = "ff7eb6";
        regular3 = "42be65";
        regular4 = "08bdba";
        regular5 = "82cfff";
        regular6 = "33b1ff";
        regular7 = "dde1e6";
        bright0 = "525252";
        bright1 = "78a9ff";
        bright2 = "ff7eb6";
        bright3 = "42be65";
        bright4 = "08bdba";
        bright5 = "82cfff";
        bright6 = "33b1ff";
        bright7 = "ffffff";
      };
    };
  };

  ### -- notifications
  services.dunst = {
    enable = true;
    settings = {
      global = {
        follow = "mouse";
        width = 300;
        origin = "top-left";
        notification_limit = 0;
        offset = "18x18";
        icon_position = "off";
        progress_bar_height = 9;
        progress_bar_frame_width = 0;
        padding = 18;
        horizontal_padding = 18;
        frame_width = 0;
        gap_size = 9;
        font = "Liga SFMono Nerd Font 11";
        format = "<span size='x-large' font_desc='Liga SFMono Nerd Font 9' weight='bold' foreground='#dde1e6'>%a</span>\\n%s\\n%b";
        show_indicators = false;
        mouse_left_click = "do_action";
        mouse_middle_click = "close_all";
        mouse_right_click = "close_current";
        ellipsize = "end";
        markup = "full";
      };

      urgency_low = {
        timeout = 3;
        background = "#131313";
        foreground = "#dde1e6";
        highlight = "#be95ff";
      };
      urgency_normal = {
        timeout = 6;
        background = "#131313";
        foreground = "#dde1e6";
        highlight = "#3ddbd9";
      };
      urgency_critical = {
        timeout = 0;
        background = "#131313";
        foreground = "#dde1e6";
        highlight = "#ff7eb6";
      };
    };
  };

  ### -- swayidle
  services.swayidle = let
    display = status: "swaymsg 'output * power ${status}'";
  in {
    enable = true;
    events = [
      {
        event = "before-sleep";
        command = display "off";
      }
      {
        event = "before-sleep";
        command = "swaylock";
      }
      {
        event = "after-resume";
        command = display "on";
      }
      {
        event = "lock";
        command = display "off";
      }
      {
        event = "unlock";
        command = display "on";
      }
    ];
    timeouts = [
      {
        timeout = 300;
        command = display "off";
        resumeCommand = display "on";
      }
      {
        timeout = 310;
        command = "swaylock";
      }
    ];
  };
}
