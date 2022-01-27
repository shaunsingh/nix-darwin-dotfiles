{ config, options, lib, ... }: {
  wayland.windowManager.sway = {
    enable = true;
    wrapperFeatures.gtk = true;
    config = with config.lib.base16.theme; {
      bars = [{ command = "waybar"; }];
      fonts = {
        names = [ "IBM Plex Sans" "Liga SFMono Nerd Font" ];
        size = 16.0;
      };
      gaps.inner = 20;
      input."type:keyboard" = {
        xkb_variant = ",qwerty";
        xkb_options = "grp:alt_caps_toggle";
      };
      input."type:touchpad" = {
        tap = "enabled";
        natural_scroll = "enabled";
        scroll_method = "two_finger";
      };
      keybindings = let
        mod = config.wayland.windowManager.sway.config.modifier;
      in lib.mkOptionDefault {
        # Shortcuts for easier navigation between workspaces
        "${mod}+Control+Left" = "workspace prev";
        "${mod}+Control+l" = "workspace prev";
        "${mod}+Control+Right" = "workspace next";
        "${mod}+Control+h" = "workspace next";
        "${mod}+Tab" = "workspace back_and_forth";
        "${mod}+Enter" = "exec alacritty";
        "${mod}+Shift+e" = "exec nwgbar -o 0.2";
      };
      colors =  {
        focused = {
          border = "0x${base01-hex}";
          background = "0x${base0D-hex}";
          text = "0x${base07-hex}";
          indicator = "0x${base0D-hex}";
          childBorder = "0x${base0D-hex}";
        };
        focusedInactive = {
          border = "0x${base02-hex}";
          background = "0x${base04-hex}";
          text = "0x${base00-hex}";
          indicator = "0x${base04-hex}";
          childBorder = "0x${base04-hex}";
        };
        unfocused = {
          border = "0x${base01-hex}";
          background = "0x${base02-hex}";
          text = "0x${base06-hex}";
          indicator = "0x${base02-hex}";
          childBorder = "0x${base02-hex}";
        };
        urgent = {
          border = "0x${base03-hex}";
          background = "0x${base08-hex}";
          text = "0x${base00-hex}";
          indicator = "0x${base08-hex}";
          childBorder = "0x${base08-hex}";
        };
      };
      modifier = "Mod4";
      window.border = 0;
    };
    extraSessionCommands = ''
      export XDG_SESSION_TYPE=wayland
      export XDG_SESSION_DESKTOP=sway
      export XDG_CURRENT_DESKTOP=sway
    '';
  };
}
