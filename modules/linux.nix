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
      # colors =  {
      #   focused = {
      #     border = "#${base01-hex}";
      #     background = "#${base0D-hex}";
      #     text = "#${base07-hex}";
      #     indicator = "#${base0D-hex}";
      #     childBorder = "#${base0D-hex}";
      #   };
      #   focusedInactive = {
      #     border = "#${base02-hex}";
      #     background = "#${base04-hex}";
      #     text = "#${base00-hex}";
      #     indicator = "#${base04-hex}";
      #     childBorder = "#${base04-hex}";
      #   };
      #   unfocused = {
      #     border = "#${base01-hex}";
      #     background = "#${base02-hex}";
      #     text = "#${base06-hex}";
      #     indicator = "#${base02-hex}";
      #     childBorder = "#${base02-hex}";
      #   };
      #   urgent = {
      #     border = "#${base03-hex}";
      #     background = "#${base08-hex}";
      #     text = "#${base00-hex}";
      #     indicator = "#${base08-hex}";
      #     childBorder = "#${base08-hex}";
      #   };
      # };
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
