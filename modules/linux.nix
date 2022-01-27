{ config, options, lib, ... }: {
  wayland.windowManager.sway = {
    enable = true;
    wrapperFeatures.gtk = true;
    config = {
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
      modifier = "Mod4";
      window.border = 0;
    };
    extraSessionCommands = ''
      export XDG_SESSION_TYPE=wayland
      export XDG_SESSION_DESKTOP=sway
      export XDG_CURRENT_DESKTOP=sway
    '';
    extraConfig = ''
      source ${config.lib.base16.templateFile { name = "sway"; }}
    '';
  };
}
