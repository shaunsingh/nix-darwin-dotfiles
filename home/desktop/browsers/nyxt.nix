{
  pkgs,
  lib,
  inputs,
  config,
  ...
}: {
  home.packages = with pkgs; [
    nyxt-dev
    mpv
    python3Packages.grip
  ];
  xdg.configFile."nyxt" = {
    source = ../../../configs/nyxt;
    recursive = true;
  };
  xdg.dataFile."nyxt/extensions/nx-kaomoji" = {
    source = inputs.nx-kaomoji-src;
    recursive = true;
  };
  xdg.dataFile."nyxt/extensions/nx-dark-reader" = {
    source = inputs.nx-dark-reader-src;
    recursive = true;
  };
  xdg.dataFile."nyxt/extensions/nx-search-engines" = {
    source = inputs.nx-search-engines-src;
    recursive = true;
  };
  xdg.dataFile."nyxt/extensions/nx-notmuch" = {
    source = inputs.nx-notmuch-src;
    recursive = true;
  };
  systemd.user.services.waylock-autolock = {
    Unit = {
      PartOf = ["graphical-session.target"];
      After = ["network.target"];
      Description = "Launch c backend for xterm";
    };
    Service = {
      ExecStart = ''
        ${pkgs.ttyd}/bin/ttyd -t rendererType=canvas -t 'theme={"background": "#161616", "foreground": "#ffffff", "cursor": "#f2f4f8", "black": "#161616", "red": "#78a9ff", "green": "#ff7eb6", "yellow": "#42be65", "blue": "#08bdba", "magenta": "#82cfff", "cyan": "#33b1ff", "white": "#f2f4f8", "brightBlack": "#c1c7cd", "brightRed": "#78a9ff", "brightGreen": "#ff7eb6", "brightYellow": "#42be65", "brightBlue": "#08bdba", "brightMagenta": "#82cfff", "brightCyan": "#33b1ff", "brightWhite": "#ffffff"}' -t disableLeaveAlert=true -t disableResizeOverlay=true  -t enableSixel=true -t lineHeight=1.18 -t fontSize=15 fish
      '';
      Restart = "on-failure";
      RestartSec = 1;
    };
    Install.WantedBy = ["graphical-session.target"];
  };
}

#   xdg.configFile."nyxt/base16.lisp".text = with config.lib.base16.theme; ''
#     (defmacro define-palette (&rest colors)
#       "Helper macro to set global variables for `theme' colors."
#       `(progn ,@(loop for (name hex)
#                       in colors
#                       collect `(defparameter ,name ,hex "Color used for `theme'."))))
# 
#     (define-palette (*base00* "#${base00-hex}")
#                     (*base01* "#${base01-hex}")
#                     (*base02* "#${base02-hex}")
#                     (*base03* "#${base03-hex}")
#                     (*base04* "#${base04-hex}")
#                     (*base05* "#${base05-hex}")
#                     (*base06* "#${base06-hex}")
#                     (*base07* "#${base07-hex}")
#                     (*base08* "#${base08-hex}")
#                     (*base09* "#${base09-hex}")
#                     (*base0A* "#${base0A-hex}")
#                     (*base0B* "#${base0B-hex}")
#                     (*base0C* "#${base0C-hex}")
#                     (*base0D* "#${base0D-hex}")
#                     (*base0E* "#${base0E-hex}")
#                     (*base0F* "#${base0F-hex}")
#                     (*font* "Liga SFMono Nerd Font"))
# 
#     # builtin theme support
#     (define-configuration browser
#       ((theme (make-instance 'theme:theme
#                              :dark-p nil
#                              :background-color "#${base00-hex}"
#                              :on-background-color "#${base05-hex}"
#                              :accent-color "#${base0C-hex}"
#                              :on-accent-color "#${base04-hex}"
#                              :primary-color "#${base05-hex}"
#                              :on-primary-color "#${base01-hex}"
#                              :secondary-color "#${base02-hex}"
#                              :on-secondary-color "#${base06-hex}"
#                              :font-family "Liga SFMono Nerd Font"))))
#   '';
