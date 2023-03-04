{
  pkgs,
  lib,
  inputs,
  config,
  isWayland,
  ...
}: {
  environment = {
    sessionVariables = {
      # These are the defaults, and xdg.enable does set them, but due to load
      # order, they're not set before environment.variables are set, which could
      # cause race conditions.
      XDG_CACHE_HOME = "$HOME/.cache";
      XDG_CONFIG_HOME = "$HOME/.config";
      XDG_DATA_HOME = "$HOME/.local/share";
      XDG_BIN_HOME = "$HOME/.local/bin";
    };
  };
  xdg.portal = {
    enable = true;
    wlr.enable = true;
    extraPortals =
      [
        pkgs.xdg-desktop-portal-gtk
      ]
      ++ lib.optionals isWayland [
        pkgs.xdg-desktop-portal-wlr
      ];
  };
}
