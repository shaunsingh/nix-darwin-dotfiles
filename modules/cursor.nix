{ pkgs
, lib
, inputs
, config
, ...
}:
{
  home.pointerCursor = {
    name = "phinger-cursors";
    package = pkgs.phinger-cursors;
    size = 32;
  };
  gtk.gtk2.extraConfig = "gtk-cursor-theme-size=32";
  gtk.gtk3.extraConfig."gtk-cursor-theme-size" = 32;
  gtk.gtk4.extraConfig."gtk-cursor-theme-size" = 32;
  home.sessionVariables = {
    XCURSOR_SIZE = "32";
    # WLR_NO_HARDWARE_CURSORS = "1";
  };
}
