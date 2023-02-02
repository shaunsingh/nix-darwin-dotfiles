{ pkgs
, lib
, inputs
, config
, ...
}: {
  gtk = {
    enable = true;
    font.name = "Liga SFMono Nerd Font";
    iconTheme = {
      package = pkgs.papirus-icon-theme;
       name = "Papirus-Dark";
    };
    theme = {
      name = "phocus";
      package = pkgs.phocus-oxocarbon; 
    };
  };
  # home.sessionVariables = {
  #   GTK_CSD = "0";
  #   LD_PRELOAD = "${pkgs.nur.repos.dukzcry.gtk3-nocsd}/lib/libgtk3-nocsd.so.0";
  # };
}
