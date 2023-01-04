{ pkgs
, ...
}: {
  home.packages = with pkgs; [
    nyxt-gtk
  ];
  xdg.configFile."nyxt" = {
    source = ../configs/nyxt;
    recursive = true;
  };
}
