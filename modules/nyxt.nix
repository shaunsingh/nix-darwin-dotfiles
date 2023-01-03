{ pkgs
, ...
}: {
  home.packages = with pkgs; [
    # nyxt-3
  ];
  xdg.configFile."nyxt" = {
    source = ../configs/nyxt;
    recursive = true;
  };
}
