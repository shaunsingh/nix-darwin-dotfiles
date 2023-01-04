{ pkgs
, ...
}: {
  home.packages = with pkgs; [
    nyxt
  ];
  xdg.configFile."nyxt" = {
    source = ../configs/nyxt;
    recursive = true;
  };
}
