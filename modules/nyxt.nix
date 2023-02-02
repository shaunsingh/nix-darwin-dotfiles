# TODO actually build the gtk app
{ pkgs
, lib
, inputs
, config
, ...
}: {
  home.packages = with pkgs; [
    # nyxt-3
  ];
  xdg.configFile."nyxt" = {
    source = ../configs/nyxt;
    recursive = true;
  };
  xdg.dataFile."nyxt/extensions" = {
    source = ../configs/nyxt/extensions;
    recursive = true;
  };
}
