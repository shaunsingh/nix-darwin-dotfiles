{
  inputs,
  lib,
  config,
  pkgs,
  ...
}: {
  xdg.configFile."nyxt" = {
    source = ../config/nyxt;
    recursive = true;
  };
}
