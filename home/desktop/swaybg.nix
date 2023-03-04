{
  pkgs,
  lib,
  inputs,
  config,
  ...
}: {
  imports = [../../modules/home-manager/programs/swaybg.nix];
  programs.swaybg = {
    enable = true;
    image = ../../configs/wallpapers/cell_od11.png;
  };
}
