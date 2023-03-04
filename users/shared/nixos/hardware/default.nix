{
  pkgs,
  lib,
  inputs,
  config,
  ...
}: {
  imports = [
    ./audio.nix
    ./bluetooth.nix
    ./network.nix
  ];

  services.upower.enable = true;
  services.fstrim.enable = true;
  hardware.opengl.enable = true;
}
