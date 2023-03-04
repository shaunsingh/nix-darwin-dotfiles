{
  pkgs,
  lib,
  inputs,
  ...
}: {
  imports = [
    # auto generated
    ./hardware-configuration.nix
  ];

  # enable auto-resize of guest display when vm window resizes
  services.spice-vdagentd.enable = true;

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "shaunsingh-nixos-utm";
  networking.useDHCP = false;
  networking.interfaces.enp0s6.useDHCP = true;
}
