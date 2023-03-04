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

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.useDHCP = false;
  networking.interfaces.ens33.useDHCP = true;

  virtualisation.vmware.guest.enable = true;

  networking.hostName = "shaunsingh-nixos-virtualboy";
}
