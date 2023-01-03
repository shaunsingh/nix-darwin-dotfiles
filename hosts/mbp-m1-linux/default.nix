{ pkgs
, lib
, inputs
, ...
}: {
  imports = [
    # auto generated 
    ./hardware-configuration.nix

    # asahi
    "${inputs.nixos-m1}/nix/m1-support"

    # shared settings + overlays
    ../../modules/shared.nix

    # linux specific 
    ../../modules/linux.nix
  ];

  hardware.asahi.addEdgeKernelConfig = true;
  hardware.asahi.peripheralFirmwareDirectory = ./firmware;
  hardware.opengl.package = pkgs.asahi-mesa.drivers;

  system.stateVersion = "23.05";
}
