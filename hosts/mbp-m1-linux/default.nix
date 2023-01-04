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

  # new kernel
  hardware.asahi.addEdgeKernelConfig = true;

  # apple firmware
  hardware.asahi.peripheralFirmwareDirectory = ./firmware;

  # enable graphics acceleration 
  hardware.asahi.useExperimentalGPUDriver = true;
  hardware.opengl.enable = true;

  system.stateVersion = "23.05";
}
