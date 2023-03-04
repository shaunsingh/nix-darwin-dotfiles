{
  pkgs,
  lib,
  inputs,
  ...
}: {
  imports = [
    # auto generated
    ./hardware-configuration.nix
    inputs.nixos-apple-silicon.nixosModules.apple-silicon-support
  ];

  # overlay
  nixpkgs.overlays = [inputs.nixos-apple-silicon.overlays.apple-silicon-overlay];

  # new kernel
  hardware.asahi.addEdgeKernelConfig = true;

  # apple firmware
  hardware.asahi.peripheralFirmwareDirectory = ./firmware;

  # enable graphics acceleration
  hardware.asahi.useExperimentalGPUDriver = true;
  hardware.asahi.experimentalGPUInstallMode = "driver";

  environment.variables = {
    MESA_GL_VERSION_OVERRIDE = "3.3";
    MESA_GLES_VERSION_OVERRIDE = "3.1";
    MESA_GLSL_VERSION_OVERRIDE = "330";
  };

  # fix headphones hack
  environment.systemPackages = [pkgs.asahi-alsa-utils];
  systemd.user.services.fix-asahi-jack = {
    script = ''
      ${pkgs.asahi-alsa-utils}/bin/amixer -c 0 set 'Jack Mixer' 100%
    '';
    wantedBy = ["default.target"];
  };

  # use systemd-boot
  boot.loader.systemd-boot.enable = true;

  networking.hostName = "shaunsingh-nixos-asahi";
  system.stateVersion = "23.05";
}
