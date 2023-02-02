{ pkgs
, lib
, inputs
, ...
}: {
  imports = [
    # auto generated 
    ./hardware-configuration.nix

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
  hardware.asahi.experimentalGPUInstallMode = "driver";
  hardware.opengl.enable = true;

#   boot.kernelPatches = [
#       {
#         name = "edge-config";
#         patch = null;
#         # derived from
#         # https://github.com/AsahiLinux/PKGBUILDs/blob/stable/linux-asahi/config.edge
#         extraConfig = ''
#           DRM_SIMPLEDRM_BACKLIGHT n
#           BACKLIGHT_GPIO n
#           DRM_APPLE m
#           APPLE_SMC m
#           APPLE_SMC_RTKIT m
#           APPLE_RTKIT m
#           APPLE_MAILBOX m
#           GPIO_MACSMC m
#           DRM_VGEM n
#           DRM_SCHED y
#           DRM_GEM_SHMEM_HELPER y
#           DRM_ASAHI m
#           SUSPEND y
#         '';
#       }
#       {
#         name = "blocklist-type";
#         patch = builtins.fetchurl "https://patch-diff.githubusercontent.com/raw/AsahiLinux/linux/pull/109.diff";
#       }
#     ];

  # use sway by default
  services = {
    logind.lidSwitch = "ignore";
    greetd = {
      enable = true;
      package = pkgs.greetd.tuigreet;
      settings = {
        default_session.command = "${pkgs.greetd.tuigreet}/bin/tuigreet --cmd sway";
      };
    };
  };

  system.stateVersion = "23.05";
}
