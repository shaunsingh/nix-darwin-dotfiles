{ config, pkgs, lib, ... }:
{
  boot.kernelPackages = pkgs.callPackage ./package.nix {
    inherit (config.boot) kernelPatches;
    withRust = true;
  };

  # we definitely want to use CONFIG_ENERGY_MODEL, and
  # schedutil is a prerequisite for using it
  # source: https://www.kernel.org/doc/html/latest/scheduler/sched-energy.html
  powerManagement.cpuFreqGovernor = lib.mkOverride 800 "schedutil";

  boot.initrd.includeDefaultModules = false;
  boot.initrd.availableKernelModules = [
    # list of initrd modules stolen from asahi project
    "apple-mailbox"
    "nvme_apple"
    "pinctrl-apple-gpio"
    "macsmc"
    "macsmc-rtkit"
    "i2c-apple"
    "tps6598x"
    "apple-dart"
    "dwc3"
    "dwc3-of-simple"
    "xhci-pci"
    "pcie-apple"
    "gpio_macsmc"
    "phy-apple-atc"
    "nvmem_apple_efuses"
    "spi-apple"
    "spi-hid-apple"
    "spi-hid-apple-of"
    "rtc-macsmc"
    "simple-mfd-spmi"
    "spmi-apple-controller"
    "nvmem_spmi_mfd"
    "apple-dockchannel"
    "dockchannel-hid"
    "apple-rtkit-helper"
  ];

  boot.kernelParams = [
    "earlycon"
    "console=ttySAC0,1500000"
    "console=tty0"
    "boot.shell_on_fail"
    "nvme_apple.flush_interval=0"
  ];

  boot.kernelPatches = [
    {
      name = "edge-config";
      patch = null;
      # derived from
      # https://github.com/AsahiLinux/PKGBUILDs/blob/stable/linux-asahi/config.edge
      extraConfig = ''
        DRM_SIMPLEDRM_BACKLIGHT=n
        BACKLIGHT_GPIO=n
        DRM_APPLE=m
        APPLE_SMC=m
        APPLE_SMC_RTKIT=m
        APPLE_RTKIT=m
        APPLE_MAILBOX=m
        GPIO_MACSMC=m
        LOCALVERSION="-edge-ARCH"
        DRM_VGEM=n
        DRM_SCHED=y
        DRM_GEM_SHMEM_HELPER=y
        DRM_ASAHI=m
        SUSPEND=y
      '';
    }
  ];

  # U-Boot does not support EFI variables
  # U-Boot does not support switching console mode
  boot.loader = {
    efi.canTouchEfiVariables = false;
    systemd-boot = {
      enable = true;
      consoleMode = "0";
    };
  };
}
