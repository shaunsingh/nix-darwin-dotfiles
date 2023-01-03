{ config, pkgs, lib, ... }:
{
  boot.kernelPackages = pkgs.callPackage ./package.nix { withRust = true; };

  # we definitely want to use CONFIG_ENERGY_MODEL, and
  # schedutil is a prerequisite for using it
  # source: https://www.kernel.org/doc/html/latest/scheduler/sched-energy.html
  powerManagement.cpuFreqGovernor = 800 "schedutil";

  boot.initrd.includeDefaultModules = false;
  boot.initrd.availableKernelModules = [
    # list of initrd modules stolen from
    # https://github.com/AsahiLinux/asahi-scripts/blob/f461f080a1d2575ae4b82879b5624360db3cff8c/initcpio/install/asahi
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

    # additional stuff necessary to boot off USB for the installer
    # and if the initrd (i.e. stage 1) goes wrong
    "usb-storage"
    "xhci-plat-hcd"
    "usbhid"
    "hid_generic"
  ];

  boot.kernelParams = [
    "earlycon"
    "console=ttySAC0,1500000"
    "console=tty0"
    "boot.shell_on_fail"
    # Apple's SSDs are slow (~dozens of ms) at processing flush requests which
    # slows down programs that make a lot of fsync calls. This parameter sets
    # a delay in ms before actually flushing so that such requests can be
    # coalesced. Be warned that increasing this parameter above zero (default
    # is 1000) has the potential, though admittedly unlikely, risk of
    # UNBOUNDED data corruption in case of power loss!!!! Don't even think
    # about it on desktops!!
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
