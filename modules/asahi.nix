{ pkgs
, lib
, inputs
, config
, ...
}:
let
  bootFiles = {
    "m1n1/boot.bin" = pkgs.runCommand "boot.bin" { } ''
      cat ${pkgs.m1n1}/build/m1n1.bin > $out
      cat ${config.boot.kernelPackages.kernel}/dtbs/apple/*.dtb >> $out
      cat ${pkgs.asahi-u-boot}/u-boot-nodtb.bin.gz >> $out
    '';
  };
in
{
  # use the asahi kernel 
  boot.kernelPackages = pkgs.asahi-edge;
  powerManagement.cpuFreqGovernor = lib.mkOverride 800 "schedutil";

  # enable opengl acceleration and wifi with new drivers and peripheral firmware
  hardware.opengl.package = pkgs.asahi-mesa.drivers;
  hardware.firmware = [ pkgs.asahi-peripheral-firmware ];

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

  # U-Boot does not support EFI variables
  # U-Boot does not support switching console mode
  boot.loader = {
    efi.canTouchEfiVariables = false;
    systemd-boot = {
      enable = true;
      consoleMode = "0";
      extraFiles = bootFiles;
    };
  };

  # expose m1n1 and u-boot to the system
  system.extraDependencies = [ pkgs.m1n1 pkgs.asahi-u-boot ];
  system.build.m1n1 = bootFiles."m1n1/boot.bin";
}
