{ ... }:

{
  imports = [
    ./modules/default.nix
    ./hardware-configuration.nix
  ];

  # default is impure, use local firmware
  hardware.asahi.peripheralFirmwareDirectory = ./firmware;

  # other asahi options
  hardware.asahi.withRust = true;
  hardware.asahi.useExperimentalGPUDriver = true;
  hardware.apple.touchBar = {
    enable = true;
    settings = {
      #MediaLayerDefault = true;
      EnablePixelShift = true;
      AdaptiveBrightness = false;
    };
  };
 
  # bluetooth
  hardware.bluetooth.enable = true;

  # adjust pipewire config for bluez only
  services.pipewire.wireplumber.extraConfig.bluetoothEnhancements = {
    "monitor.bluez.properties" = {
        "bluez5.enable-sbc-xq" = true;
        "bluez5.enable-msbc" = true;
        "bluez5.enable-hw-volume" = true;
        "bluez5.roles" = [ "hsp_hs" "hsp_ag" "hfp_hf" "hfp_ag" ];
    };
  };

  # memswap
  zramSwap = {
    enable = true;
    memoryPercent = 40;
  };

  # recommended for asahi
  boot = {
    loader = {
      # Use the systemd-boot EFI boot loader.
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = false;
    };
    # For ` to < and ~ to > (for those with US keyboards)
    extraModprobeConfig = ''
      options hid_apple iso_layout=0
    '';
  };
}
