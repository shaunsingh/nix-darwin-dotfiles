{ pkgs
, lib
, inputs
, ...
}: {
  # kernel etc.
  enableRedistributableFirmware = true;
  imports = [
    ../hardware/m1/hardware-configuration.nix
    "${inputs.nixos-m1}/nix/m1-support"
  ];

  # boot
  boot.loader = {
    efi.canTouchEfiVariables = false;
    systemd-boot = {
      enable = true;
      # "error switching console mode" on boot.
      consoleMode = "auto";
      configurationLimit = 5;
    };
  };


  # give myself sudo rights
  users.users.shauryasingh = {
    isNormalUser = true;
    home = "/home/shauryasingh";
    extraGroups = [ "wheel" "networkmanager" ];
    shell = pkgs.fish;
  };

  # us locale
  services.timesyncd.enable = true;
  i18n = {
    defaultLocale = lib.mkDefault "en_US.UTF-8";

    extraLocaleSettings = {
      LC_TIME = lib.mkDefault "en_US.UTF-8";
    };

    supportedLocales = lib.mkDefault [
      "en_US.UTF-8/UTF-8"
      "ja_JP.UTF-8/UTF-8"
    ];
  };

  # console
  console =
    let
      normal = [ "181825" "F38BA8" "A6E3A1" "F9E2AF" "89B4FA" "F5C2E7" "94E2D5" "BAC2DE" ];
      bright = [ "1E1E2E" "F38BA8" "A6E3A1" "F9E2AF" "89B4FA" "F5C2E7" "94E2D5" "A6ADC8" ];
    in
    {
      colors = normal ++ bright;
      font = "${pkgs.terminus_font}/share/consolefonts/ter-u28n.psf.gz";
      keyMap = "us";
    };

  # env variables + essential packages
  environment = {
    variables = {
      EDITOR = "nvim";
      BROWSER = "nyxt";
      NIXOS_OZONE_WL = "1";
    };
    systemPackages = with pkgs; [
      curl
      llvmPackages_14.llvm
      uutils-coreutils
    ];
  };

  # disk
  services.fstrim.enable = true;

  # hardware-accelerated graphics
  hardware.asahi.useExperimentalGPUDriver = true;

  # bluetooh support
  hardware.bluetooth.enable = true;

  # sound
  services.pipewire = {
    enable = true;
    wireplumber.enable = true;
    pulse.enable = true;
    jack.enable = true;
    alsa = {
      enable = true;
      support32Bit = true;
    };
  };

  # network
  networking.networkmanager.enable = true;
  services.openssh.enable = true;
  systemd.services.NetworkManager-wait-online.enable = false;

  # systemd 
  systemd.user.services = {
    pipewire.wantedBy = [ "default.target" ];
    pipewire-pulse.wantedBy = [ "default.target" ];
  };

  # security
  security = {
    rtkit.enable = true;

    apparmor = {
      enable = true;
      killUnconfinedConfinables = true;
      packages = [ pkgs.apparmor-profiles ];
    };

    pam = {
      services.login.enableGnomeKeyring = true;

      loginLimits = [
        {
          domain = "@wheel";
          item = "nofile";
          type = "soft";
          value = "524288";
        }
        {
          domain = "@wheel";
          item = "nofile";
          type = "hard";
          value = "1048576";
        }
      ];
    };
  };

  # xdg directories
  xdg.portal = {
    enable = true;
    wlr.enable = true;
  };

  # font + fontconfig
  fonts = {
    fonts = with pkgs; [
      # noto fallbacks
      noto-fonts
      noto-fonts-cjk
      noto-fonts-emoji

      # apple fonts
      sf-mono-liga
      sf-pro
      ny
    ];
  };

  fontconfig = {
    enable = true;
    antialias = true;
    hinting = {
      enable = true;
      autohint = true;
      style = "hintfull";
    };

    subpixel.lcdfilter = "default";

    defaultFonts = {
      emoji = [ "Noto Color Emoji" ];
      monospace = [ "Liga SFMono Nerd Font" ];
      sansSerif = [ "SF Pro" "Noto Color Emoji" ];
      serif = [ "New York" "Noto Color Emoji" ];
    };
  };

  system.stateVersion = "23.05";
}
