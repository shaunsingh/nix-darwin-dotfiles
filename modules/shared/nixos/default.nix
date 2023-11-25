{ config, lib, pkgs, ... }:

/*
  NixOS configuration

  Useful links:
  - Package Search: https://search.nixos.org/packages?channel=unstable
  - Options Search: https://search.nixos.org/options?channel=unstable
*/
{
  boot = {
    consoleLogLevel = 0;

    kernelPackages = pkgs.linuxKernel.packages.linux_lqx;

    kernelParams = [
      "preempt=full"
      "mitigations=off"
      "quiet"
      "udev.log_level=3"
    ];
      
    loader = {
      efi.canTouchEfiVariables = true;

      systemd-boot = {
        enable = true;
        configurationLimit = 3;
        consoleMode = "max";
        editor = false;
      };
    };
  };

  console =
    let
      normal = [ "161616" "33b1ff" "ff7eb6" "42be65" "08bdba" "82cfff" "78a9ff" "dde1e6" ];
      bright = [ "525252" "33b1ff" "ff7eb6" "42be65" "08bdba" "82cfff" "78a9ff" "ffffff" ];
    in
    {
      colors = normal ++ bright;
      keyMap = "us";
    };

  documentation.man =
    let
      activeManOutputs = [ "man" ] ++ lib.optionals config.documentation.dev.enable [ "devman" ];
    in
    {
      generateCaches = true;

      man-db.manualPages = pkgs.buildEnv {
        name = "man-paths";
        paths = config.environment.systemPackages;
        pathsToLink = [ "/share/man" ];
        extraOutputsToInstall = activeManOutputs;
        ignoreCollisions = true;
      };
    };

  environment = {
    /*
      NOTE: This isn't found in https://search.nixos.org/options.

      Here's the warning that came with it:

      "Please note that NixOS assumes all over the place that shell to be Bash,
      so override the default setting only if you know exactly what you're doing."
    */
    binsh = "${pkgs.zsh}/bin/zsh";
    pathsToLink = [ "/share/zsh" ];
    shells = with pkgs; [ zsh ];
  };

  fonts = {
    packages = builtins.attrValues {
      inherit (pkgs)
        twemoji-color-font
        sarasa-gothic
        sf-mono-liga-bin
        otf-apple;
    };

    fontconfig = {
      enable = lib.mkDefault true;
      antialias = true;
      subpixel.lcdfilter = "default";
      defaultFonts = {
        monospace = [ "Liga SFMono Nerd Font" ];
        sansSerif = [ "SF Pro Text" ];
        serif = [ "New York Medium" ];
        emoji = [ "Twitter Color Emoji" ];
      };
    };
  };

  i18n = {
    defaultLocale = "en_US.UTF-8";
    supportedLocales = [ "en_US.UTF-8/UTF-8" ];
  };

  powerManagement.cpuFreqGovernor = "performance";

  programs = {
    bash.interactiveShellInit = ''export HISTFILE=$HOME/.config/.bash_history'';
    command-not-found.enable = false;
    dconf.enable = true;
    ssh.pubkeyAcceptedKeyTypes = [ "ssh-ed25519" ];
    fish.enable = true;
  };

  qt.platformTheme = "qt5ct";

  security = {
    sudo.wheelNeedsPassword = false;
    polkit.enable = true;
  };

  services = {
    journald.extraConfig = lib.mkForce "";

    openssh = {
      enable = true;
      settings = {
        GatewayPorts = "yes";
        PermitRootLogin = "yes";
      };
    };

    pipewire = {
      enable = true;
      socketActivation = false;
      jack.enable = true;
      pulse.enable = true;
      alsa = {
        enable = true;
        support32Bit = true;
      };
    };
  };

  systemd = {
    services.rtkit-daemon.serviceConfig.ExecStart = [
      ""
      "${pkgs.rtkit}/libexec/rtkit-daemon --our-realtime-priority=95 --max-realtime-priority=90"
    ];

    user.services = {
      pipewire.wantedBy = [ "default.target" ];
      pipewire-pulse.wantedBy = [ "default.target" ];
    };
  };

  time = {
    hardwareClockInLocalTime = true;
    timeZone = "America/New_York";
  };

  users.users.shaurizard = {
    isNormalUser = true;
    home = "/home/shaurizard";
    shell = pkgs.fish;

    extraGroups = [
      "wheel"
      "video"
      "audio"
      "realtime"
    ];
  };

  zramSwap = {
    enable = true;
    memoryPercent = 40;
  };
}
