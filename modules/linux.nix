{ config, options, pkgs, lib, ... }: {

  # Use nix-unstable
  nix = {
    package = pkgs.nixUnstable;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.shauryasingh = {
    isNormalUser = true;
    extraGroups = [
      "wheel"
      "networkManager"
    ]; # Enable ‘sudo’ for the user + nwcli access
    shell = pkgs.fish;
  };

  # kernel and hardware management
  hardware.enableRedistributableFirmware = true; # fsf :ha:
  boot.kernelPackages =
    pkgs.linuxPackages_latest; # why is default linux so old

  # Bootloader
  boot.loader = {
    efi.canTouchEfiVariables = true;
    systemd-boot.enable = true;
  };

  # Set your time zone.
  time.timeZone = "America/New_York";
  time.hardwareClockInLocalTime =
    true; # lets not mess up my windows clock too

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  # Sound
  sound.enable = false;
  hardware.pulseaudio.enable = false;

  # Enable pipewire and emulate alsa/pulse/jack
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    jack.enable = true;
  };

  # Set up XDG for screen-sharing
  xdg = {
    portal = {
      enable = true;
      gtkUsePortal = true;
      extraPortals = with pkgs; [
        xdg-desktop-portal-wlr
        xdg-desktop-portal-gtk
      ];
    };
  };

  # Install fonts
  fonts = {
    fonts = with pkgs; [
      ibm-plex
      emacs-all-the-icons-fonts
      sf-mono-liga-bin
    ];
  };
}
