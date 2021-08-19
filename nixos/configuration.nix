# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, emacsNg-src, ... }:

{

  nix = {
    autoOptimiseStore = true;
    package = pkgs.nixFlakes;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  # allow me to install unfree packages and firmware
  nixpkgs.config.allowUnfree = true;
  hardware.enableRedistributableFirmware = true;

  # Network settings.
  networking = {
    hostName = "shaunsingh-laptop"; # Hostname
    useDHCP = false; # Deprecated, so set explicitly to false
    wireless.enable = false;
    networkmanager.enable = true; # Enable networkmanager
  };

   # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Set your time zone.
  # time.timeZone = "Europe/Amsterdam";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  # Sound
  sound.enable = false;
  hardware.pulseaudio.enable = false; # Pulseaudio

  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    jack.enable = true;
  };

  # Enable printing via cups
  services.printing.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.shauryasingh = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkManager" ]; # Enable ‘sudo’ for the user.
    shell = pkgs.fish;
  };

  # Packages
  environment.systemPackages = with pkgs; [

    # Dotfiles Management
    yadm

    # Utils
    wget
    git
    gcc
    ripgrep
    exa
    bat
    fd

    # forgive me stallman
    firmwareLinuxNonfree

    # Terminal stuff
    ## neofetch
    ## starship
    alacritty
    tmux

    # Mail
    ## offlineimap
    mu
    msmtp
    isync

    # Browsers
    ## firefox
    ## nyxt
    qutebrowser

    # Editors
    ## neovim-nightly
    emacsPgtkGcc
    ## neovim
    ## emacsNg-src.defaultPackage.x86_64-linux

    # Emacs config deps (latex, aspell)
    ## (pkgs.texlive.combine { inherit (pkgs.texlive) scheme-small dvipng dvisvgm l3packages xcolor soul adjustbox collectbox amsmath siunitx cancel mathalpha capt-of chemfig wrapfig mhchem latexmk; })
    (aspellWithDicts (dicts: with dicts; [ en en-computers en-science ]))
    tectonic

  ];

  
  let
    unstable-pkgs = import <unstable> {};
  in
  {
    environment.systemPackages = [ unstable-pkgs.neovim ]
  }
  
  # use sway :chad:
  programs.sway = {
   enable = true;
   extraPackages = with pkgs; [
     # bar
     waybar

     # launcher
     ## dmenu
     ## bemenu
     ## wofi

     # gtk
     ## pop-icon-theme
     ## nordic

     # utils
     grim
     slurp

     # clipboard and recording
     wl-clipboard
     wf-recorder

     # Control
     brightnessctl
     playerctl

   ];
   extraSessionCommands = ''
     export SDL_VIDEODRIVER=wayland
     export QT_QPA_PLATFORM=wayland
     export QT_WAYLAND_DISABLE_WINDOWDECORATION="1"
     export _JAVA_AWT_WM_NONREPARENTING=1
     export MOZ_ENABLE_WAYLAND=1
   '';
  };

  # setup systemd etc
  systemd.user.targets.sway-session = {
    description = "Sway compositor session";
    documentation = ["man:systemd.special(7)" ];
    bindsTo = [ "graphical-session.target" ];
    wants = ["graphical-session-pre.target" ];
    after = [ "graphical-session-pre.target" ];
  };

  # battery life stuff
  services.tlp.enable = true;

  # Use fish, launch sway after login
  programs.fish = {
    enable = true;
    loginShellInit = ''
      if test (id --user $USER) -ge 1000 && test (tty) = "/dev/tty1"
        exec sway
      end
    '';
  };

  system.stateVersion = "21.11"; # Did you read the comment?
}
