# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, emacsNg-src, eww, ... }:

{

  nix = {
    autoOptimiseStore = true;
    package = pkgs.nixFlakes;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  # Use the unstable branch
  ## system.autoUpgrade.channel = https://nixos.org/channels/nixos-unstable;

  # Always update nixOS
  ## system.autoUpgrade.enable = true;
  ## system.autoUpgrade.allowReboot = true;

  # Allow me to install unfree packages
  nixpkgs.config.allowUnfree = true;

  # Use the Zen Kernel
  boot.kernelPackages = pkgs.linuxPackages_zen;

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
  time.timeZone = "America/New_York";
  ## services.localtime.enable = true;

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
  # services.printing.enable = true;

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

    # Rust coreutils
    uutils-coreutils

    # Utils
    wget
    git
    gcc
    ripgrep
    exa
    bat
    fd

    # Terminal stuff
    ## neofetch
    ## starship
    ## alacritty
    alacritty-ligatures
    tmux
    tty-clock
    htop

    # Mail
    ## offlineimap
    mu
    msmtp
    isync

    # Browsers
    ## firefox
    ## nyxt
    qutebrowser

    # Emacs config deps (latex, aspell)
    ## (pkgs.texlive.combine { inherit (pkgs.texlive) scheme-small dvipng dvisvgm l3packages xcolor soul adjustbox collectbox amsmath siunitx cancel mathalpha capt-of chemfig wrapfig mhchem latexmk; })
    (aspellWithDicts (dicts: with dicts; [ en en-computers en-science ]))
    tectonic

    # Editors
    ## neovim-nightly
    ## emacsPgtkGcc
    neovim
    emacsNg-src.defaultPackage.x86_64-linux

  ];

  # use sway :chad:
  programs.sway = {
   enable = true;
   extraPackages = with pkgs; [
     # bar
     waybar

     # launcher
     ## dmenu
     ## bemenu
     wofi

     # gtk
     ## papirus-icon-theme
     pop-icon-theme
     ## nordic

     # utils
     grim
     slurp
     mako
     ## eww.defaultPackage.x86_64-linux

     # locking
     ## swaylock
     swaylock-effects
     swayidle

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
  ## services.tlp.enable = true;
  services.power-profiles-daemon.enable = true;
  # Scale cpu
  services.auto-cpufreq.enable = true;
  # Macbook (non-pro), I mean it should work right?
  services.mbpfan.enable = true;
  # Intel power daemon
  services.thermald.enable = true;

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
