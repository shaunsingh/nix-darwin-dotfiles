# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan, use cachix
      ./hardware-configuration.nix
      ./cachix.nix
    ];

  # allow me to install unfree packages 
  nixpkgs.config.allowUnfree = true;

  # use network manager
  networking.networkmanager.enable = true;

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # set the hostname
  networking.hostName = "nixos"; # Define your hostname.

  # Set your time zone.
  # time.timeZone = "Europe/Amsterdam";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Enable printing via cups
  services.printing.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.shauryasingh = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkManager" ]; # Enable ‘sudo’ for the user.
  };

  # List packages installed in system profile. To search, run:
  environment.systemPackages = with pkgs; [

    # dotfiles management
    yadm 

    # utils
    wget
    git
    gcc
    ripgrep 
    exa 
    bat
    fd

    # terminal stuff
    starship
    alacritty
    tmux
    neofetch 

    # browser
    firefox

    # editors
    neovim-nightly
    emacsPgtkGcc

    # emacs config deps (latex, aspell)
    (aspellWithDicts (dicts: with dicts; [ en en-computers en-science ]))
    (pkgs.texlive.combine { inherit (pkgs.texlive) scheme-small dvipng dvisvgm l3packages xcolor soul adjustbox collectbox amsmath siunitx cancel mathalpha capt-of chemfig wrapfig mhchem latexmk; })

  ];

  # overlay for emacsGcc and Neovim-Nightly
  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
    }))
    (import (builtins.fetchTarball {
      url = https://github.com/nix-community/neovim-nightly-overlay/archive/master.tar.gz;
    }))
  ];

  # use sway :chad:
  programs.sway = {
   enable = true;
   extraPackages = with pkgs; [
     waybar
     dmenu
     swaylock
     swayidle
     mako 
     grim
     slurp
     wl-clipboard
     wf-recorder
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

  # Set default shell to sway
  users.defaultUserShell = pkgs.fish;

  system.stateVersion = "21.05"; # Did you read the comment?
}

