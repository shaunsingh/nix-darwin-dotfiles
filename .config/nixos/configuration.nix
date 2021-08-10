# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ./cachix.nix
    ];

  nixpkgs.config.allowUnfree = true;
  networking.networkmanager.enable = true;

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

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

  services.printing.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.shauryasingh = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkManager" ]; # Enable ‘sudo’ for the user.
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    wget
    git
    gcc
    fish
    starship
    alacritty
    tmux
    neofetch 
    yadm 
    aspell
    (aspellWithDicts (dicts: with dicts; [ en en-computers en-science ]))
    ripgrep 
    firefox
    neovim-nightly
    emacsPgtkGcc
    exa 
    bat
   (pkgs.texlive.combine { inherit (pkgs.texlive) scheme-small dvipng dvisvgm l3packages xcolor soul adjustbox collectbox amsmath siunitx cancel mathalpha capt-of chemfig wrapfig mhchem latexmk; })
  ];

  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
    }))
    (import (builtins.fetchTarball {
      url = https://github.com/nix-community/neovim-nightly-overlay/archive/master.tar.gz;
    }))
  ];

  programs.sway = {
   enable = true;
   extraPackages = with pkgs; [
     waybar
     dmenu
     swaylock
     swayidle
     xwayland
     mako 
     kanshi
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

  systemd.user.targets.sway-session = {
    description = "Sway compositor session";
    documentation = ["man:systemd.special(7)" ];
    bindsTo = [ "graphical-session.target" ];
    wants = ["graphical-session-pre.target" ];
    after = [ "graphical-session-pre.target" ];
  };

  systemd.user.services.kanshi = {
    description = "Kanshi output autoconfig";
    wantedBy = [ "graphical-session.target" ];
    partOf = [ "graphical-session.target" ];
    environment = { XDG_CONFIG_HOME="/home/shauryasingh/.config"; };
    serviceConfig = {
      ExecStart = ''
      ${pkgs.kanshi}/bin/kanshi
      '';
      RestartSec = 5;
      Restart = "always";
    };
  };
 
  services.xserver.enable = true;
  services.xserver.displayManager.defaultSession = "sway";
  services.xserver.displayManager.sddm.enable = true;
  services.xserver.libinput.enable = true; 
  services.tlp.enable = true;

  programs.fish.enable = true;

  users.extraUsers.shauryasingh = {
    shell = pkgs.fish;
  };

  system.stateVersion = "21.05"; # Did you read the comment?
}

