{ config, lib, pkgs, ... }:

let

  unstable = import (fetchTarball
    "https://github.com/NixOS/nixpkgs/archive/nixos-unstable.tar.gz") {
      overlays = [
        (import (builtins.fetchTarball {
          url =
            "https://github.com/nix-community/emacs-overlay/archive/master.tar.gz";
        }))
      ];
    };

  my-emacs = (pkgs.emacsPackagesGen unstable.emacsPgtkGcc).emacsWithPackages
    (epkgs: [ epkgs.vterm epkgs.pdf-tools ]);

in {

  # Allow me to install unfree packages
  nixpkgs.config.allowUnfree = true;

  environment.systemPackages = with pkgs; [

    # Dotfiles Management
    yadm
  
    # Utils
    wget
    git-lfs
    ripgrep
    exa
    bat
    tree
    fd
  
    # Terminal stuff
    alacritty
    tmux
  
    # Mail
    ## offlineimap
    ## mu
    msmtp
    isync
  
    # Browsers
    ## firefox
    ## nyxt
    ## qutebrowser
  
    # Emacs config deps (latex, aspell)
    (aspellWithDicts (dicts: with dicts; [ en en-computers en-science ]))
    (pkgs.texlive.combine { inherit (pkgs.texlive) scheme-small dvipng dvisvgm l3packages xcolor soul adjustbox collectbox amsmath siunitx cancel mathalpha capt-of chemfig wrapfig mhchem latexmk; })
    sdcv
    gnuplot
  
    # Editors
    neovim
    my-emacs
  
    # Chat
    ## discord
    discocss
  
    # Pdf/images
    ## zathura
  ];

  # Add ibm and overpass fonts for emacs
  fonts.fonts = with pkgs; [
    overpass
    alegreya
  ];
  
  # Use a custom configuration.nix location.
  # darwin-rebuild switch -I darwin-config=$HOME/.config/darwin/configuration.nix
  # environment.darwinConfig = "$HOME/.config/darwin/configuration.nix";

  # Auto upgrade nix package and the daemon service.
  # services.nix-daemon.enable = true;
  nix.package = pkgs.nixUnstable;

  # Create /etc/bashrc that loads the nix-darwin environment.
  # programs.zsh.enable = true;  # default shell on catalina
  programs.fish.enable = true;

  # bar
  # services.spacebar.enable = true;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
}

