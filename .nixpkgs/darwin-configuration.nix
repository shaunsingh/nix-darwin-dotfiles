{ config, lib, pkgs, ... }:

{

  # Preferences
  system.defaults.NSGlobalDomain.AppleKeyboardUIMode = 3;
  system.defaults.NSGlobalDomain.ApplePressAndHoldEnabled = false;
  system.defaults.NSGlobalDomain.InitialKeyRepeat = 10;
  system.defaults.NSGlobalDomain.KeyRepeat = 1;
  system.defaults.NSGlobalDomain.NSNavPanelExpandedStateForSaveMode = true;
  system.defaults.NSGlobalDomain.NSNavPanelExpandedStateForSaveMode2 = true;
  system.defaults.NSGlobalDomain._HIHideMenuBar = true;

  system.defaults.dock.autohide = true;
  system.defaults.dock.mru-spaces = false;
  system.defaults.dock.showhidden = true;

  system.defaults.finder.AppleShowAllExtensions = true;
  system.defaults.finder.QuitMenuItem = true;
  system.defaults.finder.FXEnableExtensionChangeWarning = false;

  system.defaults.trackpad.Clicking = true;
  system.keyboard.enableKeyMapping = true;
  system.keyboard.remapCapsLockToControl = true;

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
    ## alacritty
    ## tmux
  
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
    (pkgs.texlive.combine { inherit (pkgs.texlive) scheme-small dvipng dvisvgm l3packages xcolor soul adjustbox collectbox amsmath siunitx cancel mathalpha capt-of chemfig wrapfig mhchem fvextra latexmk; })
    sdcv
    gnuplot
  
    # Editors
    ## neovim
    neovim-nightly
    ## emacs
    emacsPgtkGcc
  
    # Chat
    ## discord
    discocss
  
    # Pdf/images
    ## zathura
  ];

  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url = "https://github.com/nix-community/neovim-nightly-overlay/archive/master.tar.gz";

    }))
    (import (builtins.fetchTarball {
      url = "https://github.com/nix-community/emacs-overlay/archive/master.tar.gz";
    }))
  ];


  # Add ibm and overpass fonts for emacs
  fonts.fonts = with pkgs; [
    overpass
    alegreya
  ];
  
  # Auto upgrade nix package and the daemon service.
  nix.package = pkgs.nixUnstable;

  # Use Auto GC
  nix.gc.automatic = true;
  nix.gc.options = "--max-freed $((25 * 1024**3 - 1024 * $(df -P -k /nix/store | tail -n 1 | awk '{ print $4 }')))";
  nix.gc.user = "shauryasingh";

   # Use the nix sandbox
  nix.useSandbox = true;
  nix.sandboxPaths = [ "/private/tmp" "/private/var/tmp" "/usr/bin/env" ];

  # Use Fish and Tmux as my user shell 
  programs.fish.enable = true;
  programs.tmux.enable = true;

  # Set loginShell
  environment.loginShell = "${pkgs.zsh}/bin/zsh -l";
  environment.variables.LANG = "en_US.UTF-8";

  # Use yabai as my wm
  # services.yabai.enable = true;
  # services.yabai.package = pkgs.yabai;
  # services.skhd.enable = true;

  # bar
  # services.spacebar.enable = true;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
}

