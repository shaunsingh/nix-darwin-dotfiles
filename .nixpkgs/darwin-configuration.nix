{ config, lib, pkgs, ... }:

let

  username = "shauryasingh";
  home = "/Users/${username}";

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
    (epkgs: [ epkgs.emacsql-sqlite epkgs.vterm epkgs.pdf-tools ]);

in {

  # Preferences

  ## Expand Save by default 
  system.defaults.NSGlobalDomain.NSNavPanelExpandedStateForSaveMode = true;
  system.defaults.NSGlobalDomain.NSNavPanelExpandedStateForSaveMode2 = true;

  ## Use Subpixel AA on external monitors
  system.defaults.NSGlobalDomain.AppleFontSmoothing = 1;

  ## Hide menubar by default 
  system.defaults.NSGlobalDomain._HIHideMenuBar = true;

  ## Use Dark theme
  system.defaults.NSGlobalDomain.AppleInterfaceStyle = "Dark";

  ## Save screenshots to /tmp
  system.defaults.screencapture.location = "/tmp";

  ## Autohide dock and disable mru
  system.defaults.dock.autohide = true;
  system.defaults.dock.mru-spaces = false;
  system.defaults.dock.showhidden = true;

  ## Show all file extensions in finder, and disable the warning for it
  system.defaults.finder.AppleShowAllExtensions = true;
  system.defaults.finder.QuitMenuItem = true;
  system.defaults.finder.FXEnableExtensionChangeWarning = false;

  ## Disable natural scrolling, tap to click
  system.defaults.NSGlobalDomain."com.apple.mouse.tapBehavior" = 1;
  system.defaults.NSGlobalDomain."com.apple.swipescrolldirection" = false;

  ## Bind Caps lock to CTRL 
  system.keyboard.enableKeyMapping = true;
  system.keyboard.remapCapsLockToControl = true;
  system.defaults.NSGlobalDomain.AppleKeyboardUIMode = 3;
  system.defaults.NSGlobalDomain.ApplePressAndHoldEnabled = false;
  system.defaults.NSGlobalDomain.InitialKeyRepeat = 10;
  system.defaults.NSGlobalDomain.KeyRepeat = 1;

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
    tmux
  
    # Mail
    ## offlineimap
    mu
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
    my-emacs

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

  # Use Fish
  programs.fish.enable = true;
  environment.shells = with pkgs; [ fish ];
  users.users."${username}" = {
    inherit home;
    shell = pkgs.fish;
  };
  system.activationScripts.postActivation.text = ''
    # Set the default shell as fish for the user
    sudo chsh -s ${lib.getBin pkgs.fish}/bin/fish ${username}

    # Setup neovim
    nvim --headless +PackerSync +qa

    # Install emacs
    emacs --batch --eval "(progn (require 'org) (setq org-confirm-babel-evaluate nil) (org-babel-tangle-file \"~/.config/doom/config.org\"))"
    git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d
    ~/.emacs.d/bin/doom -y install

    # Sync mu4e
    mkdir ~/.mbsync
    nvim .mbsyncrc
    nvim .msmtprc
    mu init --maildir=~/.mbsync --my-address=shaunsingh0207@gmail.com
    mu index
    mbsync --all
  '';

  # Touchid for sudo authentication
  imports = [ ~/nixos/darwin/pam.nix ];
  security.pam.enableSudoTouchIdAuth = true;

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

