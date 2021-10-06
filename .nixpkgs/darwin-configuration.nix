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
  '';

  # Touchid for sudo authentication
  imports = [ ~/nixos/darwin/pam.nix ];
  security.pam.enableSudoTouchIdAuth = true;

  # Tmux
  programs.tmux.enable = true;
  programs.tmux.enableVim = true;
  programs.tmux.extraConfig = ''
   # make sure fish works in tmux
   set -g  default-terminal   "xterm-256color"
   set -sa terminal-overrides ',xterm-256color:RGB'

   # so that escapes register immidiately in vim
   set -sg escape-time 0

   # mouse support
   set -g mouse on

   # change prefix to C-a
   set -g prefix C-a
   unbind C-b
   bind C-a send-prefix

   # extend scrollback
   set-option -g history-limit 5000

   # vim-like pane resizing
   bind -r C-k resize-pane -U
   bind -r C-j resize-pane -D
   bind -r C-h resize-pane -L
   bind -r C-l resize-pane -R

   # vim-like pane switching
   bind -r k select-pane -U
   bind -r j select-pane -D
   bind -r h select-pane -L
   bind -r l select-pane -R

   # and now unbind keys
   unbind Up
   unbind Down
   unbind Left
   unbind Right

   unbind C-Up
   unbind C-Down
   unbind C-Left

   # styling
   set -g status-bg default
   set -g status-fg white
   set -g status-style fg=white,bg=default

   set -g status-left ""
   set -g status-right ""
   set -g status-justify centre
   set -g status-position bottom

   set -g pane-active-border-style bg=default,fg=default
   set -g pane-border-style fg=default

   set -g window-status-current-format "#[fg=cyan]#[fg=black]#[bg=cyan]#I #[bg=brightblack]#[fg=white] #W#[fg=brightblack]#[bg=default] #[bg=default] #[fg=magenta]#[fg=black]#[bg=magenta]λ #[fg=white]#[bg=brightblack] %a %d %b #[fg=magenta]%R#[fg=brightblack]#[bg=default]"
   set -g window-status-format "#[fg=magenta]#[fg=black]#[bg=magenta]#I #[bg=brightblack]#[fg=white] #W#[fg=brightblack]#[bg=default] "
  '';

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

