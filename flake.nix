{
  description = "Shaurya's Nix Environment";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    mk-darwin-system.url = "github:vic/mk-darwin-system/main";

    neovim.url = "github:neovim/neovim?dir=contrib";
    neovim.inputs.nixpkgs.follows = "nixpkgs";

    emacs-overlay.url = "github:nix-community/emacs-overlay";
    emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";

    spacebar.url = "github:cmacrae/spacebar/v1.3.0";
  };

  outputs = { self, nixpkgs, mk-darwin-system, emacs-overlay, neovim, spacebar, ... }@inputs: 
    let
      flake-utils = mk-darwin-system.inputs.flake-utils;
      hostName = "shaunsingh-laptop";
      systems = [ "aarch64-darwin" ];

    in flake-utils.lib.eachSystem systems (system:
      mk-darwin-system.mkDarwinSystem {
        inherit hostName system;
        nixosModules = [
          ./modules/emacs.nix
          ./modules/homebrew.nix
          ./modules/git.nix
          ./modules/pam.nix
          ({ pkgs, lib, ... }: {
            system.stateVersion = 4;

            system.keyboard = {
              enableKeyMapping = true;
              remapCapsLockToEscape = true;
            };

            system.defaults = {
              screencapture = {
                  location = "/tmp";
              };
              dock = {
                autohide = true;
                showhidden = true;
                mru-spaces = false;
              };
              finder = {
                AppleShowAllExtensions = true;
                QuitMenuItem = true;
                FXEnableExtensionChangeWarning = true;
              };
              NSGlobalDomain = {
                AppleInterfaceStyle = "Dark";
                AppleKeyboardUIMode = 3;
                ApplePressAndHoldEnabled = false;
                AppleFontSmoothing = 1;
                _HIHideMenuBar = true;
                InitialKeyRepeat = 10;
                KeyRepeat = 1;
                "com.apple.mouse.tapBehavior" = 1;
                "com.apple.swipescrolldirection" = true;
              };
            };

            security.pam.enableSudoTouchIdAuth = true;

            programs.fish.enable = true;
            environment.shells = with pkgs; [ fish ];
            users.users.shauryasingh = {
              home = "/Users/shauryasingh";
              shell = pkgs.fish;
            };
            system.activationScripts.postActivation.text = ''
              # Set the default shell as fish for the user
              sudo chsh -s ${lib.getBin pkgs.fish}/bin/fish shauryasingh 
            '';

	    programs.tmux.enable = true;
	    programs.tmux.enableVim = true;
	    programs.tmux.extraConfig = ''
	     # make sure fish works in tmux
	     set -g  default-terminal   "xterm-256color"
	     set -sa terminal-overrides ',xterm-256color:RGB'
	     # so that escapes register immidiately in vim
	     set -sg escape-time 1
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
	

            services.spacebar.enable = true;
            services.spacebar.package = spacebar;
    	 
            nixpkgs = {
              overlays = [
                emacs-overlay.overlay
                neovim.overlay
                spacebar.overlay
              ];
              config.allowUnfree = true;
            };

            nix = {
              package = pkgs.nixUnstable;
              extraOptions = ''
                system = aarch64-darwin
                extra-platforms = aarch64-darwin x86_64-darwin
                experimental-features = nix-command flakes
                build-users-group = nixbld
              '';
            };
            services.nix-daemon.enable = true;

            fonts = {
              enableFontDir = true;
              fonts = with pkgs; [
                overpass
                alegreya
              ];
            };

            home-manager = {
              useGlobalPkgs = true;
              useUserPackages = true;
              users.shauryasingh.home.packages = with pkgs; [
                # Dotfiles Management
                yadm
              
                # Utils
                wget
                git-lfs
                exa
                bat
                tree

                # Mail
                ## offlineimap
                mu
                msmtp
                isync
              
                # Browsers
                ## firefox
                ## nyxt
                ## qutebrowser

                # Neovim
                neovim
              
                # Emacs config deps (latex, aspell)
                (aspellWithDicts (dicts: with dicts; [ en en-computers en-science ]))
                (pkgs.texlive.combine { inherit (pkgs.texlive) scheme-small dvipng dvisvgm l3packages xcolor soul adjustbox collectbox amsmath siunitx cancel mathalpha capt-of chemfig wrapfig mhchem fvextra latexmk; })
                sdcv

                # Chat
                discocss
              ];
            };
          })
        ];

        flakeOutputs = { pkgs, ... }@outputs:
          outputs // (with pkgs; { packages = { inherit hello; }; });
     });
}
