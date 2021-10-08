{
  description = "Shaurya's Nix Environment";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    mk-darwin-system.url = "github:vic/mk-darwin-system/main";

    emacs-overlay.url = "github:nix-community/emacs-overlay";
    emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";

    spacebar.url = "github:cmacrae/spacebar/v1.3.0";
  };

  outputs = { self, nixpkgs, mk-darwin-system, emacs-overlay, spacebar, ... }@inputs: 
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
          ./modules/mbsync.nix
          ({ pkgs, lib, ... }: {
            networking.hostName = "shaunsingh-laptop";
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

            nixpkgs = {
              overlays = [
                emacs-overlay.overlay
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
            services.mbsync.enable = true;

            fonts = {
              enableFontDir = true;
              fonts = with pkgs; [
                overpass
                alegreya
              ];
            };

            environment.systemPackages = with pkgs; [ ];
            home-manager = {
              useGlobalPkgs = true;
              useUserPackages = true;
              users.shauryasingh.home.packages = with pkgs; [
                # Utils
                yadm
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

                # Emacs config deps (latex, aspell)
                (aspellWithDicts (dicts: with dicts; [ en en-computers en-science ]))
                (pkgs.texlive.combine { inherit (pkgs.texlive) scheme-small dvipng dvisvgm l3packages xcolor soul adjustbox collectbox amsmath siunitx cancel mathalpha capt-of chemfig wrapfig mhchem fvextra latexmk; })
                sdcv

                # Neovim
                neovim 
                ## neovide

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
