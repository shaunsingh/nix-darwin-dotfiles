{
  description = "Shaurya's Nix Environment";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    mk-darwin-system.url = "github:vic/mk-darwin-system/main";
    spacebar.url = "github:shaunsingh/spacebar/master";
  };

  outputs = { self, nixpkgs, mk-darwin-system, spacebar, ... }@inputs:
    let
      flake-utils = mk-darwin-system.inputs.flake-utils;
      hostName = "shaunsingh-laptop";
      systems = [ "aarch64-darwin" ];

    in flake-utils.lib.eachSystem systems (system:
      mk-darwin-system.mkDarwinSystem {
        inherit hostName system;
        nixosModules = [
          ./modules/emacs.nix
          ./modules/mac.nix
          ./modules/home.nix
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
                alegreya
                overpass
              ];
            };

            environment.systemPackages = with pkgs; [ ];
            home-manager = {
              useGlobalPkgs = true;
              useUserPackages = true;
            };
          })
        ];

        flakeOutputs = { pkgs, ... }@outputs:
          outputs // (with pkgs; { packages = { inherit hello; }; });
     });
}
