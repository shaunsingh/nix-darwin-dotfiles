

# The =flake.nix= below does the following:
# 1. Add a binary cache for =nix-community= overlays
# 2. Add inputs (=nixpkgs-master=, =nix-darwin=, =home-manager,= and =spacebar=)
# 3. Add overlays to get the latest versions of =neovim= (nightly) and =emacs= (emacs29)
# 4. Create a nix-darwin configuration for my hostname
# 5. Source the [[file:./modules/mac.nix][mac]], [[file:./modules/home.nix][home]], and [[file:./modules/pam.nix][pam]] modules
# 6. Configure home-manager and the nix-daemon
# 7. Enable the use of touch-id for sudo authentication
# 8. Configure =nixpkgs= to use the overlays above, and allow unfree packages
# 9. Configure =nix= to enable =flakes= and =nix-command= by default, and add =x86-64-darwin= as a platform (to install packages through rosetta)
# 10. Install my packages and config dependencies
# 11. Install the required fonts

# [[file:nix-config.org::*Notes on using the flake][Notes on using the flake:4]]
{
  description = "Shaurya's Nix Environment";

  nixConfig.extra-substituters = "https://nix-community.cachix.org";
  nixConfig.extra-trusted-public-keys =
    "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs=";

  inputs = {
    # All packages should follow latest nixpkgs
    unstable.url = "github:nixos/nixpkgs/master";
    nur.url = "github:nix-community/NUR";

    doom-emacs.url = "github:hlissner/doom-emacs/develop";
    doom-emacs.flake = false;

    darwin = {
      url = "github:LnL7/nix-darwin/master";
      inputs.nixpkgs.follows = "unstable";
    };
    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "unstable";
    };
    # Bar
    spacebar = {
      url = "github:shaunsingh/spacebar/master";
      inputs.nixpkgs.follows = "unstable";
    };
    yabai-src = {
      url = "github:koekeishiya/yabai/master";
      flake = false;
    };
    # Editors
    neovim = {
      url = "github:nix-community/neovim-nightly-overlay";
      inputs.nixpkgs.follows = "unstable";
    };
    emacs = {
      url = "github:shaunsingh/emacs";
      inputs.nixpkgs.follows = "unstable";
    };
    # overlays
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "unstable";
    };
    nixpkgs-s2k = {
      url = "github:shaunsingh/nixpkgs-s2k";
      inputs.nixpkgs.follows = "unstable";
    };
  };

  outputs = { self, nixpkgs, nixpkgs-s2k, darwin, home-manager, doom-emacs, ...
    }@inputs: {
      darwinConfigurations."shaunsingh-laptop" = darwin.lib.darwinSystem {
        system = "aarch64-darwin";
        modules = [
          { nixpkgs.overlays = [ nixpkgs-s2k.overlay ]; }
          ./modules/mac.nix
          ./modules/home.nix
          ./modules/pam.nix
          ./modules/editors.nix
          home-manager.darwinModule
          {
            home-manager = {
              useGlobalPkgs = true;
              useUserPackages = true;
            };
          }
          ({ pkgs, lib, ... }: {
            services.nix-daemon.enable = true;
            security.pam.enableSudoTouchIdAuth = true;
            nixpkgs = {
              overlays = with inputs; [
                nur.overlay
                spacebar.overlay
                neovim.overlay
                emacs.overlay
                rust-overlay.overlay
                (final: prev: { doomEmacsRevision = doom-emacs.rev; })
                (final: prev: {
                  yabai = let
                    version = "4.0.0-dev";
                    buildSymlinks = prev.runCommand "build-symlinks" { } ''
                      mkdir -p $out/bin
                      ln -s /usr/bin/xcrun /usr/bin/xcodebuild /usr/bin/tiffutil /usr/bin/qlmanage $out/bin
                    '';
                  in prev.yabai.overrideAttrs (old: {
                    inherit version;
                    src = inputs.yabai-src;
                    buildInputs = with prev.darwin.apple_sdk.frameworks; [
                      Carbon
                      Cocoa
                      ScriptingBridge
                      prev.xxd
                      SkyLight
                    ];
                    nativeBuildInputs = [ buildSymlinks ];
                  });
                })
              ];
              config.allowUnfree = true;
            };
            nix = {
              package = pkgs.nix;
              extraOptions = ''
                system = aarch64-darwin
                extra-platforms = aarch64-darwin x86_64-darwin
                experimental-features = nix-command flakes
                build-users-group = nixbld
              '';
            };
            environment.systemPackages = with pkgs; [
              # Build Tools
              jdk
              rust-bin.nightly.latest.default

              # Language Servers
              nodePackages.pyright
              rust-analyzer

              # Formatting
              nixfmt
              black
              shellcheck

              # Terminal utils and rust alternatives :tm:
              xcp
              lsd
              procs
              tree
              zoxide
              bottom
              gpx
            ];
            fonts = {
              enableFontDir = true;
              fonts = with pkgs; [
                overpass
                fira
                emacs-all-the-icons-fonts
                sf-mono-liga-bin
              ];
            };
          })
        ];
      };
    };
}
# Notes on using the flake:4 ends here
