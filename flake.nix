

# The =flake.nix= below does the following:
# 1. Add a binary cache for =nix-community= overlays
# 2. Add inputs (=nixpkgs-master=, =nix-darwin=, =home-manager,= and =spacebar=)
# 3. Add overlays to get the latest versions of =neovim= (nightly) and =emacs= (emacs29)
# 4. Create a nix-darwin configuration for my hostname
# 5. Source the [[./modules/mac.nix][mac]], [[./modules/home.nix][home]], and [[./modules/pam.nix][pam]] modules
# 6. Configure home-manager and the nix-daemon
# 7. Enable the use of touch-id for sudo authentication
# 8. Configure =nixpkgs= to use the overlays above, and allow unfree packages
# 9. Configure =nix= to enable =flakes= and =nix-command= by default, and add =x86-64-darwin= as a platform (to install packages through rosetta)
# 10. Install my packages and config dependencies
# 11. Install the required fonts

# [[file:nix-config.org::*Notes on using the flake][Notes on using the flake:4]]
{
  description = "Shaurya's Nix Environment";

  nixConfig = {
    # Add binary cache for neovim-nightly/emacsGcc
    extra-substituters =
      [ "https://cachix.cachix.org" "https://nix-community.cachix.org" ];
    extra-trusted-public-keys = [
      "cachix.cachix.org-1:eWNHQldwUO7G2VkjpnjDbWwy4KQ/HNxht7H4SSoMckM="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };

  inputs = {
    # All packages should follow latest nixpkgs
    unstable.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    nur.url = "github:nix-community/NUR";
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

  outputs = { self, nixpkgs, nixpkgs-s2k, darwin, home-manager, ...
    }@inputs: {
      darwinConfigurations."shaunsingh-laptop" = darwin.lib.darwinSystem {
        system = "aarch64-darwin";
        modules = [
          { nixpkgs.overlays = [ nixpkgs-s2k.overlay ]; }
          ./modules/mac.nix
          ./modules/home.nix
          ./modules/pam.nix
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
            environment.systemPackages = with pkgs; [
              # Emacs deps
              ((emacsPackagesNgGen emacs).emacsWithPackages
                (epkgs: [ epkgs.vterm ]))
              ## make sure ripgrep supports pcre2 (for vertico)
              (ripgrep.override { withPCRE2 = true; })
              sqlite
              # zstd
              ## Required for plots but not installed by default
              gnuplot
              # pandoc
              ## Required for dictionaries but not installed by default
              sdcv
              (aspellWithDicts (ds: with ds; [ en en-computers en-science ]))
              (texlive.combine {
                inherit (texlive)
                  scheme-small dvipng dvisvgm l3packages xcolor soul adjustbox
                  collectbox amsmath siunitx cancel mathalpha capt-of chemfig
                  wrapfig mhchem fvextra cleveref latexmk tcolorbox environ arev
                  amsfonts simplekv alegreya sourcecodepro newpx svg catchfile
                  transparent hanging;
              })

              # Jetbrains deps
              # jdk

              # Language deps
              # python39Packages.grip
              # python39Packages.pyflakes
              # python39Packages.isort
              # python39Packages.pytest
              nodePackages.pyright
              # pipenv
              nixfmt
              # black
              rust-analyzer
              rust-bin.nightly.latest.default
              # shellcheck
              languagetool
              
              # Terminal utils and rust alternatives :tm:
              uutils-coreutils
              xcp
              # exa
              procs
              tree
              fd
              zoxide
              bottom
              discocss
              # eww
            ];
            fonts = {
              enableFontDir = true;
              fonts = with pkgs; [
                overpass
                alegreya
                alegreya-sans
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
