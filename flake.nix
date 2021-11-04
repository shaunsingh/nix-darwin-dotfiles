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
    # All packages should follow latest nixpkgs home-manager & flake-utils
    unstable.url = "github:nixos/nixpkgs/nixpkgs-unstable";
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
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "unstable";
    };
  };

  outputs = { self, nixpkgs, spacebar, neovim, emacs, darwin, home-manager, ...
    }@inputs: {
      darwinConfigurations."shaunsingh-laptop" = darwin.lib.darwinSystem {
        system = "aarch64-darwin";
        modules = [
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
              overlays = [ spacebar.overlay neovim.overlay emacs.overlay ];
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
              ((emacsPackagesNgGen emacsGcc).emacsWithPackages
                (epkgs: [ epkgs.vterm epkgs.pdf-tools ]))
              ## make sure ripgrep supports pcre2 (for vertico)
              (ripgrep.override { withPCRE2 = true; })
              binutils
              gnutls
              gnuplot
              sqlite
              # tree-sitter
              (aspellWithDicts (ds: with ds; [ en en-computers en-science ]))
              (texlive.combine {
                inherit (texlive)
                  scheme-small dvipng dvisvgm l3packages xcolor soul adjustbox
                  collectbox amsmath siunitx cancel mathalpha capt-of chemfig
                  wrapfig mhchem fvextra cleveref latexmk tcolorbox environ arev
                  amsfonts simplekv alegreya sourcecodepro newpx;
              })
              sdcv

              # Language deps
              python39Packages.grip
              python39Packages.pyflakes
              python39Packages.isort
              python39Packages.pytest
              nodePackages.pyright
              pipenv
              nixfmt
              black
              rust-analyzer
              rustup
              shellcheck

              # Terminal utils
              wget
              exa
              tree
              fd
              sd
              discocss
              neovim-nightly
            ];
            fonts = {
              enableFontDir = true;
              fonts = with pkgs; [
                alegreya
                overpass
                alegreya-sans
                ibm-plex
                emacs-all-the-icons-fonts
              ];
            };
          })
        ];
      };
    };
}
