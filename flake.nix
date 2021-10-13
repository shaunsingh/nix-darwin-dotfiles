{
  description = "Shaurya's Nix Environment";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    spacebar = {
      url = "github:shaunsingh/spacebar/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    neovim = {
      url = "github:neovim/neovim?dir=contrib";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    darwin = {
      url = "github:LnL7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, spacebar, neovim, darwin, home-manager, ... }@inputs: {
    darwinConfigurations."shaunsingh-laptop" = darwin.lib.darwinSystem {
      system = "aarch64-darwin";
      modules = [
        ./modules/emacs.nix
        ./modules/mac.nix
        ./modules/home.nix
        ./modules/pam.nix
        ./modules/mbsync.nix
        home-manager.darwinModule
          {
            home-manager = {
              useGlobalPkgs = true;
              useUserPackages = true;
            };
          }
        ({ pkgs, lib, ... }: {
          security.pam.enableSudoTouchIdAuth = true;
          nixpkgs = {
            overlays = [ spacebar.overlay neovim.overlay ];
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
        })
      ];
    };
  };
}
