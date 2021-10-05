{

  description = "Shaurya's Darwin System";

  inputs = {

    # use nixpkgs unstable
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable-darwin";

    # Grab the latest neovim and compile it
    neovim.url = "github:neovim/neovim?dir=contrib";

    # Same with emacs
    emacs-overlay.url = "github:nix-community/emacs-overlay";

    # attempt to use sway-borders
    nixpkgs-overlays = {
        url = "path:./nixos/overlays/";
        inputs.nixpkgs.follows = "nixpkgs";
    };

    # Nix Darwin
    darwin.url = "github:lnl7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "nixpkgs";

  };

  outputs = { self, nixpkgs, darwin, nixpkgs-overlays, emacs-overlay, neovim, ... }@inputs: {
    darwinConfigurations."shaunsingh-laptop" = darwin.lib.darwinSystem {
      system = "aarch64-darwin";
      modules = [
        { _module.args = inputs; }
        # use those extra packages as an overlay
        { nixpkgs.overlays = [ nixpkgs-overlays.overlay ]; }
        # source our two config files, obviously
        ./nixos/darwin-configuration.nix
        # add the neovim and emacs overlays 
        ({ pkgs, ... }: {
          nixpkgs.overlays = [
            neovim.overlay
            emacs-overlay.overlay
          ];
        })
      ];
    };
  };
}
