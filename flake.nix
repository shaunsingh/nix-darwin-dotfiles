{

  # use binaries for emacsng
  nixConfig.extra-substituters = "https://emacsng.cachix.org";
  nixConfig.extra-trusted-public-keys = "emacsng.cachix.org-1:i7wOr4YpdRpWWtShI8bT6V7lOTnPeI7Ho6HaZegFWMI=";

  inputs = {
    # use nixpkgs unstable
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    # I prefer emacs-ng, but emacsPgtkGcc is also an option
    emacsNg-src.url = "github:emacs-ng/emacs-ng";
    emacs-overlay.url = "github:nix-community/emacs-overlay";

    # Grab the latest neovim and compile it
    neovim.url = "github:neovim/neovim?dir=contrib";

    # Nice selection of extra packages, noteably rust-nightly and alacritty-ligaturs
    nixpkgs-f2k = {
        url = "github:fortuneteller2k/nixpkgs-f2k";
        inputs.nixpkgs.follows = "nixpkgs";
    };

    # attempt to use sway-borders
    nixpkgs-sway-border = {
        url = "path:/nixos/overlay/sway-border";
        inputs.nixpkgs.follows = "nixpkgs";
    };

  };

  outputs = { self, nixpkgs, nixpkgs-f2k, nixpkgs-sway-border, emacsNg-src, emacs-overlay, neovim, ... }@inputs: {
    nixosConfigurations = {
      # macbook 6,1 config
      shaunsingh-laptop = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          { _module.args = inputs; }

          # use those extra packages as an overlay
          { nixpkgs.overlays = [ nixpkgs-f2k.overlay ]; }
          { nixpkgs.overlays = [ nixpkgs-sway-border.overlay ]; }

          # source our two config files, obviously
          ./nixos/configuration.nix
          ./nixos/hardware-configuration.nix

          # add both the neovim and emacs overlay, as well as my own sway overlay
          ({ pkgs, ... }: {
            nixpkgs.overlays = [
              emacs-overlay.overlay
              neovim.overlay
            ];
          })
        ];
      };
    };
  };
}
