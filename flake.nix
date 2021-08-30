{

  nixConfig = {

    # use binaries for emacsng
    extra-substituters = "https://emacsng.cachix.org";
    extra-trusted-public-keys = "emacsng.cachix.org-1:i7wOr4YpdRpWWtShI8bT6V7lOTnPeI7Ho6HaZegFWMI=";

  };

  inputs = {

    # use nixpkgs unstable
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    # I prefer emacs-ng, but emacsPgtkGcc is also an option
    emacsNg-src.url = "github:emacs-ng/emacs-ng";

    # Grab the latest neovim and compile it
    neovim.url = "github:neovim/neovim?dir=contrib";

    # attempt to use sway-borders
    nixpkgs-overlays = {
        url = "path:./nixos/overlays/";
        inputs.nixpkgs.follows = "nixpkgs";
    };

  };

  outputs = { self, nixpkgs, nixpkgs-overlays, emacsNg-src, neovim, ... }@inputs: {

    overlay = final: prev: {
      # use busybox coreutils (disabled for now)
      oldutils = prev.coreutils; coreutils = final.busybox;
    };

    nixosConfigurations = {
      # macbook 6,1 config
      shaunsingh-laptop = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          { _module.args = inputs; }

          # use those extra packages as an overlay
          { nixpkgs.overlays = [ nixpkgs-overlays.overlay ]; }

          # source our two config files, obviously
          ./nixos/configuration.nix
          ./nixos/hardware-configuration.nix

          # add the neovim overlay
          ({ pkgs, ... }: {
            nixpkgs.overlays = [
              neovim.overlay
              # self.overlay
            ];
          })
        ];
      };
    };
  };
}
