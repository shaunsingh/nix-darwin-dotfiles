{
  nixConfig.extra-substituters = "https://emacsng.cachix.org";
  nixConfig.extra-trusted-public-keys = "emacsng.cachix.org-1:i7wOr4YpdRpWWtShI8bT6V7lOTnPeI7Ho6HaZegFWMI=";
  inputs = {
    # nixpkgs.url = "github:NixOS/nixpkgs/nixos-21.05";
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    emacsNg-src.url = "github:emacs-ng/emacs-ng";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    neovim.url = "github:neovim/neovim?dir=contrib";
    nixpkgs-f2k = {
        url = "github:fortuneteller2k/nixpkgs-f2k";
        inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, nixpkgs-f2k, emacsNg-src, emacs-overlay, neovim, flake-utils, ... }@inputs: {
    overlay = final: prev: {
      nixpkgs-extra = {
        sway-borders        = prev.callPackage ./nixos/pkgs/sway-borders {};
      };
    };
    nixosConfigurations = {
      shaunsingh-laptop = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          { _module.args = inputs; }
          { nixpkgs.overlays = [ nixpkgs-f2k.overlay ]; }
          ./nixos/configuration.nix
          ./nixos/hardware-configuration.nix
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
