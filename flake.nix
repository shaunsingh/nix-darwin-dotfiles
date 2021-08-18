{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-21.05";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    emacsNg-src.url = "github:emacs-ng/emacs-ng";
  };
  outputs = { nixpkgs, nixpkgs-unstable, emacsNg-src, ... }@inputs: {
    nixosConfigurations = {
      shaunsingh-laptop = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          { _module.args = inputs; }
          ./nixos/configuration.nix
          ./nixos/hardware-configuration.nix
        ];
      };
    };
  };
}
