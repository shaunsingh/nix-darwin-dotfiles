{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-21.05";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    emacsNg-src.url = "github:emacs-ng/emacs-ng";
  };
  outputs = { nixpkgs, nixpkgs-unstable, emacsNg-src, ... }: {
    nixosConfigurations = {
      shaunsingh-laptop = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          { _module.args = inputs; }
          ./config/nixos/configuration.nix
          ./config/nixos/hardware-configuration.nix
        ];
      };
    };
  };
}
