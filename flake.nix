{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-21.05";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    emacsNg-src.url = "github:emacs-ng/emacs-ng"
  };

  outputs = { nixpkgs, nixpkgs-unstable, emacs-overlay, ... }: {
    nixosConfigurations = {
      shaunsingh-laptop = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ./config/nixos/configuration.nix
          ./config/nixos/hardware-configuration.nix
        ];
      };
    };
  };
}
