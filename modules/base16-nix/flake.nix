{
  description = "Base16-template builder for nix.";
  inputs.nixpkgs.url = "github:nixos/nixpkgs/master";

  outputs = { self, nixpkgs }@inputs:
    with nixpkgs.legacyPackages."aarch64-darwin"; rec {
      # Home-Manager Module
      hmModule = ./base16.nix;
    };
}
