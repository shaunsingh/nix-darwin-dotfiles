{ inputs, ... }:

{
  # nix-darwin configurations
  parts.darwinConfigurations.nix-darwin-aarch64 = {
    system = "aarch64-darwin";
    stateVersion = 4;
    modules = [ ./nix-darwin-aarch64/configuration.nix ];
  };

  # NixOS configurations
  parts.nixosConfigurations = {
    nixos-c1-x86 = {
      system = "x86_64-linux";
      stateVersion = "23.05";

      modules = [
        ./nixos-c1-x86/configuration.nix
      ];
    };
  };
}