{ config, lib, inputs, ... }:
{
  # home-manager configurations
  parts.homeConfigurations = {
    "shaunsingh@nix-darwin-aarch64" = {
      system = "aarch64-darwin";
      stateVersion = "23.05";

      modules = [ ./shaunsingh/home.nix ];
    };

    "shaurizard@nixos-c1-x86" = {
      system = "x86_64-linux";
      stateVersion = "23.05";

      modules = [ ./shaurizard/home.nix ];
    };
  };
}
