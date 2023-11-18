{ config, lib, inputs, ... }:

{
  # home-manager configurations
  parts.homeConfigurations = {
    "shaunsingh@nix-darwin-aarch64" = {
      system = "aarch64-darwin";
      stateVersion = "23.05";
      agenix = true;

      modules = [ ./shaunsingh/home.nix ]
        ++ lib.optional config.parts.homeConfigurations."shaurizard@nix-darwin-aarch64".agenix ./shaunsingh/age.nix;
    };

    "shaurizard@nixos-c1-x86" = {
      system = "x86_64-linux";
      stateVersion = "23.05";

      modules = [
        inputs.base16.hmModule
        ./shaurizard/home.nix
      ];
    };
  };
}