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
    nixos-c1-x86_64 = {
      system = "x86_64-linux";
      stateVersion = "23.05";

      modules = [
#        TODO agenix
#         inputs.agenix.nixosModules.default
#         {
#           # NOTE: you should either change this or disable it completely by commenting it out
#           age.secrets.tokens = {
#             file = ../secrets/tokens.age;
#             owner = "moni";
#             mode = "0444";
#           };
#         }

        ./nixos-c1-x86_64/configuration.nix
      ];
    };
  };
}