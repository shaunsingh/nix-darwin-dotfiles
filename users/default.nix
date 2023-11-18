_:
{
  # home-manager configurations
  parts.homeConfigurations = {
    "shaunsingh@m1" = {
      system = "aarch64-darwin";
      stateVersion = "23.05";

      modules = [ ./shaunsingh/home.nix ];
    };

    "shaurizard@c1" = {
      system = "x86_64-linux";
      stateVersion = "23.05";

      modules = [ ./shaurizard/home.nix ];
    };
  };
}
