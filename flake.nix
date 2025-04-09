{
  description = "flake to run nyxt config as kiosk under asahi/nixOS +gamescope";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    nixpkgs-wayland.url = "github:nix-community/nixpkgs-wayland";

    home-manager.url = "github:nix-community/home-manager";
    rust-overlay.url = "github:oxalica/rust-overlay";
    eww.url = "github:elkowar/eww";

    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    rust-overlay.inputs.nixpkgs.follows = "nixpkgs";
    eww.inputs.nixpkgs.follows = "nixpkgs";
    eww.inputs.rust-overlay.follows = "rust-overlay";
  };
  outputs = {
    self,
    nixpkgs,
    home-manager,
    ...
  } @ inputs: let
    inherit (self) outputs;
  in {
    nixosConfigurations = {
      nyxtkiosk = nixpkgs.lib.nixosSystem {
        specialArgs = {inherit inputs outputs;};
        modules = [
          ./configuration.nix
          home-manager.nixosModules.home-manager
          ({
            inputs,
            lib,
            config,
            pkgs,
            ...
          }: let
            flakeInputs = lib.filterAttrs (_: lib.isType "flake") inputs;
          in {
            config = {
              nix = {
                channel.enable = false;
                registry = lib.mapAttrs (_: flake: {inherit flake;}) flakeInputs;
                nixPath = lib.mapAttrsToList (n: _: "${n}=flake:${n}") flakeInputs;
                settings = {
                  experimental-features = "nix-command flakes";
                  flake-registry = "";
                  trusted-public-keys = [
                    "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
                    "nixpkgs-wayland.cachix.org-1:3lwxaILxMRkVhehr5StQprHdEo4IrE8sRho9R9HOLYA="
                  ];
                  substituters = [
                    "https://cache.nixos.org"
                    "https://nixpkgs-wayland.cachix.org"
                  ];
                };
              };
              nixpkgs = {
                overlays = [
                  inputs.nixpkgs-wayland.overlay
                ];
                config.allowUnfree = true;
              };
              home-manager = {
                useGlobalPkgs = true;
                useUserPackages = true;
                extraSpecialArgs = {inherit inputs outputs;};
              };
            };
          })
        ];
      };
    };
  };
}
