{
  description = "NixOS/nix-darwin/home-manager configuration using Nix Flakes.";
  nixConfig.commit-lockfile-summary = "flake: bump inputs";

  outputs = inputs: inputs.parts.lib.mkFlake { inherit inputs; } {
    systems = [ "aarch64-darwin" "x86_64-linux" ];
    imports = [ ./modules/parts ./overlays ./hosts ./users ];
  };

  inputs = {
    ### -- nix lang & nixpkgs help
    nix.url = "github:nixos/nix";
    nix-index-database.url = "github:Mic92/nix-index-database";
    nixpkgs-fmt.url = "github:nix-community/nixpkgs-fmt";
    parts.url = "github:hercules-ci/flake-parts";
    statix.url = "github:nerdypepper/statix";

    ### -- nixpkgs
    master.url = "github:nixos/nixpkgs/master";
    stable.url = "github:nixos/nixpkgs/release-23.05";
    unstable.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    nur.url = "github:nix-community/NUR";

    # Default Nixpkgs for packages and modules
    nixpkgs.follows = "master";

    ### -- platform support
    darwin.url = "github:lnl7/nix-darwin";
    home.url = "github:nix-community/home-manager";
    nixos-wsl.url = "github:nix-community/nixos-wsl";
    nixpkgs-wayland.url = "github:nix-community/nixpkgs-wayland";

    ### -- tooling
    # colorscheme
    base16.url = "github:shaunsingh/base16.nix";
    base16-oxocarbon = {
      url = "github:shaunsingh/base16-oxocarbon";
      flake = false;
    };

    # language support
    rust-overlay.url = "github:oxalica/rust-overlay";

    # gaming
    nix-gaming.url = "github:fufexan/nix-gaming";

    # desktop
    eww.url = "github:elkowar/eww";
    eww.inputs.rust-overlay.follows = "rust-overlay";

    # Minimize duplicate instances of inputs
    nix.inputs.nixpkgs.follows = "nixpkgs";
    nix-index-database.inputs.nixpkgs.follows = "nixpkgs";
    darwin.inputs.nixpkgs.follows = "nixpkgs";
    home.inputs.nixpkgs.follows = "nixpkgs";
    nixos-wsl.inputs.nixpkgs.follows = "nixpkgs";
    statix.inputs.nixpkgs.follows = "nixpkgs";
    base16.inputs.nixpkgs.follows = "nixpkgs";
    rust-overlay.inputs.nixpkgs.follows = "nixpkgs";
    eww.inputs.nixpkgs.follows = "nixpkgs";
  };
}