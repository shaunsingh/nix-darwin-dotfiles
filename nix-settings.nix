{ inputs
, system
, nixpkgs
}: {
  nixPath =
    let
      path = toString ./.;
    in
    [
      "nixpkgs=${nixpkgs}"
      "home-manager=${inputs.home-manager}"
    ];

  package = inputs.master.legacyPackages.${system}.nix;

  registry = {
    system.flake = inputs.self;
    default.flake = nixpkgs;
    home-manager.flake = inputs.home-manager;
  };

  settings = {
    accept-flake-config = true;

    trusted-substituters = [
      "https://cache.nixos.org?priority=10"
      "https://cache.ngi0.nixos.org/"
      "https://nix-community.cachix.org?priority=5"
      "https://nixpkgs-wayland.cachix.org"
    ];
    trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "cache.ngi0.nixos.org-1:KqH5CBLNSyX184S9BKZJo1LxrxJ9ltnY2uAs5c/f1MA="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "nixpkgs-wayland.cachix.org-1:3lwxaILxMRkVhehr5StQprHdEo4IrE8sRho9R9HOLYA="
    ];
    trusted-users = [ "root" "shauryasingh" ];

    max-jobs = "auto";
    http-connections = 0;
    experimental-features = [ "nix-command" "flakes" "ca-derivations" ];
  };

  # darwin stuff
  extraOptions = nixpkgs.lib.mkIf nixpkgs.pkgs.stdenv.isDarwin ''
    extra-platforms = aarch64-darwin x86_64-darwin
    build-users-group = nixbld
  '';
}
