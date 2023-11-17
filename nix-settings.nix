{ inputs
, system
, nixpkgs
}: {
  nixPath = [ "nixpkgs=${nixpkgs}" ];
  package = inputs.master.legacyPackages.${system}.nix;

  registry = {
    system.flake = inputs.self;
    default.flake = nixpkgs;
    home-manager.flake = inputs.home-manager;
  };

  settings = {
    accept-flake-config = true;

    experimental-features = [
      "auto-allocate-uids"
      "ca-derivations"
      "flakes"
      "nix-command"
    ];

    max-jobs = "auto";

    # home-manager will attempt to rebuild the world otherwise...
    trusted-substituters = [
      "https://cache.nixos.org?priority=7"
      "https://mirrors.tuna.tsinghua.edu.cn/nix-channels/store/?priority=10"
      "https://cache.ngi0.nixos.org/"
      "https://nix-community.cachix.org?priority=5"
      "https://nixpkgs-wayland.cachix.org"
      "https://nix-gaming.cachix.org"
    ];

    trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "cache.ngi0.nixos.org-1:KqH5CBLNSyX184S9BKZJo1LxrxJ9ltnY2uAs5c/f1MA="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "nixpkgs-wayland.cachix.org-1:3lwxaILxMRkVhehr5StQprHdEo4IrE8sRho9R9HOLYA="
       "nix-gaming.cachix.org-1:nbjlureqMbRAxR1gJ/f3hxemL9svXaZF/Ees8vCUUs4="
    ];

    trusted-users = [ "root" "shauryasingh" ];
  };

  extraOptions = ''
    keep-outputs = true
    keep-derivations = true
    auto-allocate-uids = false
    builders-use-substitutes = true
    http-connections = 0
  ''
  +
  (nixpkgs.lib.optionalString (system == "aarch64-darwin") ''
    extra-platforms = aarch64-darwin x86_64-darwin
  '');
}
