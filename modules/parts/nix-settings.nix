{ lib, stdenv, inputs, inputs' }:

# Nix daemon settings that can't be put in `nixConfig`.
{

# TODO buildmachines
#   buildMachines = lib.optionals stdenv.isDarwin [
#     {
#       hostName = "192.168.1.9";
#       system = "x86_64-linux";
#       sshUser = "moni";
#       sshKey = "/Users/moni/.ssh/id_ed25519";
#       publicHostKey = "c3NoLWVkMjU1MTkgQUFBQUMzTnphQzFsWkRJMU5URTVBQUFBSUtyUGRxSWlUckdxbk42ZUFoUnVHbDlaVjJzVXovSVI4NVQzL1R6VVQ0T2wgcm9vdEBzdGFyY3J1aXNlcgo=";
#       maxJobs = 6;
#       speedFactor = 2;
#       supportedFeatures = [ "nixos-test" "benchmark" "big-parallel" "kvm" ];
#       mandatoryFeatures = [ ];
#     }
# 
#     {
#       hostName = "localhost";
#       system = "aarch64-linux";
#       sshUser = "builder";
#       sshKey = "/Users/moni/.ssh/builder_ed25519";
#       publicHostKey = "c3NoLWVkMjU1MTkgQUFBQUMzTnphQzFsWkRJMU5URTVBQUFBSUpCV2N4Yi9CbGFxdDFhdU90RStGOFFVV3JVb3RpQzVxQkorVXVFV2RWQ2Igcm9vdEBuaXhvcwo=";
#       maxJobs = 6;
#       speedFactor = 3;
#       supportedFeatures = [ "nixos-test" "benchmark" "big-parallel" "kvm" ];
#       mandatoryFeatures = [ ];
#     }
#   ];
# 
#   distributedBuilds = true;

  extraOptions = ''
    keep-outputs = true
    keep-derivations = true
    auto-allocate-uids = false
    builders-use-substitutes = true
    http-connections = 0
  '';

  nixPath = [ "nixpkgs=${inputs.nixpkgs}" ];

  registry = {
    system.flake = inputs.self;
    default.flake = inputs.nixpkgs;
    home-manager.flake = inputs.home;
  };

  settings = {
    accept-flake-config = true;
    flake-registry = __toFile "begone-evil.json" (__toJSON { flakes = [ ]; version = 2; });

    experimental-features = [
      "auto-allocate-uids"
      "ca-derivations"
      "flakes"
      "nix-command"
    ];

    max-jobs = "auto";

    # home-manager will attempt to rebuild the world otherwise...
    trusted-substituters = [
      "https://mirrors.tuna.tsinghua.edu.cn/nix-channels/store/?priority=10"
      "https://cache.nixos.org?priority=7"
      "https://nix-community.cachix.org?priority=5"
      "https://nixpkgs-wayland.cachix.org"
      "https://nix-gaming.cachix.org"
    ];

    trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "nixpkgs-wayland.cachix.org-1:3lwxaILxMRkVhehr5StQprHdEo4IrE8sRho9R9HOLYA="
      "nix-gaming.cachix.org-1:nbjlureqMbRAxR1gJ/f3hxemL9svXaZF/Ees8vCUUs4="
    ];

    trusted-users = [ "root" "shaunsingh" "shaurizard" ];
    use-xdg-base-directories = true;
  } // (lib.optionalAttrs (stdenv.isDarwin && stdenv.isAarch64) {
    extra-platforms = "x86_64-darwin";
  });
}