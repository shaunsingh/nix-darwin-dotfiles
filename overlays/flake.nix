# Overlays
# Sometimes there are packages that I want from git, or that aren't available from =nixpkgs=. This overlay adds the following:
# - Yabai (mac) from https://github.com/donaldguy/yabai
# - Neovide (mac) from https://github.com/neovide/neovide

# [[file:../nix-config.org::*Overlays][Overlays:1]]
{
  description = "Shaunsingh's stash of fresh packages";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/master";

    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    rust-nightly = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    yabai-src = {
      url = "github:donaldguy/yabai/canon";
      flake = false;
    };

    neovide-src = {
      url = "github:neovide/neovide";
      flake = false;
    };

  };

  outputs = args@{ self, flake-utils, nixpkgs, rust-nightly, ... }:
    {
      overlay = final: prev: {
        inherit (self.packages.${final.system})
          yabai-git
          neovide-git;
      };
    }
    // flake-utils.lib.eachSystem [ "aarch64-darwin" ] (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ rust-nightly.overlay ];
          # allowBroken = true;
          # allowUnsupportedSystem = true;
        };
        version = "999-unstable";
      in
      {

        defaultPackage = self.packages.${system}.neovide-git;

        packages = rec {

        yabai-git = (pkgs.yabai.overrideAttrs (old: {
            inherit version;
            src = args.yabai-src;
            # buildInputs = [ Carbon Cocoa ScriptingBridge xxd ];
            buildInputs = (old.buildInputs or [ ]) ++ (with pkgs; [
              xcodebuild
            ]);
          }));

        neovide-git = (pkgs.neovide.overrideAttrs (old: {
            inherit version;
            src = args.neovide-src;
            buildInputs = (old.buildInputs or [ ]) ++ (with pkgs; [
              rust-bin.nightly.latest.default
            ]);
          }));
        };
      }
    );
}
# Overlays:1 ends here
