{
  description = "sway-borders";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/master";
    # NOTE: remove when meson has advanced to 0.58.1 in master
    meson058.url = "github:jtojnar/nixpkgs/meson-0.58";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = { url = "github:edolstra/flake-compat"; flake = false; };
    flake-compat-ci.url = "github:hercules-ci/flake-compat-ci";
    sway-src = { url = "github:fluix-dev/sway-borders"; flake = false; };
    wlroots-src = { url = "github:swaywm/wlroots"; flake = false; };

  };

  outputs = args@{ self, flake-utils, nixpkgs, meson058, ... }:
    {
      ciNix = args.flake-compat-ci.lib.recurseIntoFlake self;

      overlay = final: prev: {
        inherit (self.packages.${final.system})
          sway-borders-git
          wlroots-git;
      };
    }
    // flake-utils.lib.eachSystem [ "x86_64-linux" "aarch64-linux" ] (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          allowBroken = true;
          allowUnsupportedSystem = true;
        };

        version = "999-unstable";

        mesonPkgs = import meson058 { inherit system; };
      in
      {

        defaultPackage = self.packages.${system}.sway-borders-git;

        packages = rec {

          sway-borders-git = (pkgs.sway-unwrapped.overrideAttrs (_: {
            src = args.sway-src;
          })).override {
            inherit (mesonPkgs) meson;
            wlroots = wlroots-git;
          };

          wlroots-git = (pkgs.wlroots.overrideAttrs (old: {
            inherit version;
            src = args.wlroots-src;

            buildInputs = (old.buildInputs or [ ]) ++ (with pkgs; [
              seatd
            ]);
          })).override {
            inherit (mesonPkgs) meson;
          };

        };
      }
    );
}
