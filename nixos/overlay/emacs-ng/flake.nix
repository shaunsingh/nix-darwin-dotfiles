{
  description = "sway-borders";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/master";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = { url = "github:edolstra/flake-compat"; flake = false; };
    flake-compat-ci.url = "github:hercules-ci/flake-compat-ci";

    nixConfig.extra-substituters = "https://emacsng.cachix.org";
    nixConfig.extra-trusted-public-keys = "emacsng.cachix.org-1:i7wOr4YpdRpWWtShI8bT6V7lOTnPeI7Ho6HaZegFWMI=";

    inputs = {
      nixpkgs = { url = "nixpkgs/nixpkgs-unstable"; };
      emacs-overlay = {
        type = "github";
        owner = "nix-community";
        repo = "emacs-overlay";
      };

    emacs-ng = { url = "github:emacs-ng/emacs-ng"; };

  };

  outputs = args@{ self, flake-utils, nixpkgs, ... }:
    {
      ciNix = args.flake-compat-ci.lib.recurseIntoFlake self;

      overlay = final: prev: {
        inherit (self.packages.${final.system})
          emacs-ng-git;
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
      in
      {

        emacsNg = emacs-ng.defaultPackage."${final.system}";

        packages = rec {

        emacs-ng-git = (pkgs.emacsNg.overrideAttrs (old: {
          #withWebrender = true;
        }));

        };
      }
    );
}
