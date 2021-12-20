{
  description = "Shaurya's Nix Environment";

  nixConfig.extra-substituters = "https://nix-community.cachix.org";
  nixConfig.extra-trusted-public-keys =
    "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs=";

  inputs = {
    # All packages should follow latest nixpkgs
    unstable.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    nur.url = "github:nix-community/NUR";
    # core
    darwin = {
      url = "github:LnL7/nix-darwin/master";
      inputs.nixpkgs.follows = "unstable";
    };
    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "unstable";
    };
    # Bar
    spacebar = {
      url = "github:shaunsingh/spacebar/master";
      inputs.nixpkgs.follows = "unstable";
    };
    # WM
    yabai-src = {
      url = "github:koekeishiya/yabai/master";
      flake = false;
    };
    # Editors
    emacs-src = {
      url = "github:emacs-mirror/emacs";
      flake = false;
    };
    doom-emacs = {
      url = "github:hlissner/doom-emacs/develop";
      flake = false;
    };
    # overlays
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "unstable";
    };
    neovim-overlay = {
      url = "github:nix-community/neovim-nightly-overlay";
      inputs.nixpkgs.follows = "unstable";
    };
    alacritty-src = {
      url = "github:zenixls2/alacritty/ligature";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, darwin, home-manager, ... }@inputs: {
    darwinConfigurations."shaunsingh-laptop" = darwin.lib.darwinSystem {
      system = "aarch64-darwin";
      modules = [
        ./modules/mac.nix
        ./modules/home.nix
        ./modules/pam.nix
        ./modules/editors.nix
        home-manager.darwinModule
        {
          home-manager = {
            useGlobalPkgs = true;
            useUserPackages = true;
          };
        }
        ({ pkgs, lib, ... }: {
          services.nix-daemon.enable = true;
          security.pam.enableSudoTouchIdAuth = true;
          nixpkgs = {
            config.allowUnfree = true;
            overlays = with inputs; [
              nur.overlay
              spacebar.overlay
              neovim-overlay.overlay
              rust-overlay.overlay
              (final: prev: {
                doomEmacsRevision = inputs.doom-emacs.rev;
                sf-mono-liga-bin = pkgs.callPackage ./pkgs/sf-mono-liga-bin { };
                yabai = let
                  version = "4.0.0-dev";
                  buildSymlinks = prev.runCommand "build-symlinks" { } ''
                    mkdir -p $out/bin
                    ln -s /usr/bin/xcrun /usr/bin/xcodebuild /usr/bin/tiffutil /usr/bin/qlmanage $out/bin
                  '';
                in prev.yabai.overrideAttrs (old: {
                  inherit version;
                  src = inputs.yabai-src;

                  buildInputs = with prev.darwin.apple_sdk.frameworks; [
                    Carbon
                    Cocoa
                    ScriptingBridge
                    prev.xxd
                    SkyLight
                  ];

                  nativeBuildInputs = [ buildSymlinks ];
                });
                alacritty-ligatures = with pkgs;
                  (alacritty.overrideAttrs (old: rec {
                    src = inputs.alacritty-src;
                    cargoDeps = old.cargoDeps.overrideAttrs (_: {
                      inherit src;
                      outputHash =
                        "sha256-tY5sle1YUlUidJcq7RgTzkPsGLnWyG/3rtPqy2GklkY=";
                    });
                  }));
                emacs = (prev.emacs.override {
                  srcRepo = true;
                  nativeComp = true;
                  withSQLite3 = true;
                  withXwidgets = true;
                }).overrideAttrs (o: rec {
                  version = "29.0.50";
                  src = inputs.emacs-src;

                  buildInputs = o.buildInputs
                    ++ [ prev.darwin.apple_sdk.frameworks.WebKit pkgs.cairo pkgs.harfbuzz];

                  configureFlags = o.configureFlags
                    ++ [ "--with-cairo" "--with-harfbuzz" ];

                  patches = [
                    ./patches/fix-window-role.patch
                    # ./patches/fix-harfbuzz-check.patch
                    # ./patches/no-titlebar.patch
                  ];

                  postPatch = o.postPatch + ''
                    substituteInPlace lisp/loadup.el \
                    --replace '(emacs-repository-get-branch)' '"master"'
                  '';

                  CFLAGS =
                    "-DMAC_OS_X_VERSION_MAX_ALLOWED=110203 -g -O3 -mtune=native -march=native -fomit-frame-pointer";
                });
              })
            ];
          };
          nix = {
            package = pkgs.nix;
            extraOptions = ''
              system = aarch64-darwin
              extra-platforms = aarch64-darwin x86_64-darwin
              experimental-features = nix-command flakes
              build-users-group = nixbld
            '';
          };
          environment.systemPackages = with pkgs; [
            # emacs needs to be here since its a GUI app
            emacs

            # Build Tools
            jdk
            rust-bin.nightly.latest.default

            # Language Servers
            nodePackages.pyright
            rust-analyzer

            # Formatting
            nixfmt
            black
            shellcheck

            # Terminal utils and rust alternatives :tm:
            xcp
            lsd
            procs
            tree
            zoxide
            bottom
          ];
          fonts = {
            enableFontDir = true;
            fonts = with pkgs; [
              overpass
              fira
              emacs-all-the-icons-fonts
              sf-mono-liga-bin
            ];
          };
        })
      ];
    };
  };
}
