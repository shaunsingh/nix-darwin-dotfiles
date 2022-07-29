{
  description = "Shaurya's Nix Environment";

  inputs = {
    # All packages should follow latest nixpkgs/nur
    unstable.url = "github:nixos/nixpkgs/master";
    nur.url = "github:nix-community/NUR";
    # Nix-Darwin
    darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "unstable";
    };
    # HM-manager for dotfile/user management
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "unstable";
    };
    # Bar (macos)
    spacebar = {
      url = "github:shaunsingh/spacebar";
      inputs.nixpkgs.follows = "unstable";
    };
    # WM
    yabai-src = {
      url = "github:koekeishiya/yabai";
      flake = false;
    };
    # SRC for macOS emacs overlay
    emacs-src = {
      url = "github:emacs-mirror/emacs";
      flake = false;
    };
    # New project seems interesting
    emacs-commercial-src = {
      url = "github:commercial-emacs/commercial-emacs";
      flake = false;
    };
    # Use latest libverm to build macOS emacs build
    emacs-vterm-src = {
      url = "github:akermu/emacs-libvterm";
      flake = false;
    };
    # Themeing
    base16 = {
      url = "github:shaunsingh/base16.nix";
      inputs.nixpkgs.follows = "unstable";
    };
    # IBM-Carbon-Theme (see IBM-design: colors)
    base16-carbon-dark = {
      url = "github:shaunsingh/base16-carbon-dark";
      flake = false;
    };
    # Neovim Nightly
    neovim-overlay = {
      url = "github:nix-community/neovim-nightly-overlay";
      inputs.nixpkgs.follows = "unstable";
    };
  };
  outputs = { self, nixpkgs, darwin, home-manager, ... }@inputs: {
    darwinConfigurations."shaunsingh-laptop" = darwin.lib.darwinSystem {
      system = "aarch64-darwin";
      modules = [
        ./modules/mac.nix
        ./modules/pam.nix
        home-manager.darwinModule
        {
          home-manager = {
            useGlobalPkgs = true;
            useUserPackages = true;
            extraSpecialArgs = {
              inherit (inputs)
                base16-carbon-dark; # pass base16 input so hm can use it
            };
            users.shauryasingh = {
              imports = [
                inputs.base16.hmModule
                ./modules/home.nix
                ./modules/emacs.nix
                ./modules/theme.nix
              ];
            };
          };
        }
        ({ config, pkgs, lib, ... }: {
          services.nix-daemon.enable = true;
          security.pam.enableSudoTouchIdAuth = true;
          nixpkgs = {
            overlays = with inputs; [
              nur.overlay
              spacebar.overlay
              neovim-overlay.overlay
              (final: prev: {
                sf-mono-liga-bin = pkgs.callPackage ./pkgs/sf-mono-liga-bin { };
                yabai =
                  let
                    version = "4.0.0-dev";
                    buildSymlinks = prev.runCommand "build-symlinks" { } ''
                      mkdir -p $out/bin
                      ln -s /usr/bin/xcrun /usr/bin/xcodebuild /usr/bin/tiffutil /usr/bin/qlmanage $out/bin
                    '';
                  in
                  prev.yabai.overrideAttrs (old: {
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
                emacs-vterm = prev.stdenv.mkDerivation rec {
                  pname = "emacs-vterm";
                  version = "master";

                  src = inputs.emacs-vterm-src;

                  nativeBuildInputs = [ prev.cmake prev.libtool prev.glib.dev ];

                  buildInputs =
                    [ prev.glib.out prev.libvterm-neovim prev.ncurses ];

                  cmakeFlags = [ "-DUSE_SYSTEM_LIBVTERM=yes" ];

                  preConfigure = ''
                    echo "include_directories(\"${prev.glib.out}/lib/glib-2.0/include\")" >> CMakeLists.txt
                    echo "include_directories(\"${prev.glib.dev}/include/glib-2.0\")" >> CMakeLists.txt
                    echo "include_directories(\"${prev.ncurses.dev}/include\")" >> CMakeLists.txt
                    echo "include_directories(\"${prev.libvterm-neovim}/include\")" >> CMakeLists.txt
                  '';

                  installPhase = ''
                    mkdir -p $out
                    cp ../vterm-module.so $out
                    cp ../vterm.el $out
                  '';

                };
                emacs-mac = (prev.emacs.override {
                  srcRepo = true;
                  nativeComp = true;
                  withSQLite3 = true;
                  withXwidgets = true;
                }).overrideAttrs (o: rec {
                  version = "29.0.50";
                  src = inputs.emacs-src;

                  buildInputs = o.buildInputs
                    ++ [ prev.darwin.apple_sdk.frameworks.WebKit ];

                  configureFlags = o.configureFlags ++ [
                    "--without-gpm"
                    "--without-dbus"
                    "--without-mailutils"
                    "--without-pop"
                  ];

                  patches = [
                    ./patches/fix-window-role.patch
                    ./patches/system-appearance.patch
                  ];

                  postPatch = o.postPatch + ''
                    substituteInPlace lisp/loadup.el \
                    --replace '(emacs-repository-get-branch)' '"master"'
                  '';

                  postInstall = o.postInstall + ''
                    cp ${final.emacs-vterm}/vterm.el $out/share/emacs/site-lisp/vterm.el
                    cp ${final.emacs-vterm}/vterm-module.so $out/share/emacs/site-lisp/vterm-module.so
                  '';

                  CFLAGS =
                    "-DMAC_OS_X_VERSION_MAX_ALLOWED=110203 -g -O3 -mtune=native -march=native -fomit-frame-pointer";
                });
                commercial-emacs = (prev.emacs.override {
                  srcRepo = true;
                  nativeComp = false;
                  withSQLite3 = true;
                  withXwidgets = true;
                  withToolkitScrollBars = false;
                }).overrideAttrs (o: rec {
                  version = "29.0.50-commercial";
                  src = inputs.emacs-commercial-src;

                  buildInputs = o.buildInputs
                    ++ [ prev.darwin.apple_sdk.frameworks.WebKit ];

                  postPatch = o.postPatch + ''
                    substituteInPlace lisp/loadup.el \
                    --replace '(emacs-repository-get-branch)' '"master"'
                  '';

                  postInstall = o.postInstall + ''
                    cp ${final.emacs-vterm}/vterm.el $out/share/emacs/site-lisp/vterm.el
                    cp ${final.emacs-vterm}/vterm-module.so $out/share/emacs/site-lisp/vterm-module.so
                  '';

                  CFLAGS =
                    "-DMAC_OS_X_VERSION_MAX_ALLOWED=110203 -g -O3 -mtune=native -march=native -fomit-frame-pointer";
                });
              })
            ];
          };
        })
      ];
    };
  };
}
