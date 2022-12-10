{
  description = "Shaurya's Nix Environment";

  inputs = {
    # All packages should follow latest nixpkgs/nur
    # unstable.url = "github:nixos/nixpkgs/master";
    unstable.url = "github:shaunsingh/nixpkgs/master";
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
    # SRC for macOS emacs overlay
    emacs-src = {
      url = "github:emacs-mirror/emacs";
      flake = false;
    };
    # Use latest libverm to build macOS emacs build
    emacs-vterm-src = {
      url = "github:akermu/emacs-libvterm";
      flake = false;
    };
    # Bar
    sketchybar-src = {
      url = "github:FelixKratz/SketchyBar";
      flake = false;
    };
    # Window Management
    yabai-src = {
      url = "github:FelixKratz/yabai";
      flake = false;
    };
    # SFMono w/ patches
    sf-mono-liga-src = {
      url = "github:shaunsingh/SFMono-Nerd-Font-Ligaturized";
      flake = false;
    };
    # Terminal emulator
    alacritty-src = {
      url = "github:fee1-dead/alacritty/ligatures_harfbuzz";
      flake = false;
    };
    wezterm-src = {
      type = "git";
      url = "https://github.com/wez/wezterm.git";
      ref = "main";
      submodules = true;
      flake = false;
    };
    crane = {
      url = "github:ipetkov/crane";
      inputs.nixpkgs.follows = "unstable";
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
    # Rust Nightly
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "unstable";
    };
    # Firefox for darwin
    firefox-overlay = {
      url = "github:bandithedoge/nixpkgs-firefox-darwin";
      inputs.nixpkgs.follows = "unstable";
    };
  };
  outputs = { self, nixpkgs, darwin, home-manager, ... }@inputs: {
    darwinConfigurations."shaunsingh-laptop" = darwin.lib.darwinSystem {
      system = "aarch64-darwin";
      modules = [
        ./modules/mac.nix
        ./modules/sketchybar
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
          nix.registry.nixpkgs.flake = inputs.unstable;
          services.nix-daemon.enable = true;
          security.pam.enableSudoTouchIdAuth = true;
          nixpkgs = {
            config = {
              allowUnfree = true;
              # replaceStdenv = { pkgs }: pkgs.nativeMoldyClangStdenv;
            };
            overlays = let
              versionOf = input: input.rev;
              badstdenv =
                (import pkgs.path { system = "aarch64-darwin"; }).stdenv;
              safeFlags = [ "-O3" "-pipe" ];
            in with inputs; [
              nur.overlay
              neovim-overlay.overlay
              rust-overlay.overlays.default
              firefox-overlay.overlay
              (final: prev: {
                # Generates an stdenv based on clang v15 with mold-macOS as a linker
                # Packages are built with pipe & native optimizations along with -O3
                nativeMoldyClangStdenv =
                  prev.stdenvAdapters.impureUseNativeOptimizations
                  (prev.stdenvAdapters.withCFlags safeFlags
                    (prev.overrideCC prev.llvmPackages_15.stdenv
                      (prev.wrapCCWith rec {
                        cc = prev.llvmPackages_15.clang-unwrapped;
                        bintools = (final.wrapBintoolsWith {
                          bintools = final.binutils-unwrapped.overrideAttrs
                            (old: {
                              postInstall = ''
                                ln -sf ${final.mold}/bin/ld64.mold $out/bin/ld
                              '';
                            });
                        });
                      })));
                # stuff that doesn't like clang. Build against the default macOS stdenv
                # pkg-config-unwrapped = prev.pkg-config-unwrapped.override { stdenv = badstdenv; };
                # perl = prev.perl.override { stdenv = badstdenv; };
                # readline = prev.readline.override { stdenv = badstdenv; };
                # gdbm = prev.gdbm.override { stdenv = badstdenv; };
                # tzdata = prev.tzdata.override { stdenv = badstdenv; };
                # cpio = prev.cpio.override { stdenv = badstdenv; };
                # file = prev.cpio.override { stdenv = badstdenv; };

                # tests fail
                # dejagnu = prev.dejagnu.overrideAttrs (old: rec { doCheck = false; });
                # overlays 
                sf-mono-liga-bin = prev.stdenvNoCC.mkDerivation rec {
                  pname = "sf-mono-liga-bin";
                  version = versionOf inputs.sf-mono-liga-src;
                  src = inputs.sf-mono-liga-src;
                  dontConfigure = true;
                  installPhase = ''
                    mkdir -p $out/share/fonts/opentype
                    cp -R $src/*.otf $out/share/fonts/opentype/
                  '';
                };
                sketchybar-git = prev.sketchybar.overrideAttrs (old: rec {
                  version = versionOf inputs.sketchybar-src;
                  src = inputs.sketchybar-src;
                });
                alacritty-ligatures = prev.alacritty.overrideAttrs (old: rec {
                  version = versionOf inputs.alacritty-src;
                  src = inputs.alacritty-src;
                  doCheck = false;
                  cargoDeps = old.cargoDeps.overrideAttrs (_: {
                    inherit src;
                    outputHash =
                      "sha256-qRvPBuDJ5K7II1LXOpTINs35XvKALOFQa4h5PPIMZic=";
                  });
                });
                yabai-git = prev.stdenv.mkDerivation rec {
                  pname = "yabai";
                  version = versionOf inputs.yabai-src;
                  src = inputs.yabai-src;

                  nativeBuildInputs = with prev; [ installShellFiles ];

                  buildInputs = with prev.darwin.apple_sdk_11_0.frameworks; [
                    Carbon
                    Cocoa
                    ScriptingBridge
                    SkyLight
                    DisplayServices
                  ];

                  dontConfigure = true;
                  enableParallelBuilding = true;

                  postPatch = ''
                    # remove reference to x86-only call
                    substituteInPlace makefile \
                      --replace "-arch x86_64" ""
                    # `NSScreen::safeAreaInsets` is only available on macOS 12.0 and above, which frameworks arent packaged.
                    # When a lower OS version is detected upstream just returns 0, so we can hardcode that at compiletime.
                    # https://github.com/koekeishiya/yabai/blob/v4.0.2/src/workspace.m#L109
                    substituteInPlace src/workspace.m \
                      --replace 'return screen.safeAreaInsets.top;' 'return 0;'
                  '';
                  buildPhase = ''
                    # use xcode's clang, as arm64's pointer authentication support isn't upstreamed to nix's llvm yet
                    # https://reviews.llvm.org/D112941 
                    PATH=/usr/bin:/bin /usr/bin/make install
                  '';
                  installPhase = ''
                    runHook preInstall
                    mkdir -p $out/{bin,share/icons/hicolor/scalable/apps}
                    cp ./bin/yabai $out/bin/yabai
                    cp ./assets/icon/icon.svg $out/share/icons/hicolor/scalable/apps/yabai.svg
                    installManPage ./doc/yabai.1
                    runHook postInstall
                  '';
                };
                wezterm-git =
                  prev.darwin.apple_sdk_11_0.callPackage ./pkgs/wezterm {
                    inherit (prev.darwin.apple_sdk_11_0.frameworks)
                      Cocoa CoreGraphics Foundation UserNotifications;
                    src = inputs.wezterm-src;
                    version = versionOf inputs.wezterm-src;
                    crane-lib = inputs.crane.lib."${prev.system}";
                  };
                webkitgtk = prev.stdenv.mkDerivation rec {
                  pname = "webkitgtk";
                  version = "2.32.1";

                  src = prev.fetchurl {
                    url =
                      "https://webkitgtk.org/releases/${pname}-${version}.tar.xz";
                    sha256 =
                      "05v9hgpkc6mi2klrd8nqql1n8xzq8rgdz3hvyy369xkhgwqifq8k";
                  };
                  patches = [
                    (prev.fetchpatch {
                      url =
                        "https://github.com/WebKit/WebKit/commit/94cdcd289b993ed4d39c17d4b8b90db7c81a9b10.diff";
                      sha256 =
                        "sha256-ywrTEjf3ATqI0Vvs60TeAZ+m58kCibum4DamRWrQfaA=";
                      excludes = [ "Source/WebKit/ChangeLog" ];
                    })
                    (prev.fetchpatch {
                      url =
                        "https://bug-225856-attachments.webkit.org/attachment.cgi?id=428797";
                      sha256 =
                        "sha256-ffo5p2EyyjXe3DxdrvAcDKqxwnoqHtYBtWod+1fOjMU=";
                      excludes = [ "Source/WebCore/ChangeLog" ];
                    })
                    ./patches/428774.patch # https://bug-225850-attachments.webkit.org/attachment.cgi?id=428774
                    (prev.fetchpatch {
                      url =
                        "https://bug-225850-attachments.webkit.org/attachment.cgi?id=428776";
                      sha256 =
                        "sha256-ryNRYMsk72SL0lNdh6eaAdDV3OT8KEqVq1H0j581jmQ=";
                      excludes = [ "Source/WTF/ChangeLog" ];
                    })
                    (prev.fetchpatch {
                      url =
                        "https://bug-225850-attachments.webkit.org/attachment.cgi?id=428778";
                      sha256 =
                        "sha256-78iP+T2vaIufO8TmIPO/tNDgmBgzlDzalklrOPrtUeo=";
                      excludes = [ "Source/WebKit/ChangeLog" ];
                    })
                    (prev.fetchpatch {
                      url =
                        "https://bug-227360-attachments.webkit.org/attachment.cgi?id=432180";
                      sha256 =
                        "sha256-1JLJKu0G1hRTzqcHsZgbXIp9ZekwbYFWg/MtwB4jTjc=";
                      excludes = [ "Source/WebKit/ChangeLog" ];
                    })
                  ];

                  outputs = [ "out" "dev" ];

                  preConfigure = lib.optionalString
                    (prev.stdenv.hostPlatform != prev.stdenv.buildPlatform) ''
                      # Ignore gettext in cmake_prefix_path so that find_program doesn't
                      # pick up the wrong gettext. TODO: Find a better solution for
                      # this, maybe make cmake not look up executables in
                      # CMAKE_PREFIX_PATH.
                      cmakeFlags+=" -DCMAKE_IGNORE_PATH=${
                        lib.getBin gettext
                      }/bin"
                    '';

                  nativeBuildInputs = with prev; [
                    bison
                    cmake
                    gettext
                    gobject-introspection
                    gperf
                    ninja
                    perl
                    perl.pkgs.FileCopyRecursive # used by copy-user-interface-resources.pl
                    pkg-config
                    python3
                    ruby
                    glib # for gdbus-codegen
                    wrapGAppsHook # for MiniBrowser
                  ];

                  buildInputs = with prev; [
                    at-spi2-core
                    enchant2
                    libepoxy
                    gnutls
                    gst_all_1.gst-plugins-bad
                    gst_all_1.gst-plugins-base
                    (harfbuzz.override { withIcu = true; })
                    libGL
                    libGLU
                    mesa # for libEGL headers
                    libgcrypt
                    libgpg-error
                    libidn
                    libintl
                    lcms2
                    libtasn1
                    libwebp
                    libxkbcommon
                    libxml2
                    libxslt
                    libnotify
                    nettle
                    openjpeg
                    p11-kit
                    pcre
                    sqlite
                    woff2
                    libedit
                    readline
                    glib-networking # for MiniBrowser
                    geoclue2
                    libsecret
                  ];

                  propagatedBuildInputs = with prev; [ gtk3 libsoup ];

                  cmakeFlags = [
                    "-DENABLE_INTROSPECTION=ON"
                    "-DPORT=GTK"
                    "-DUSE_SYSTEMD=OFF"
                    "-DUSE_LIBHYPHEN=OFF"
                    "-DENABLE_MINIBROWSER=ON"
                    "-DLIBEXEC_INSTALL_DIR=${
                      placeholder "out"
                    }/libexec/webkit2gtk"
                    # "-DUSE_SOUP2"
                    "-DUSE_LIBSECRET=ON"
                    "-DENABLE_GAMEPAD=OFF"
                    "-DENABLE_JOURNALD_LOG=OFF"
                    "-DENABLE_QUARTZ_TARGET=ON"
                    "-DENABLE_X11_TARGET=OFF"
                    "-DUSE_APPLE_ICU=OFF"
                    "-DUSE_OPENGL_OR_ES=OFF"
                    "-DENABLE_GTKDOC=OFF"
                    "-DENABLE_VIDEO=ON"
                  ];

                  postPatch = ''
                    patchShebangs .
                    # It needs malloc_good_size.
                    sed 22i'#include <malloc/malloc.h>' -i Source/WTF/wtf/FastMalloc.h
                    # <CommonCrypto/CommonRandom.h> needs CCCryptorStatus.
                    sed 43i'#include <CommonCrypto/CommonCryptor.h>' -i Source/WTF/wtf/RandomDevice.cpp 
                  '';

                  postFixup = ''
                    # needed for TLS and multimedia functionality
                    wrapGApp $out/libexec/webkit2gtk/MiniBrowser --argv0 MiniBrowser
                  '';

                  # we only want to wrap the MiniBrowser
                  dontWrapGApps = true;
                  requiredSystemFeatures = [ "big-parallel" ];
                };
                emacs-vterm = prev.stdenv.mkDerivation rec {
                  pname = "emacs-vterm";
                  version = versionOf inputs.emacs-vterm-src;
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
                  withGTK3 = true;
                  withXwidgets = true;
                }).overrideAttrs (o: rec {
                  version = "29.0.50";
                  src = inputs.emacs-src;

                  # needed for xwidgets
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
              })
            ];
          };
        })
      ];
    };
  };
}
