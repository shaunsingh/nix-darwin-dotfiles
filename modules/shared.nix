{ pkgs, lib, inputs, ... }: {
  time.timeZone = "America/New_York";
  networking.hostName = "shaunsingh-laptop";
  nix = {
    package = pkgs.nix;
    registry.nixpkgs.flake = inputs.unstable;
    extraOptions = ''
      warn-dirty = false
      experimental-features = nix-command flakes
      build-users-group = nixbld
      sandbox = true
    '';
  };
  nixpkgs = {
    config = {
      allowUnfree = true;
      # replaceStdenv = { pkgs }: pkgs.nativeMoldyRustyClangStdenv;
    };
    overlays =
      let
        versionOf = input: input.rev;
        # badstdenv = (import pkgs.path { system = "aarch64-darwin"; }).stdenv;
      in
      with inputs; [
        nur.overlay
        neovim-overlay.overlay
        nixpkgs-wayland.overlay
        rust-overlay.overlays.default
        firefox-overlay.overlay
        (final: prev: {
          # Generates an stdenv based on clang v15 with mold-macOS as a linker
          # Packages are built with pipe & native optimizations along with -O3
          # nativeMoldyRustyClangStdenv =
          #   prev.stdenvAdapters.impureUseNativeOptimizations
          #     (prev.stdenvAdapters.withCFlags [ "-O3" "-pipe" ]
          #       (prev.overrideCC prev.llvmPackages_15.stdenv
          #         (prev.wrapCCWith rec {
          #           cc = prev.llvmPackages_15.clang-unwrapped;
          #           bintools = (final.wrapBintoolsWith {
          #             bintools = final.binutils-unwrapped.overrideAttrs
          #               (old: {
          #                 postInstall = ''
          #                   ln -sf ${final.mold}/bin/ld64.mold $out/bin/ld
          #                 '';
          #               });
          #           });
          #         })));
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

          # shared overlays
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
          alacritty-ligatures = prev.alacritty.overrideAttrs (old: rec {
            version = versionOf inputs.alacritty-src;
            src = inputs.alacritty-src;
            doCheck = false;
            cargoDeps = old.cargoDeps.overrideAttrs (_: {
              inherit src;
              outputHash = "sha256-qRvPBuDJ5K7II1LXOpTINs35XvKALOFQa4h5PPIMZic=";
            });
          });

          # linux overlays
          asahi-mesa = (prev.mesa.override {
            galliumDrivers = [ "swrast" "asahi" ];
            vulkanDrivers = [ "swrast" ];
            enableGalliumNine = false;
          }).overrideAttrs (old: {
            version = versionOf inputs.asahi-mesa-src;
            src = inputs.asahi-mesa-src;
            mesonFlags = lib.filter (x: !(lib.hasPrefix "-Dxvmc-libs-path=" x)) oldAttrs.mesonFlags;
          });

          # darwin overlays 
          julia-bin =
            if prev.stdenv.hostPlatform.isDarwin then
              final.callPackage ../pkgs/julia-18 { }
            else
              prev.julia-bin;
          sketchybar-git = prev.sketchybar.overrideAttrs (old: rec {
            version = versionOf inputs.sketchybar-src;
            src = inputs.sketchybar-src;
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
        })
      ];
  };
}
