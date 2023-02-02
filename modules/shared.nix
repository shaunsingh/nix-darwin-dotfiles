# TODO piece this out too, why do i have all of my overlays here?
{ pkgs, lib, inputs, ... }: {
  time.timeZone = "America/New_York";
  networking.hostName = "shaunsingh-laptop";
  nix = {
    package = pkgs.nix;
    registry.nixpkgs.flake = inputs.nixpkgs;
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
      tarball-ttl = 0;
      contentAddressedByDefault = false;
      # replaceStdenv = { pkgs }: pkgs.nativeStdenv;
    };
    overlays =
      let
        versionOf = input: input.rev;
        badstdenv = (import pkgs.path { system = "aarch64-linux"; }).stdenv;
      in
      with inputs; [
        nur.overlay
        prismmc.overlay
        firefox-overlay.overlay
        nixpkgs-wayland.overlay
        rust-overlay.overlays.default
        nixos-apple-silicon.overlays.default
        (final: prev: {
          nativeStdenv = 
            prev.stdenvAdapters.withCFlags [ 
              "-Ofast" 
              "-pipe" 
              "-mcpu=apple-m1" 
              # "-fuse-ld=${final.mold}/bin/mold" 
            ] final.llvmPackages_latest.stdenv;

          # stuff that doesn't like clang (wants gcc/g++)
          # boost-build = prev.boost-build.override { stdenv = badstdenv; };
          # jq = prev.jq.override { stdenv = badstdenv; };

          # stuff that doesn't like clang (tests fail)
          # nghttp2 = prev.nghttp2.overrideAttrs (old: rec { doCheck = false; });

# nativeStdenv = prev.stdenvAdapters.withCFlags [ 
#     "-Ofast" 
#     "-pipe" 
#     "-mcpu=apple-m1" 
#     "-fuse-ld=${final.mold}/bin/mold" 
#     ]
#   (prev.overrideCC final.llvmPackages_latest.stdenv
#     (prev.wrapCCWith {
#       cc = final.llvmPackages_latest.clang-unwrapped;
#       bintools = (final.wrapBintoolsWith {
#         coreutils = final.uutils-coreutils;
#         libc = final.glibc;
#         # build bintools without ld linked
#         bintools = prev.llvmPackages_latest.bintools.overrideAttrs (o: {
#           postInstall = "ln -sf ${prev.mold}/bin/mold $out/bin/ld";
#         });
#       });
#     }));

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
          # neovim = inputs.neovim-nightly.packages.${pkgs.system}.neovim;
          sf-mono-liga-bin = prev.stdenvNoCC.mkDerivation {
            pname = "sf-mono-liga-bin";
            version = versionOf inputs.sf-mono-liga-src;
            src = inputs.sf-mono-liga-src;
            dontConfigure = true;
            installPhase = ''
              mkdir -p $out/share/fonts/opentype
              cp -R $src/*.otf $out/share/fonts/opentype/
            '';
          };
          otf-apple = prev.stdenvNoCC.mkDerivation {
            name = "otf-apple";
            version = "1.0";
            buildInputs = [ prev.p7zip ];
            src = [
              inputs.sf-pro-src
              inputs.sf-compact-src
              inputs.ny-src
            ];
            sourceRoot = "./";
            preUnpack = "mkdir fonts";
            unpackCmd = ''
              7z x $curSrc >/dev/null
              dir="$(find . -not \( -path ./fonts -prune \) -type d | sed -n 2p)"
              cd $dir 2>/dev/null
              7z x *.pkg >/dev/null
              7z x Payload~ >/dev/null
              mv Library/Fonts/*.otf ../fonts/
              cd ../
              rm -R $dir
            '';
          
            installPhase = ''
              mkdir -p $out/share/fonts/opentype/{SF\ Pro,SF\ Compact,New\ York}
              cp -a fonts/SF-Pro*.otf $out/share/fonts/opentype/SF\ Pro
              cp -a fonts/SF-Compact*.otf $out/share/fonts/opentype/SF\ Compact
              cp -a fonts/NewYork*.otf $out/share/fonts/opentype/New\ York
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
          eww = inputs.eww.packages.${pkgs.system}.eww-wayland;
          swayfx-unwrapped = inputs.swayfx.packages.${pkgs.system}.swayfx-unwrapped;
          alsa-ucm-conf = prev.alsa-ucm-conf.overrideAttrs (old: rec {
            installPhase = ''
              runHook preInstall
              mkdir -p $out/share/alsa
              cp -r ucm ucm2 $out/share/alsa
              cp -r ${inputs.asahi-alsa-src}/ucm2/conf.d/macaudio $out/share/alsa/ucm2/conf.d/macaudio
              runHook postInstall
            '';
          });
          phocus-oxocarbon = prev.stdenvNoCC.mkDerivation rec {
            pname = "phocus-oxocarbon";
            version = versionOf inputs.phocus-src;
            src = inputs.phocus-src;
          
            patches = [
              ../patches/remove-npm.diff
              ../patches/gradient.diff
              ../patches/accent-substitute-all.diff
            ];
          
            postPatch = with colors; ''
              substituteInPlace scss/gtk-3.0/_colors.scss \
                --replace "@bg0@" "#161616" \
                --replace "@bg1@" "#262626" \
                --replace "@bg2@" "#393939"\
                --replace "@bg3@" "#424242" \
                --replace "@bg4@" "#525252" \
                --replace "@red@" "#ff7eb6" \
                --replace "@lred@" "#ff7eb6" \
                --replace "@orange@" "#ee5396" \
                --replace "@lorange@" "#ee5396" \
                --replace "@yellow@" "#33b1ff" \
                --replace "@lyellow@" "#33b1ff" \
                --replace "@green@" "#42be65" \
                --replace "@lgreen@" "#42be65" \
                --replace "@cyan@" "#3ddbd9" \
                --replace "@lcyan@" "#3ddbd9" \
                --replace "@blue@" "#08bdba" \
                --replace "@lblue@" "#08bdba" \
                --replace "@purple@" "#be95ff" \
                --replace "@lpurple@" "#be95ff" \
                --replace "@pink@" "#ff7eb6" \
                --replace "@lpink@" "#ff7eb6" \
                --replace "@primary@" "#f2f4f8" \
                --replace "@secondary@" "#dde1e6"
            '';

            nativeBuildInputs = [ prev.nodePackages.sass ];
            installFlags = [ "DESTDIR=$(out)" "PREFIX=" ];
          };
          screenshot = pkgs.writeShellScriptBin "screenshot" ''
            ${pkgs.grim}/bin/grim -g "$(${pkgs.slurp}/bin/slurp)" - -t png | ${pkgs.wl-clipboard}/bin/wl-copy -t image/png
          '';
          ocrScript =
            let
              inherit (pkgs) grim libnotify slurp tesseract5 wl-clipboard;
              _ = lib.getExe;
            in
            pkgs.writeShellScriptBin "wl-ocr" ''
              ${_ grim} -g "$(${_ slurp})" -t ppm - | ${_ tesseract5} - - | ${wl-clipboard}/bin/wl-copy
              ${_ libnotify} "$(${wl-clipboard}/bin/wl-paste)"
            '';
          volume = pkgs.writeShellScriptBin "volume" ''
            #!/bin/sh
        
            ${pkgs.pamixer}/bin/pamixer "$@"
            volume="$(${pkgs.pamixer}/bin/pamixer --get-volume-human)"
        
            if [ "$volume" = "muted" ]; then
                ${pkgs.libnotify}/bin/notify-send -r 69 \
                    -a "Volume" \
                    "Muted" \
                    -t 888 \
                    -u low
            else
                ${pkgs.libnotify}/bin/notify-send -r 69 \
                    -a "Volume" "Currently at $volume" \
                    -h int:value:"$volume" \
                    -t 888 \
                    -u low
            fi
          '';
          microphone = pkgs.writeShellScriptBin "microphone" ''
            #!/bin/sh
        
            ${pkgs.pamixer}/bin/pamixer --default-source "$@"
            mic="$(${pkgs.pamixer}/bin/pamixer --default-source --get-volume-human)"
        
            if [ "$mic" = "muted" ]; then
                ${pkgs.libnotify}/bin/notify-send -r 69 \
                    -a "Microphone" \
                    "Muted" \
                    -t 888 \
                    -u low
            else
              ${pkgs.libnotify}/bin/notify-send -r 69 \
                    -a "Microphone" "Currently at $mic" \
                    -h int:value:"$mic" \
                    -t 888 \
                    -u low
            fi
          '';
          brightness =
            let
              brightnessctl = pkgs.brightnessctl + "/bin/brightnessctl";
            in
            pkgs.writeShellScriptBin "brightness" ''
              #!/bin/sh
        
              ${brightnessctl} "$@"
              brightness=$(echo $(($(${brightnessctl} g) * 100 / $(${brightnessctl} m))))
        
              ${pkgs.libnotify}/bin/notify-send -r 69 \
                  -a "Brightness" "Currently at $brightness%" \
                  -h int:value:"$brightness" \
                  -t 888 \
                  -u low
            '';


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
