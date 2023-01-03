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
        readConfig = configfile: import (pkgs.runCommand "config.nix" { } ''
          echo "{ } // " > "$out"
          while IFS='=' read key val; do
            [ "x''${key#CONFIG_}" != "x$key" ] || continue
            no_firstquote="''${val#\"}";
            echo '{  "'"$key"'" = "'"''${no_firstquote%\"}"'"; } //' >> "$out"
          done < "${configfile}"
          echo "{ }" >> $out
        '').outPath;
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
          sway-git = prev.sway.overrideAttrs (old: {
            version = versionOf inputs.sway-src;
            src = inputs.sway-src;
          });
          nyxt-asdf = pkgs.lispPackages_new.build-asdf-system {
            pname = "nyxt-asdf";
            version = versionOf inputs.nyxt-src;
            src = inputs.nyxt-src;
            systems = [ "nyxt-asdf" ];
            lisp = pkgs.lispPackages_new.sbcl;
          };
          ndebug = pkgs.lispPackages_new.build-asdf-system {
            pname = "ndebug";
            version = versionOf inputs.ndebug-src;
            src = inputs.ndebug-src;
            lisp = pkgs.lispPackages_new.sbcl;
            lispLibs = with pkgs.lispPackages_new.sbclPackages; [
              dissect
              trivial-custom-debugger
              trivial-gray-streams
              bordeaux-threads
            ];
          };
          nsymbols = pkgs.lispPackages_new.build-asdf-system {
            pname = "nsymbols";
            version = versionOf inputs.nsymbols-src;
            src = inputs.nsymbols-src;
            lisp = pkgs.lispPackages_new.sbcl;
            lispLibs = with pkgs.lispPackages_new.sbclPackages; [ closer-mop ];
          };
          lisp-unit2 =
            prev.lispPackages_new.sbclPackages.lisp-unit2.overrideLispAttrs (_: {
              version = versionOf inputs.lisp-unit2-src;
              src = inputs.lisp-unit2-src;
            });
          hu_dot_dwim_dot_defclass-star =
            prev.lispPackages_new.sbclPackages.hu_dot_dwim_dot_defclass-star.overrideLispAttrs
              (_: {
                src = final.fetchFromGitHub {
                  owner = "hu-dwim";
                  repo = "hu.dwim.defclass-star";
                  rev = "2698bd93073f9ba27583351221a3a087fb595626";
                  sha256 = "0v6bj3xbcpz98bkv3a2skz2dh0p50mqaflgkfbrzx1dzbkl1630y";
                };
              });
          ospm = pkgs.lispPackages_new.build-asdf-system {
            version = versionOf inputs.ospm-src;
            src = inputs.ospm-src;
            pname = "ospm";
            lisp = pkgs.lispPackages_new.sbcl;
            lispLibs = with pkgs.lispPackages_new.sbclPackages;
              [
                alexandria
                calispel
                local-time
                moptilities
                named-readtables
                osicat
                serapeum
                trivia
              ] ++ [ pkgs.hu_dot_dwim_dot_defclass-star ];
          };
          nyxt-3 = pkgs.lispPackages_new.build-asdf-system {
            pname = "nyxt";
            version = versionOf inputs.nyxt-src;
            src = inputs.nyxt-src;
            lisp = pkgs.lispPackages_new.sbcl;
            systems = [
              "nyxt"
              "nyxt/history-tree"
              "nyxt/class-star"
              "nyxt/prompter"
              "nyxt/tests"
              "nyxt/history-tree/tests"
              "nyxt/class-star/tests"
              "nyxt/prompter/tests"
            ];
            lispLibs = with pkgs.lispPackages_new.sbclPackages;
              [
                alexandria
                bordeaux-threads
                calispel
                cl-base64
                cl-containers
                cl-css
                cl-custom-hash-table
                enchant
                cl-gopher
                cl-html-diff
                cl-json
                cl-ppcre
                cl-ppcre-unicode
                cl-prevalence
                cl-qrencode
                str
                cl-tld
                closer-mop
                clss
                cluffer
                dexador
                flexi-streams
                iolib
                lass
                local-time
                log4cl
                lparallel
                montezuma
                moptilities
                named-readtables
                nfiles
                nhooks
                nkeymaps
                parenscript
                phos
                plump
                quri
                serapeum
                swank
                spinneret
                trivia
                trivial-clipboard
                trivial-features
                trivial-garbage
                trivial-package-local-nicknames
                trivial-types
                uiop
                unix-opts
                cl-cffi-gtk
                cl-webkit2
                mk-string-metrics
                dissect
                py-configparser
                slynk
              ] ++ (with pkgs; [
                hu_dot_dwim_dot_defclass-star
                nyxt-asdf
                ndebug
                ospm
                lisp-unit2
                nsymbols
              ]);
          };

          # asahi overlays
          asahi-edge-kernel = (prev.linuxKernel.manualConfig
            rec {
              version = versionOf inputs.linux-asahi-src;
              src = inputs.linux-asahi-src;
              modDirVersion = version;

              kernelPatches = [
                # patch the kernel to set the default size to 16k instead of modifying
                # the config so we don't need to convert our config to the nixos
                # infrastructure or patch it and thus introduce a dependency on the host
                # system architecture
                {
                  name = "default-pagesize-16k";
                  patch = ./asahi/default-pagesize-16k.patch;
                }
                {
                  name = "edge-config";
                  patch = null;
                  # derived from
                  # https://github.com/AsahiLinux/PKGBUILDs/blob/stable/linux-asahi/config.edge
                  extraConfig = ''
                    DRM_SIMPLEDRM_BACKLIGHT=n
                    BACKLIGHT_GPIO=n
                    DRM_APPLE=m
                    APPLE_SMC=m
                    APPLE_SMC_RTKIT=m
                    APPLE_RTKIT=m
                    APPLE_MAILBOX=m
                    GPIO_MACSMC=m
                    LOCALVERSION="-edge-ARCH"
                    DRM_VGEM=n
                    DRM_SCHED=y
                    DRM_GEM_SHMEM_HELPER=y
                    DRM_ASAHI=m
                    SUSPEND=y
                  '';
                }
              ];

              configfile = ./asahi/config;
              config = readConfig ./asahi/config;
              extraMeta.branch = "6.1";

            }).overrideAttrs (old: {
            # add rust support
            nativeBuildInputs = (old.nativeBuildInputs or [ ])
              ++ [ pkgs.rust-bindgen pkgs.rustfmt pkgs.rustPlatform.rust.rustc ];
            RUST_LIB_SRC = rustPlatform.rustLibSrc;
          });
          asahi-edge = pkgs.recurseIntoAttrs (pkgs.linuxPackagesFor pkgs.asahi-edge-kernel);
          m1n1 = prev.stdenv.mkDerivation rec {
            pname = "m1n1";
            version = versionOf inputs.m1n1-src;
            src = inputs.m1n1-src;

            makeFlags = [ "ARCH=aarch64-unknown-linux-gnu-" "RELEASE=1" ];

            nativeBuildInputs = [ pkgs.dtc pkgs.gcc ];

            postPatch = ''
              substituteInPlace proxyclient/m1n1/asm.py \
                --replace 'aarch64-linux-gnu-' 'aarch64-unknown-linux-gnu-' \
                --replace 'TOOLCHAIN = ""' 'TOOLCHAIN = "'$out'/toolchain-bin/"'
            '';

            installPhase = ''
              runHook preInstall
              mkdir -p $out/build
              cp build/m1n1.bin $out/build
              runHook postInstall
            '';
          };
          asahi-u-boot = (prev.pkgs.buildUBoot rec {
            version = versionOf inputs.uboot-src;
            src = inputs.uboot-src;

            defconfig = "apple_m1_defconfig";
            extraMeta.platforms = [ "aarch64-linux" ];
            filesToInstall = [ "u-boot-nodtb.bin.gz" "m1n1-u-boot.bin" ];
            extraConfig = ''
              CONFIG_IDENT_STRING=" ${version}"
              CONFIG_VIDEO_FONT_4X6=n
              CONFIG_VIDEO_FONT_8X16=n
              CONFIG_VIDEO_FONT_SUN12X22=n
              CONFIG_VIDEO_FONT_TER12X24=n
              CONFIG_VIDEO_FONT_TER16X32=y
            '';
          }).overrideAttrs (o: {
            # nixos's downstream patches are not applicable
            # however, we add in bigger u-boot fonts because the mac laptop screens are high-res
            patches = [
              (fetchpatch {
                url =
                  "https://git.alpinelinux.org/aports/plain/testing/u-boot-asahi/apritzel-first5-video.patch?id=990110f35b50b74bdb4e902d94fa15b07a8eac9e";
                sha256 = "sha256-QPvJYxIcQBHbwsj7l96qGUZSipk1sB3ZyniD1Io18dY=";
                revert = false;
              })

              (fetchpatch {
                url =
                  "https://git.alpinelinux.org/aports/plain/testing/u-boot-asahi/mps-u-boot-ter12x24.patch?id=990110f35b50b74bdb4e902d94fa15b07a8eac9e";
                sha256 = "sha256-wrQpIYiuNRi/p2p290KCGPmuRxFEOPlbICoFvd+E8p0=";
                revert = false;
              })
            ];

            preInstall = ''
              # compress so that m1n1 knows U-Boot's size and can find things after it
              gzip -n u-boot-nodtb.bin
              cat ${pkgs.m1n1}/build/m1n1.bin arch/arm/dts/t[68]*.dtb u-boot-nodtb.bin.gz > m1n1-u-boot.bin
            '';
          });
          asahi-fwextract = prev.python3.pkgs.buildPythonApplication rec {
            pname = "asahi-fwextract";
            version = versionOf inputs.asahi-fwextract-src;
            src = inputs.asahi-fwextract-src;
            patches = [ ../patches/add_entry_point.patch ];
            postPatch = ''
              substituteInPlace asahi_firmware/img4.py \
                --replace 'liblzfse.so' '${lzfse}/lib/liblzfse.so'
              substituteInPlace asahi_firmware/update.py \
                --replace '"tar"' '"${gnutar}/bin/tar"' \
                --replace '"xf"' '"-x", "-I", "${gzip}/bin/gzip", "-f"'
            '';
            nativeBuildInputs = [ pkgs.python3.pkgs.setuptools ];
            doCheck = false;
          };
          asahi-peripheral-firmware = prev.stdenv.mkDerivation rec {
            name = "asahi-peripheral-firmware";
            nativeBuildInputs = [ pkgs.asahi-fwextract pkgs.cpio ];
            buildCommand = ''
              mkdir extracted
              asahi-fwextract ${/. + ../hosts/mbp-m1-linux/firmware} extracted
              mkdir -p $out/lib/firmware
              cat extracted/firmware.cpio | cpio -id --quiet --no-absolute-filenames
              mv vendorfw/* $out/lib/firmware
            '';
          };
          asahi-mesa = (prev.mesa.override {
            galliumDrivers = [ "swrast" "asahi" ];
            vulkanDrivers = [ "swrast" ];
            enableGalliumNine = false;
          }).overrideAttrs (old: {
            version = versionOf inputs.mesa-src;
            src = inputs.mesa-src;
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
