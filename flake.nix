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
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "unstable";
    };
    nixos-hardware = {
      url = github:NixOS/nixos-hardware/master;
      inputs.nixpkgs.follows = "unstable";
    };
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "unstable";
    };
    # Bar
    spacebar = {
      url = "github:shaunsingh/spacebar";
      inputs.nixpkgs.follows = "unstable";
    };
    # WM
    yabai-src = {
      url = "github:koekeishiya/yabai";
      flake = false;
    };
    # Editors
    emacs-src = {
      url = "github:emacs-mirror/emacs";
      flake = false;
    };
    emacs-ng = {
      url = "github:emacs-ng/emacs-ng";
      inputs.nixpkgs.follows = "unstable";
    };
    emacs-vterm-src = {
      url = "github:akermu/emacs-libvterm";
      flake = false;
    };
    doom-emacs = {
      url = "github:hlissner/doom-emacs";
      flake = false;
    };
    # Themeing
    base16 = {
      url = "github:shaunsingh/base16-nix";
      inputs.nixpkgs.follows = "unstable";
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
  };
  outputs = { self, nixpkgs, darwin, home-manager, ... }@inputs: {
    darwinConfigurations."shaunsingh-laptop" = darwin.lib.darwinSystem {
      system = "aarch64-darwin";
      modules = [
        ./modules/mac.nix
        ./modules/pam.nix
        ./modules/editors.nix
        home-manager.darwinModule
        {
          home-manager = {
            useGlobalPkgs = true;
            useUserPackages = true;
            users.shauryasingh = {
              imports = [
                inputs.base16.hmModule
                ./modules/home.nix
                ./modules/theme.nix
              ];
            };
          };
        }
        ({ config, pkgs, lib, ... }: {
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
                neovide = pkgs.callPackage ./pkgs/neovide { };
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
                emacs-vterm = prev.stdenv.mkDerivation rec {
                  pname = "emacs-vterm";
                  version = "master";

                  src = inputs.emacs-vterm-src;

                  nativeBuildInputs = [
                    prev.cmake
                    prev.libtool
                    prev.glib.dev
                  ];

                  buildInputs = [
                    prev.glib.out
                    prev.libvterm-neovim
                    prev.ncurses
                  ];

                  cmakeFlags = [
                    "-DUSE_SYSTEM_LIBVTERM=yes"
                  ];

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
                emacs = (prev.emacs.override {
                  srcRepo = true;
                  nativeComp = true;
                  withSQLite3 = true;
                  withXwidgets = true;
                }).overrideAttrs (o: rec {
                  version = "29.0.50";
                  src = inputs.emacs-src;

                  buildInputs = o.buildInputs ++ [ prev.darwin.apple_sdk.frameworks.WebKit ];

                  configureFlags = o.configureFlags ++ [
                    "--without-gpm"
                    "--without-dbus"
                    "--without-mailutils"
                    "--without-toolkit-scroll-bars"
                    "--without-pop"
                  ];

                  patches = [
                    ./patches/fix-window-role.patch
                    ./patches/system-appearance.patch
                    # ./patches/no-titlebar.patch
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
          nix = {
            package = pkgs.nix;
            extraOptions = ''
              system = aarch64-darwin
              extra-platforms = aarch64-darwin x86_64-darwin
              experimental-features = nix-command flakes
              build-users-group = nixbld
            '';
          };
          programs.fish.enable = true;
          environment.shells = with pkgs; [ fish ];
          users.users.shauryasingh = {
            home = "/Users/shauryasingh";
            shell = pkgs.fish;
          };
          system.activationScripts.postActivation.text = ''
            # Set the default shell as fish for the user
            sudo chsh -s ${lib.getBin pkgs.fish}/bin/fish shauryasingh
          '';
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
            discocss
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
    nixosConfigurations = {
      shaunsingh-thinkpad = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
        modules = [
          ./modules/editors.nix
          ./hardware/thinkpad-hardware-configuration.nix
          inputs.nixos-hardware.nixosModules.lenovo-thinkpad-x1
          inputs.nixos-hardware.nixosModules.lenovo-thinkpad-x1-7th-gen
          home-manager.nixosModules.home-manager
          {
            home-manager = {
              useGlobalPkgs = true;
              useUserPackages = true;
              users.shauryasingh = {
                imports = [
                  inputs.base16.hmModule
                  ./modules/home.nix
                  ./modules/theme.nix
                ];
              };
            };
          }
          ({ config, pkgs, lib, ... }: {
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
                  emacs-vterm = prev.stdenv.mkDerivation rec {
                    pname = "emacs-vterm";
                    version = "master";

                    src = inputs.emacs-vterm-src;

                    nativeBuildInputs = [
                      prev.cmake
                      prev.libtool
                      prev.glib.dev
                    ];

                    buildInputs = [
                      prev.glib.out
                      prev.libvterm-neovim
                      prev.ncurses
                    ];

                    cmakeFlags = [
                      "-DUSE_SYSTEM_LIBVTERM=yes"
                    ];

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
                })
              ];
            };
            nix = {
              package = pkgs.nixUnstable;
              extraOptions = ''
                experimental-features = nix-command flakes
              '';
            };

            # Use fish, launch exwm after login
            programs.fish = {
              enable = true;
              loginShellInit = ''
                if test (id --user $USER) -ge 1000 && test (tty) = "/dev/tty1"
                   exec >~/.logs/xsession 2>&1
                   export LANG="en_GB.UTF-8"
                   export LANGUAGE="en_GB.UTF-8"
                   export DBUS_SESSION_BUS_ADDRESS=unix:path=/run/user/$(id -u)/bus
                   export _JAVA_AWT_WM_NONREPARENTING=1
                   wmname LG3D
                   xset -dpms
                   xset s off
                   xhost +SI:localuser:$USER
                   emacs -mm --with-exwm
                end
              '';
            };

            # enable the xserver
            services.xserver = {
              enable = true;
              libinput = {
                enable = true;
              };
            };

            # Define a user account. Don't forget to set a password with ‘passwd’.
            users.users.shauryasingh = {
              isNormalUser = true;
              extraGroups =
                [ "wheel" "networkManager" ]; # Enable ‘sudo’ for the user.
              shell = pkgs.fish;
            };

            # Power management
            services.power-profiles-daemon.enable = true;
            services.thermald.enable = true;
            powerManagement.cpuFreqGovernor = lib.mkDefault "performance";
            hardware.enableRedistributableFirmware = true;
            boot.kernelPackages = pkgs.linuxPackages_latest;

            # Network settings.
            networking = {
              hostName = "shaunsingh-thinkpad"; # Hostname
              useDHCP = false; # Deprecated, so set explicitly to false
              wireless.enable = false;
              networkmanager.enable = true;
              firewall.enable = false;
            };

            # Bootloader
            boot.loader.efi.canTouchEfiVariables = true;
            boot.loader.grub.enable = true;
            boot.loader.grub.version = 2;
            boot.loader.grub.device = "nodev";
            boot.loader.grub.useOSProber = true;
            time.hardwareClockInLocalTime = true;

            # Set your time zone.
            time.timeZone = "America/New_York";

            # Select internationalisation properties.
            i18n.defaultLocale = "en_US.UTF-8";
            console = {
              font = "Lat2-Terminus16";
              keyMap = "us";
            };

            # Sound
            sound.enable = false;
            hardware.pulseaudio.enable = false; 

            services.pipewire = {
              enable = true;
              alsa.enable = true;
              alsa.support32Bit = true;
              pulse.enable = true;
              jack.enable = true;
            };

            environment.systemPackages = with pkgs; [
              # emacs needs to be here since its a GUI app
              emacs

              # Build Tools
              rust-bin.nightly.latest.default

              # Language Servers
              nodePackages.pyright
              rust-analyzer

              # Formatting
              nixfmt
              black
              shellcheck

              # linux utils
              firefox

              # Terminal utils and rust alternatives :tm:
              xcp
              lsd
              procs
              tree
              zoxide
              bottom
              discocss

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
  };
}
