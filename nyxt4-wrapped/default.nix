{
  config,
  lib,
  pkgs,
  ...
}: let
  # needed fonts
  otf-apple = pkgs.callPackage ./derivations/otf-apple.nix {};
  sf-mono-liga-bin = pkgs.callPackage ./derivations/sf-mono-liga-bin.nix {};
  # nyxt4-electron = pkgs.callPackage ./nyxt.nix {};
  # shellinabox = pkgs.callPackage ./derivations/shellinabox.nix {};
  wetty = pkgs.callPackage ./derivations/wetty.nix {};

  # enable DRM support on webkit
  webkitgtk-eme = pkgs.webkitgtk_4_1.overrideAttrs (oldAttrs: rec {
    buildInputs =
      oldAttrs.buildInputs
      ++ [
        pkgs.libgpg-error
      ];
    cmakeFlags =
      oldAttrs.cmakeFlags
      ++ [
        "-DENABLE_ENCRYPTED_MEDIA=ON"
      ];
  });

  # nyxt prerelease w/ DRM support
  nyxt4-prerelease-3 = pkgs.nyxt.overrideAttrs (oldAttrs: rec {
    pname = "nyxt4-3";
    version = "4.0.0-pre-release-3";
    src = pkgs.fetchzip {
      url = "https://github.com/atlas-engineer/nyxt/releases/download/${version}/nyxt-${version}-source-with-submodules.tar.xz";
      hash = "sha256-T5p3OaWp28rny81ggdE9iXffmuh6wt6XSuteTOT8FLI=";
      stripRoot = false;
    };
    LD_LIBRARY_PATH =
      lib.makeLibraryPath
      [
        pkgs.glib
        pkgs.gobject-introspection
        pkgs.gdk-pixbuf
        pkgs.cairo
        pkgs.pango
        pkgs.gtk3
        pkgs.openssl
        pkgs.libfixposix
        webkitgtk-eme
      ];
  });

  # command to start shellinaboxd
  # fd = "3";
  # createFd = "${fd}<${../certificates/certificate.pem}";
  # sbx-args = [ "--background" "--disable-ssl" ]; # version of ssl is outdated/insecure anyways
  # sbx-cmd = "${shellinabox}/bin/shellinaboxd ${lib.concatStringsSep " " sbx-args}";
in {
  options.nyxt4-wrapped = {
    display = lib.mkOption {
      type = lib.types.str;
      description = "display output to use";
    };
    resolution = lib.mkOption {
      type = lib.types.str;
      description = "default resolution";
    };
    scale = lib.mkOption {
      type = lib.types.int;
      description = "integer scale for gui";
    };
  };
  config = {
    # optional personal config
    home-manager.users.nyxtkiosk = import ./home.nix;

    environment.defaultPackages = with pkgs; [
      # apps
      # nyxt4-electron
      nyxt4-prerelease-3
      cage

      # tools
      wlr-randr
      mpv
      grim
      slurp
      wf-recorder
      pamixer
      brightnessctl
      bluez-experimental
      tor
      gtk3
      wetty

      # wrapped
      (pkgs.writeShellScriptBin "nyxt-gamescope" ''
        gamescope -f -s ${builtins.toString config.nyxt4-wrapped.scale} -- nyxt "$@"
      '')
      (pkgs.writeShellScriptBin "nyxt-cage" ''
        GTK_USE_PORTAL=0 cage -m last -s -d -- sh -c 'wlr-randr --output ${config.nyxt4-wrapped.display} --mode ${config.nyxt4-wrapped.resolution} --scale ${builtins.toString config.nyxt4-wrapped.scale} && nyxt'
      '')

      # dev dependencies
      zola
    ];

    fonts = {
      packages = with pkgs; [
        sf-mono-liga-bin
        otf-apple
        twemoji-color-font
        sarasa-gothic
      ];
      fontconfig = {
        enable = lib.mkDefault true;
        antialias = true;
        subpixel.lcdfilter = "default";
      };
    };

    # asahi has vulkan issues
    programs.gamescope = {
      enable = true;
      capSysNice = true;
    };

#     # old openssl is insecure
#     nixpkgs.config.permittedInsecurePackages = [
#       "openssl-1.0.2u"
#     ];
# 
#     # terminal in web
#     systemd.user.services.shellinaboxd = {
#       description = "Shellinabox Web Server Daemon";
# 
#       wantedBy = [ "multi-user.target" ];
#       requires = [ "sshd.service" ];
#       after = [ "sshd.service" ];
# 
#       serviceConfig = {
#         Type = "forking";
#         ExecStart = "${sbx-cmd}";
#         ExecReload = "${pkgs.coreutils}/bin/kill -HUP $MAINPID";
#       };
#     };

    # terminal in web
    systemd.user.services.wetty = {
      description = "Wetty Web Server Daemon";

      wantedBy = [ "multi-user.target" ];
      requires = [ "sshd.service" ];
      after = [ "sshd.service" ];

      serviceConfig = {
        Type = "forking";
        ExecStart = "${wetty}";
        ExecReload = "${pkgs.coreutils}/bin/kill -HUP $MAINPID";
      };
    };

    # wpa_supplicant + wpa3 doesn't work on broadcom
    networking.networkmanager.enable = true;
  };
}
