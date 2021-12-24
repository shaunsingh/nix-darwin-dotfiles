{ rustPlatform, runCommand, lib, fetchFromGitHub, fetchgit, fetchurl
, makeWrapper, pkg-config, python2, openssl, SDL2, fontconfig, freetype, ninja
, gn, llvmPackages, makeFontsConf, stdenv, xcbuild, AppKit, ApplicationServices, Carbon, CoreFoundation, CoreGraphics, CoreVideo
, Foundation, OpenGL, QuartzCore }:
rustPlatform.buildRustPackage rec {
  pname = "neovide";
  version = "unstable-2021-10-09";

  src = fetchFromGitHub {
    owner = "Kethku";
    repo = "neovide";
    rev = "7f76ad4764197ba75bb9263d25b265d801563ccf";
    sha256 = "sha256-kcP0WSk3quTaWCGQYN4zYlDQ9jhx/Vu6AamSLGFszwQ=";
  };

  cargoSha256 = "sha256-TQEhz9FtvIb/6Qtyz018dPle0+nub1oMZMFtKAqYcoI=";

  SKIA_SOURCE_DIR = let
    repo = fetchFromGitHub {
      owner = "rust-skia";
      repo = "skia";
      rev = "m91-0.39.4";
      sha256 = "sha256-ovlR1vEZaQqawwth/UYVUSjFu+kTsywRpRClBaE1CEA=";
    };
    # The externals for skia are taken from skia/DEPS
    externals =
      lib.mapAttrs (n: v: fetchgit v) (lib.importJSON ./skia-externals.json);
  in runCommand "source" { } (''
    cp -R ${repo} $out
    chmod -R +w $out

    mkdir -p $out/third_party/externals
    cd $out/third_party/externals
  '' + (builtins.concatStringsSep "\n"
    (lib.mapAttrsToList (name: value: "cp -ra ${value} ${name}") externals)));

  SKIA_NINJA_COMMAND = "${ninja}/bin/ninja";
  SKIA_GN_COMMAND = "${gn}/bin/gn";
  LIBCLANG_PATH = "${llvmPackages.libclang.lib}/lib";

  preConfigure = ''
    unset CC CXX
  '';

  # test needs a valid fontconfig file
  FONTCONFIG_FILE = makeFontsConf { fontDirectories = [ ]; };

  nativeBuildInputs = [
    pkg-config
    makeWrapper
    python2 # skia-bindings
    llvmPackages.clang # skia
    xcbuild
  ];

  doCheck = false;

  buildInputs = [
    openssl
    SDL2
    AppKit
    ApplicationServices
    Carbon
    CoreFoundation
    CoreGraphics
    CoreVideo
    Foundation
    OpenGL
    QuartzCore
    (fontconfig.overrideAttrs (old: {
      propagatedBuildInputs = [
        (freetype.overrideAttrs (old: rec {
          version = "2.10.4";
          src = fetchurl {
            url =
              "mirror://savannah/${old.pname}/${old.pname}-${version}.tar.xz";
            sha256 = "112pyy215chg7f7fmp2l9374chhhpihbh8wgpj5nj6avj3c59a46";
          };
        }))
      ];
    }))
  ];

  meta = with lib; {
    description = "This is a simple graphical user interface for Neovim.";
    homepage = "https://github.com/Kethku/neovide";
    license = with licenses; [ mit ];
    maintainers = with maintainers; [ shaunsingh ];
    platforms = platforms.unix;
    mainProgram = "neovide";
  };
}
