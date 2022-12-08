{ lib
  # , stdenv
, crane-lib
, runCommand
, src
, version
, fontconfig
, ncurses
, perl
, pkg-config
, python3
, zlib
, CoreGraphics
, Cocoa
, Foundation
, libiconv
, UserNotifications
}:

let
  pname = "wezterm";
  
  nativeBuildInputs = [
    pkg-config
    python3
    ncurses
    perl
  ];

  buildInputs = [
    fontconfig
    zlib
    Cocoa
    CoreGraphics
    Foundation
    libiconv
    UserNotifications
  ];

  cargoArtifacts = crane-lib.buildDepsOnly {
    inherit src pname nativeBuildInputs buildInputs;
  };
in
crane-lib.buildPackage rec {
  inherit src pname version cargoArtifacts nativeBuildInputs buildInputs;

  postInstall = ''
    mkdir -p $out/nix-support
    echo "${passthru.terminfo}" >> $out/nix-support/propagated-user-env-packages
  '';

  preFixup = ''
    mkdir -p "$out/Applications"
    OUT_APP="$out/Applications/WezTerm.app"
    cp -r assets/macos/WezTerm.app "$OUT_APP"
    rm $OUT_APP/*.dylib
    cp -r assets/shell-integration/* "$OUT_APP"
    ln -s $out/bin/{wezterm,wezterm-mux-server,wezterm-gui,strip-ansi-escapes} "$OUT_APP"
  '';

  passthru.terminfo = runCommand "wezterm-terminfo"
    {
      nativeBuildInputs = [
        ncurses
      ];
    } ''
    mkdir -p $out/share/terminfo $out/nix-support
    tic -x -o $out/share/terminfo ${src}/termwiz/data/wezterm.terminfo
  '';

  doCheck = false;

  meta = with lib; {
    description = "A GPU-accelerated cross-platform terminal emulator and multiplexer written by @wez and implemented in Rust";
    homepage = "https://wezfurlong.org/wezterm";
    license = licenses.mit;
    platforms = platforms.unix;
    maintainers = with maintainers; [ shaunsingh ];
  };
}
