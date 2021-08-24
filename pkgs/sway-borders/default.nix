{ pkgs, stdenv, ... }:

stdenv.mkDerivation {
  name = "sway-borders";
  version = "1.5.1";
  
  src = pkgs.fetchFromGitHub {
    owner = "fluix-dev";
    repo = "sway-borders";
    rev = "a5eaab1ba61f881e50561271bdd94bf3f3912093";
    sha256 = "1sljf5yk1flyd8xk0cjmnr0iw74mm1c67xfpnfbwma4mzizrpqpc";
  };

  patches = [
    ./patches/sway-config-no-nix-store-references.patch
    ./patches/load-configuration-from-etc.patch
    ./patches/bump-wlroots-version.patch
    
    (pkgs.substituteAll {
      src = ./patches/fix-paths.patch;
      inherit (pkgs.swaybg);
    })
  ];

  nativeBuildInputs = with pkgs; [
    meson ninja pkg-config wayland scdoc
  ];

  buildInputs = with pkgs; [
    wayland libxkbcommon pcre json_c dbus libevdev
    pango cairo libinput libcap pam gdk-pixbuf librsvg
    wlroots wayland-protocols
  ];
    
  mesonFlags = [
    "-Ddefault-wallpaper=false"
  ];

  meta = with pkgs.lib; {
    description = "An i3-compatible tiling Wayland compositor";
    longDescription = ''
      Sway is a tiling Wayland compositor and a drop-in replacement for the i3
      window manager for X11. It works with your existing i3 configuration and
      supports most of i3's features, plus a few extras.
      Sway allows you to arrange your application windows logically, rather
      than spatially. Windows are arranged into a grid by default which
      maximizes the efficiency of your screen and can be quickly manipulated
      using only the keyboard.
    '';
    homepage    = "https://swaywm.org";
    changelog   = "https://github.com/swaywm/sway/releases/tag/${version}";
    license     = licenses.mit;
    platforms   = platforms.linux;
    maintainers = with maintainers; [ primeos synthetica ma27 ];
  };
}
