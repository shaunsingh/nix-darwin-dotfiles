final: prev: {
  sov = prev.sov.overrideAttrs (old: rec {
    pname = "sov";
    version = "v0.92b";
    postPatch = "";
    buildInputs = old.buildInputs ++ (with prev; [ egl-wayland libGL libxkbcommon ]);
    src = prev.fetchFromGitHub {
      owner = "milgra";
      repo = "sov";
      rev = "750e3e6dbffc515441ae804dc8768b2634de1a61";
      sha256 = "sha256-1L5D0pzcXbkz3VS7VB6ID8BJEbGeNxjo3xCr71CGcIo=";
    };
  });
}
