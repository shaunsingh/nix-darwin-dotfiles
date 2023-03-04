final: prev: {
  alacritty-ligatures = prev.alacritty.overrideAttrs (old: rec {
    pname = "alacritty-ligatures";

    src = prev.fetchFromGitHub {
      owner = "fee1-dead";
      repo = "alacritty";
      rev = "796cdfeeaae730b2224e910bf9f1dfa81abcfd51";
      sha256 = "sha256-CcBiCcfOJzuq0DnokTUHpMdo7Ry29ugQ+N7Hk0R+cQE=";
    };

    doCheck = false;
    cargoDeps = old.cargoDeps.overrideAttrs (_: {
      inherit src;
      outputHash = "sha256-qRvPBuDJ5K7II1LXOpTINs35XvKALOFQa4h5PPIMZic=";
    });
  });
}
