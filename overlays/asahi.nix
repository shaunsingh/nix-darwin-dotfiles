final: prev: {
  asahi-alsa-utils = prev.alsa-utils.override {
    alsa-lib = prev.alsa-lib.override {
      alsa-ucm-conf = prev.alsa-ucm-conf.overrideAttrs (_: rec {
        asahi-alsa-src = prev.fetchFromGitHub {
          owner = "AsahiLinux";
          repo = "alsa-ucm-conf-asahi";
          rev = "461b4fe8853fc876c6b2f92414efa9d63f6aa213";
          sha256 = "sha256-BacaisE38uA5Gf5rHiYC2FRY29kJ1THBQ861wo5HJYI=";
        };

        installPhase = ''
          runHook preInstall
          mkdir -p $out/share/alsa
          cp -r ucm ucm2 $out/share/alsa
          cp -r ${asahi-alsa-src}/ucm2/conf.d/macaudio $out/share/alsa/ucm2/conf.d/macaudio
          runHook postInstall
        '';
      });
    };
  };
}
