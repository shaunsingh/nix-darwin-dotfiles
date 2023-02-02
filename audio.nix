alsa-ucm-conf = prev.alsa-ucm-conf.overrideAttrs (old: rec {
  installPhase = ''
    runHook preInstall
    mkdir -p $out/share/alsa
    cp -r ucm ucm2 $out/share/alsa
    cp -r ${inputs.asahi-alsa-src}/ucm2/conf.d/macaudio $out/share/alsa/ucm2/conf.d/macaudio
    runHook postInstall
  '';
});
