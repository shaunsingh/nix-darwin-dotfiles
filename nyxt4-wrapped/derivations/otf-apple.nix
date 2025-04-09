{
  stdenvNoCC,
  fetchurl,
  p7zip,
}:
stdenvNoCC.mkDerivation {
  pname = "otf-apple";
  version = "1.0";

  src = [
    (fetchurl {
      url = "https://devimages-cdn.apple.com/design/resources/download/SF-Pro.dmg";
      sha256 = "sha256-IccB0uWWfPCidHYX6sAusuEZX906dVYo8IaqeX7/O88=";
    })
    (fetchurl {
      url = "https://devimages-cdn.apple.com/design/resources/download/SF-Mono.dmg";
      sha256 = "sha256-bUoLeOOqzQb5E/ZCzq0cfbSvNO1IhW1xcaLgtV2aeUU=";
    })
    (fetchurl {
      url = "https://devimages-cdn.apple.com/design/resources/download/SF-Compact.dmg";
      sha256 = "sha256-PlraM6SwH8sTxnVBo6Lqt9B6tAZDC//VCPwr/PNcnlk=";
    })
    (fetchurl {
      url = "https://devimages-cdn.apple.com/design/resources/download/NY.dmg";
      sha256 = "sha256-HC7ttFJswPMm+Lfql49aQzdWR2osjFYHJTdgjtuI+PQ=";
    })
  ];

  buildInputs = [p7zip];
  sourceRoot = "./";
  preUnpack = "mkdir fonts";
  unpackCmd = ''
    7z x $curSrc >/dev/null
    dir="$(find . -not \( -path ./fonts -prune \) -type d | sed -n 2p)"
    cd $dir 2>/dev/null
    7z x *.pkg >/dev/null
    7z x Payload~ >/dev/null
    mv Library/Fonts/*.otf ../fonts/
    cd ../
    rm -R $dir
  '';
  installPhase = ''
    mkdir -p $out/share/fonts/opentype/{SF\ Pro,SF\ Compact,New\ York}
    cp -a fonts/SF-Pro*.otf $out/share/fonts/opentype/SF\ Pro
    cp -a fonts/SF-Compact*.otf $out/share/fonts/opentype/SF\ Compact
    cp -a fonts/NewYork*.otf $out/share/fonts/opentype/New\ York
  '';
}
