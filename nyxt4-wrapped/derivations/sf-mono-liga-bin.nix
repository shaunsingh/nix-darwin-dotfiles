{
  stdenvNoCC,
  fetchFromGitHub,
}:
stdenvNoCC.mkDerivation rec {
  pname = "sf-mono-liga-bin";
  version = "7723040ef50633da5094f01f93b96dae5e9b9299";

  src = fetchFromGitHub {
    owner = "shaunsingh";
    repo = "SFMono-Nerd-Font-Ligaturized";
    rev = version;
    sha256 = "sha256-vPUl6O/ji4hHIH7/qSbUe7q1QdugE1D1ZRw92QcSSDQ=";
  };

  dontConfigure = true;
  installPhase = ''
    mkdir -p $out/share/fonts/opentype
    cp -R $src/*.otf $out/share/fonts/opentype
  '';
}
