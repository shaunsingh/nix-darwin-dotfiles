{ fetchurl, lib, stdenv }:

stdenv.mkDerivation rec {
  pname = "julia-bin";
  version = "1.8.3";

  src = {
    aarch64-darwin = fetchurl {
      url = "https://julialang-s3.julialang.org/bin/mac/aarch64/${lib.versions.majorMinor version}/julia-${version}-macaarch64.tar.gz";
      sha256 = "sha256-9XrNAh5+fApA0plnoPaAynfFmI2FbhzCIJgsa/o5ZP8=";
    };
  }.${stdenv.hostPlatform.system} or (throw "Unsupported system: ${stdenv.hostPlatform.system}");

  installPhase = ''
    runHook preInstall
    cp -r . $out
    runHook postInstall
  '';

  # Breaks backtraces, etc.
  dontStrip = true;

  meta = {
    description = "High-level, high-performance, dynamic language for technical computing";
    homepage = "https://julialang.org";
    # Bundled and linked with various GPL code, although Julia itself is MIT.
    license = lib.licenses.gpl2Plus;
    maintainers = with lib.maintainers; [ shaunsingh ];
    platforms = [ "aarch64-darwin" ];
    mainProgram = "julia";
  };
}
