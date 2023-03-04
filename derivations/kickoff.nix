{
  lib,
  rustPlatform,
  fetchFromGitHub,
  pkg-config,
  fontconfig,
  libxkbcommon,
  wayland,
}:
rustPlatform.buildRustPackage rec {
  pname = "kickoff";
  version = "37a29d84239388c14f82d8b508b580dc8a635b76";

  src = fetchFromGitHub {
    owner = "j0ru";
    repo = "kickoff";
    rev = version;
    sha256 = "sha256-2Az/hBNR0bgkr3IVNaZzNUFJZaqyF+ui14jREFVxce0=";
  };

  cargoSha256 = "sha256-kifOE795rM8QpDNCF3MQi4M15w/8OsmNK6cqkizOwG0=";

  nativeBuildInputs = [pkg-config];
  buildInputs = [fontconfig libxkbcommon wayland];

  postFixup = ''
    patchelf $out/bin/kickoff --add-rpath ${lib.makeLibraryPath buildInputs}
  '';
}
