{
  lib,
  rustPlatform,
  fetchFromGitHub,
}:
rustPlatform.buildRustPackage rec {
  pname = "rip";
  version = "11f0b8d018975f78d922038efc7f60bf9fffafd0";

  src = fetchFromGitHub {
    owner = "nivekuil";
    repo = "rip";
    rev = version;
    sha256 = "sha256-fkrWIysGJRscos+/dxCpNzfSgRiugKV+JZSZro1JIQI=";
  };

  cargoSha256 = "sha256-cOJtnrgFD8IB2tRXkDKQDXRm8BMK08K9grIuidAxIgU=";
}
