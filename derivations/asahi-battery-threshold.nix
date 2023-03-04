{
  lib,
  rustPlatform,
  fetchFromGitHub,
}:
rustPlatform.buildRustPackage rec {
  pname = "asahi-battery-threshold";
  version = "e502665fb990741d1e5dbd8fc69fa386d75399ba";

  src = fetchFromGitHub {
    owner = "PaddiM8";
    repo = "asahi-battery-threshold";
    rev = version;
    sha256 = "sha256-0mXCMer7aEePKTkO9qhTtr3KIMe1dqd3slLUjQg9W/k=";
  };

  cargoSha256 = "sha256-tWwoKGc+M6IXk4rzceEWF4LYztbnoY9OMHHDR5tkQSU=";
}
