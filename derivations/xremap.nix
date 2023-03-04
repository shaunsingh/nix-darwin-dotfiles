{
  rustPlatform,
  fetchFromGitHub,
}:
rustPlatform.buildRustPackage rec {
  pname = "xremap";
  version = "24ab052adb76270e6d0da627e1ca8475bfd9224d";

  src = fetchFromGitHub {
    owner = "k0kubun";
    repo = "xremap";
    rev = version;
    sha256 = "sha256-louxRX9tg0me/El4XCxDGaZkRJbYVwKwy7yIN0z1z/A=";
  };

  buildFeatures = ["sway"];
  cargoSha256 = "sha256-ebBVNlvKZI1oJ7TviKXpNC+dq2QYZwYWySkf0MxesFo=";
}
