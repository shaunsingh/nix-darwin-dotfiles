{
  lib,
  rustPlatform,
  fetchFromGitHub,
  extra-cmake-modules,
  pkg-config,
  libnotify,
}:
rustPlatform.buildRustPackage rec {
  pname = "bato";
  version = "2b8529f85e2a5a9ea50ca1292930d5df85f7098d";

  src = fetchFromGitHub {
    owner = "doums";
    repo = "bato";
    rev = version;
    sha256 = "sha256-i2gw8vXiKutq26ACzkVXH3kED7jAngSv2mNo9P3qXnA=";
  };

  nativeBuildInputs = [extra-cmake-modules pkg-config];
  buildInputs = [libnotify];

  cargoSha256 = "sha256-TJH9y4eMc09HxWdx+sJE+ElicUCK+EvC1lVZZlvzYKo=";
}
