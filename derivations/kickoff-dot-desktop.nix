{ rustPlatform
, fetchFromGitHub
,
}:
rustPlatform.buildRustPackage rec {
  pname = "kickoff-dot-desktop";
  version = "26cf8b9e6c7dc5afde29c200e91c4108d9841f93";

  src = fetchFromGitHub {
    owner = "j0ru";
    repo = "kickoff-dot-desktop";
    rev = version;
    sha256 = "sha256-CcBiCcfOJzuq0DnokTUHpMdo7Ry29ugQ+N7Hk0R+cQE=";
  };

  cargoSha256 = "sha256-bS7yBnxAWPoTTabxI6W5Knl1DFiDztYSkEPJMa8bqlY=";
}
