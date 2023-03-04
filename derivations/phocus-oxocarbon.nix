{
  stdenvNoCC,
  fetchFromGitHub,
  nodePackages,
}:
stdenvNoCC.mkDerivation rec {
  pname = "phocus-oxocarbon";
  version = "0cf0eb35a927bffcb797db8a074ce240823d92de";

  src = fetchFromGitHub {
    owner = "phocus";
    repo = "gtk";
    rev = version;
    sha256 = "sha256-URuoDJVRQ05S+u7mkz1EN5HWquhTC4OqY8MqAbl0crk=";
  };

  patches = [
    ../patches/remove-npm.diff
    ../patches/gradient.diff
    ../patches/accent-substitute-all.diff
  ];

  postPatch = ''
    substituteInPlace scss/gtk-3.0/_colors.scss \
      --replace "@bg0@" "#161616" \
      --replace "@bg1@" "#262626" \
      --replace "@bg2@" "#393939"\
      --replace "@bg3@" "#424242" \
      --replace "@bg4@" "#525252" \
      --replace "@red@" "#ff7eb6" \
      --replace "@lred@" "#ff7eb6" \
      --replace "@orange@" "#ee5396" \
      --replace "@lorange@" "#ee5396" \
      --replace "@yellow@" "#33b1ff" \
      --replace "@lyellow@" "#33b1ff" \
      --replace "@green@" "#42be65" \
      --replace "@lgreen@" "#42be65" \
      --replace "@cyan@" "#3ddbd9" \
      --replace "@lcyan@" "#3ddbd9" \
      --replace "@blue@" "#08bdba" \
      --replace "@lblue@" "#08bdba" \
      --replace "@purple@" "#be95ff" \
      --replace "@lpurple@" "#be95ff" \
      --replace "@pink@" "#ff7eb6" \
      --replace "@lpink@" "#ff7eb6" \
      --replace "@primary@" "#f2f4f8" \
      --replace "@secondary@" "#dde1e6"
  '';

  nativeBuildInputs = [nodePackages.sass];
  installFlags = ["DESTDIR=$(out)" "PREFIX="];
}
