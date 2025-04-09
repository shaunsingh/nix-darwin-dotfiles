{ lib
, stdenv
, fetchFromGitHub
, python310
, nodejs_22
, node-gyp
, pnpm_9
, makeWrapper
, dart-sass
}:

stdenv.mkDerivation rec {
  pname = "wetty";
  version = "2.7.0";

  src = fetchFromGitHub {
    owner = "butlerx";
    repo = "wetty";
    rev = "643e88b78e5b692aecf878753fcc3867dc061786";
    sha256 = "sha256-YBXJA+B3NNp1gAKTP61gmi//gSxV65TMHQQ+qaxf+58=";
  };

  patches = [
    ../patches/wetty-background.patch
    ../patches/wetty-oxocarbon.patch
    ../patches/wetty-padding.patch
  ];

  nativeBuildInputs = [
    nodejs_22
    node-gyp
    python310
    pnpm_9.configHook
    makeWrapper
    dart-sass
  ];

  pnpmDeps = pnpm_9.fetchDeps {
    inherit pname version src;
    hash = "sha256-zGJAXziQglF84nmmZ4a1+6MAqDHelPFFTpl5G6vdyiU=";
  };

  postPatch = ''
    # no git so no need for husky
    substituteInPlace package.json --replace '"prepare": "husky install"' '"prepare": "echo Skipping husky install"'
  '';

  configurePhase = ''
    runHook preConfigure

    # fake home to sandbox
    export HOME=$(mktemp -d)
    pnpm config set node-linker hoisted

    runHook postConfigure
  '';

  buildPhase = ''
    runHook preBuild

    export npm_config_build_from_source=true
    pnpm install --offline

    # rebuild native modules
    find node_modules/.pnpm -maxdepth 3 -type d -name gc-stats | while read -r dir; do
      echo "Rebuilding native module in $dir"
      (cd "$dir" && node-gyp rebuild)
    done
    find node_modules/.pnpm -maxdepth 3 -type d -name node-pty | while read -r dir; do
      echo "Rebuilding node-pty native module in $dir"
      (cd "$dir" && node-gyp rebuild)
    done

    # replace dart sass with nix package
    find node_modules/.pnpm/sass-embedded-linux-*/node_modules/sass-embedded-linux-*/dart-sass/src -name dart -print0 | xargs -I {} -0 patchelf --set-interpreter "$(<$NIX_CC/nix-support/dynamic-linker)" {}

    pnpm build

    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    mkdir -p $out/bin
    mkdir -p $out/lib/node_modules/wetty
    cp -r build package.json node_modules $out/lib/node_modules/wetty/
    makeWrapper ${nodejs_22}/bin/node $out/bin/wetty --add-flags "$out/lib/node_modules/wetty/build/main.js"
    runHook postInstall
  '';

  meta = with lib; {
    description = "Terminal in browser over HTTP/HTTPS";
    homepage = "https://github.com/butlerx/wetty";
    license = licenses.mit;
    mainProgram = "wetty";
    platforms = platforms.unix;
  };
}

