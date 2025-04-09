{
  stdenv,
  lib,
  testers,
  fetchFromGitHub,
  sbcl,
  pkg-config,
  libfixposix,
  openssl,
  xdg-utils,
  xclip,
  wl-clipboard,
  nix-update-script,
  sqlite,
  buildNpmPackage,
  importNpmLock,
  electron,
  applyPatches,
  makeWrapper,
  nodejs,
  runCommand,
}:

let
  version = "4.0.0-pre-release-4";

  src = fetchFromGitHub {
    owner = "atlas-engineer";
    repo = "nyxt";
    rev = "d369074";
    hash = "sha256-aKrQxyheLd/Sv6c9WWIjBWEwwu2Br1haajni6rCrH/M=";
    fetchSubmodules = true;
  };

  srcPatched = applyPatches {
    inherit src;
    patches = [
      ../patches/electron-builder.patch
      ../patches/electron-core.patch
    ];
  };

  # unpack tarball containing electron's headers
  electron-headers = runCommand "electron-headers" { } ''
    mkdir -p $out
    tar -C $out --strip-components=1 -xvf ${electron.headers}
  '';

  cl-electron-server-src = srcPatched + /_build/cl-electron;
  cl-electron-server = buildNpmPackage {
    pname = "cl-electron-server";
    version = (builtins.fromJSON (builtins.readFile "${src}/_build/cl-electron/package.json")).version;
    src = cl-electron-server-src;

    inherit nodejs;

    env.ELECTRON_SKIP_BINARY_DOWNLOAD = "1";

    # don't build native modules with node headers
    npmFlags = [ "--ignore-scripts" ];

    preConfigure = ''
      pushd $npmRoot

      # use electron's headers to make node-gyp compile against the electron ABI
      export npm_config_nodedir="${electron-headers}"

      ### override the detected electron version
      substituteInPlace node_modules/@electron-forge/core-utils/dist/electron-version.js \
        --replace-fail "return version" "return '${electron.version}'"

      ### create the electron archive to be used by electron-packager
      cp -r ${electron.dist} electron-dist
      chmod -R u+w electron-dist

      pushd electron-dist
      zip -0Xqr ../electron.zip .
      popd

      rm -r electron-dist

      # force @electron/packager to use our electron instead of downloading it
      substituteInPlace node_modules/@electron/packager/src/index.js \
        --replace-fail "await this.getElectronZipPath(downloadOpts)" "'$(pwd)/electron.zip'"

      # now that we patched everything, we still have to run the scripts we ignored with --ignore-scripts
      npm rebuild

      popd
    '';

    npmDeps = importNpmLock {
      npmRoot = cl-electron-server-src;
    };

    npmConfigHook = importNpmLock.npmConfigHook;

    meta = {
      description = "Electron server for cl-electron.";
      homepage = "https://github.com/atlas-engineer/cl-electron";
      license = lib.licenses.bsd3;
    };
  };
in stdenv.mkDerivation (finalAttrs: {
  pname = "nyxt";

  src = srcPatched;
  inherit version;

  nativeBuildInputs = [ makeWrapper ];

  buildInputs = [
    sbcl
    # for groveller
    pkg-config
    libfixposix
    cl-electron-server
  ];

  postPatch = ''
    substituteInPlace _build/cl-electron/source/core.lisp \
      --subst-var-by "cl-electron-server" "${cl-electron-server}/lib/node_modules/cl-electron-server"
  '';

  # for cffi
  LD_LIBRARY_PATH = lib.makeLibraryPath [
    openssl
    libfixposix
    sqlite
  ];

  postConfigure = ''
    export ASDF_OUTPUT_TRANSLATIONS="$(pwd):$(pwd)"
    export PREFIX="$out"
    export NYXT_VERSION="$version"
  '';

  makeFlags = [
    "all"
    # build cl-electron in nix instead of npm.
    "NODE_SETUP=false"
    "NYXT_SUBMODULES=true"
    "NYXT_RENDERER=electron"
  ];

  NODE_PATH = cl-electron-server.npmDeps;

  postFixup = ''
    wrapProgram "$out/bin/nyxt" \
      --prefix LD_LIBRARY_PATH : "${finalAttrs.LD_LIBRARY_PATH}" \
      --prefix NODE_PATH : "${finalAttrs.NODE_PATH}" \
      --prefix PATH : "${
      lib.makeBinPath [
        xdg-utils
        xclip
        wl-clipboard
        electron
      ]
    }"
  '';

  # prevent corrupting core in exe
  dontStrip = true;

  passthru = {
    tests.version = testers.testVersion { package = finalAttrs.finalPackage; };
    updateScript = nix-update-script { };
  };

  meta = with lib; {
    description = "Infinitely extensible web-browser (with Lisp development files using Electron platform port)";
    mainProgram = "nyxt";
    homepage = "https://nyxt.atlas.engineer";
    license = licenses.bsd3;
    platforms = platforms.all;
  };
})
