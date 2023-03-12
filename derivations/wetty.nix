{ fetchFromGitHub
, mkYarnPackage
, makeWrapper
, nodejs
}:

mkYarnPackage rec {
  pname = "wetty";
  version = "2.5.0";

  src = fetchFromGitHub {
    owner = "butlerx";
    repo = pname;
    rev = "v${version}";
    hash = "sha256-cc+IPTmWmYNKAMOyiQfL9FZB9102x9cvcpB5Y9ZqAes=";
  };

  yarnFlags = [ "--production" ];
  nativeBuildInputs = [ makeWrapper ];
  distPhase = ":"; # disable useless $out/tarballs directory

  installPhase = ''
    runHook preInstall
    mkdir -p $out/{bin,libexec/wetty}
    mv node_modules $out/libexec/wetty/node_modules
    mv deps $out/libexec/wetty/deps

    makeWrapper '${nodejs}/bin/node' "$out/bin/wetty" \
      --add-flags "$out/libexec/wetty/deps/wetty/build/main.js" \
      --set NODE_ENV production
    runHook postInstall
  '';
}
