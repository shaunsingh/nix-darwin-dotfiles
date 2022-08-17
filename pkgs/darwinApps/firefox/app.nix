{ pkgs, mkDarwinApp, fetchurl, appName, version, sha256, appMeta, ... }:

mkDarwinApp rec {
  inherit appName;
  inherit appMeta;
  inherit version;

  src = fetchurl {
    inherit sha256;
    url = "https://archive.mozilla.org/pub/firefox/releases/${version}/mac/en-US/Firefox%20${version}.dmg";
    name = "${ builtins.replaceStrings [" "] ["_"]  appName }.dmg";
  };
}
