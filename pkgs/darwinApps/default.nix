{ pkgs, lib ? pkgs.lib, debug ? false, ... }:

with lib;

let
  mkDarwinApp = { appName, version, src, buildInputs ? [ pkgs.undmg ], appMeta ? { }, postInstall ? false, ... }:
    assert pkgs.stdenv.isDarwin;
    pkgs.stdenv.mkDerivation rec {
      inherit buildInputs;
      inherit version;
      inherit src;
      inherit postInstall;

      name = "${builtins.replaceStrings [" "] ["_"] (lib.toLower appName)}-darwin-${version}";

      installPhase = ''
        mkdir -p "$out/Applications/${appName}.app"
        cp -R . "$out/Applications/${appName}.app"
      '';

      meta = with pkgs.stdenv.lib; appMeta // {
        platforms = platforms.darwin;
      };
    };
  darwinAppWrapper = { appName, app, ... }: pkgs.stdenv.mkDerivation rec {
    name = "${builtins.replaceStrings [" "] ["_"] (lib.toLower appName)}-app";
    src = ./.;

    installPhase = ''
      mkdir -p $out
      cp -r $src $out
    '';

    postInstall = ''
      ln -f ${app}/Applications/${appName}.app ~/Applications/${appName}.app
    '';
  };
in
mapAttrs'
  (name: type: {
    name = removeSuffix ".nix" name;
    value = let file = ./. + "/${name}"; in
      lib.callPackageWith
        (pkgs // {
          inherit mkDarwinApp;
          inherit darwinAppWrapper;
          inherit debug;
        })
        file
        { };
  })
  (filterAttrs
    (name: type:
      (type == "directory" && builtins.pathExists "${toString ./.}/${name}/default.nix") ||
      (type == "regular" && hasSuffix ".nix" name && ! (name == "default.nix"))
    )
    (builtins.readDir ./.))
