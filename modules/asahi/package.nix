{ pkgs, inputs, withRust ? false, kernelPatches ? [ ] }:
let
  localPkgs =
    # we do this so the config can be read on any system and not affect
    # the output hash
    if builtins ? currentSystem then
      import (pkgs.path) { system = builtins.currentSystem; }
    else
      pkgs;

  lib = localPkgs.lib;

  parseExtraConfig = cfg:
    let
      lines = builtins.filter (s: s != "") (lib.strings.splitString "\n" cfg);
      perLine = line:
        let kv = lib.strings.splitString " " line;
        in
        assert (builtins.length kv == 2);
        "CONFIG_${builtins.elemAt kv 0}=${builtins.elemAt kv 1}";
    in
    lib.strings.concatMapStringsSep "\n" perLine lines;

  readConfig = configfile:
    import (localPkgs.runCommand "config.nix" { } ''
      echo "{ } // " > "$out"
      while IFS='=' read key val; do
        [ "x''${key#CONFIG_}" != "x$key" ] || continue
        no_firstquote="''${val#\"}";
        echo '{  "'"$key"'" = "'"''${no_firstquote%\"}"'"; } //' >> "$out"
      done < "${configfile}"
      echo "{ }" >> $out
    '').outPath;

  linux_asahi_pkg =
    { stdenv
    , lib
    , fetchFromGitHub
    , fetchpatch
    , linuxKernel
    , rustPlatform
    , rustfmt
    , rust-bindgen
    , ...
    }@args:
    let
      configfile =
        if kernelPatches == [ ] then
          ./config
        else
          pkgs.writeText "config" ''
            ${builtins.readFile ./config}

            # Patches
            ${lib.strings.concatMapStringsSep "\n"
            ({ extraConfig ? "", ... }: parseExtraConfig extraConfig)
            kernelPatches}
          '';

      _kernelPatches = kernelPatches;
    in
    (linuxKernel.manualConfig
      rec {
        inherit stdenv lib;

        version = "asahi-edge-dev";
        src = inputs.linux-src;
        modDirVersion = version;

        kernelPatches = [
          # patch the kernel to set the default size to 16k instead of modifying
          # the config so we don't need to convert our config to the nixos
          # infrastructure or patch it and thus introduce a dependency on the host
          # system architecture
          {
            name = "default-pagesize-16k";
            patch = ./default-pagesize-16k.patch;
          }
        ] ++ _kernelPatches;

        inherit configfile;
        config = readConfig configfile;

        extraMeta.branch = "6.1";
      } // (args.argsOverride or { })).overrideAttrs (old:
    if withRust then {
      nativeBuildInputs = (old.nativeBuildInputs or [ ])
        ++ [ rust-bindgen rustfmt rustPlatform.rust.rustc ];
      RUST_LIB_SRC = rustPlatform.rustLibSrc;
    } else
      { });
  linux-asahi = (pkgs.callPackage linux_asahi_pkg { });
in
pkgs.recurseIntoAttrs (pkgs.linuxPackagesFor linux-asahi)
