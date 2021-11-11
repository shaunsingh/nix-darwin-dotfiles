# Overlays
# Sometimes there are packages that I want from git, or that aren't available from =nixpkgs=. This overlay adds the following:
# - Yabai (mac) from https://github.com/donaldguy/yabai
# - Eww (linux) from https://github.com/elkowar/eww
# - Alacritty-ligatures (linux) from https://github.com/zenixls2/alacritty/tree/ligature

# [[file:../nix-config.org::*Overlays][Overlays:1]]
{
  description = "overlays";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/master";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = { url = "github:edolstra/flake-compat"; flake = false; };
    flake-compat-ci.url = "github:hercules-ci/flake-compat-ci";
    eww.url = "github:elkowar/eww";
    alacritty-src = { url = "github:zenixls2/alacritty/ligature"; flake = false; };
    yabai-src = { url = "github:donaldguy/yabai/canon"; flake = false; };

  };

  outputs = args@{ self, flake-utils, nixpkgs, ... }:
    {
      ciNix = args.flake-compat-ci.lib.recurseIntoFlake self;

      overlay = final: prev: {
        inherit (self.packages.${final.system})
          eww
          alacritty-ligatures
          yabai-git;
      };
    }
    // flake-utils.lib.eachSystem [ "aarch64-darwin" ] (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          allowBroken = true;
          allowUnsupportedSystem = true;
        };
        version = "999-unstable";
      in
      {

        defaultPackage = self.packages.${system}.eww;

        packages = rec {

        eww = args.eww.defaultPackage.${system};

        alacritty-ligatures = with pkgs; (alacritty.overrideAttrs (old: rec {
            src = args.alacritty-src;

            postInstall = ''
              install -D extra/linux/Alacritty.desktop -t $out/share/applications/
              install -D extra/logo/compat/alacritty-term.svg $out/share/icons/hicolor/scalable/apps/Alacritty.svg
              strip -S $out/bin/alacritty
              patchelf --set-rpath "${lib.makeLibraryPath old.buildInputs}:${stdenv.cc.cc.lib}/lib${lib.optionalString stdenv.is64bit "64"}" $out/bin/alacritty
              installShellCompletion --zsh extra/completions/_alacritty
              installShellCompletion --bash extra/completions/alacritty.bash
              installShellCompletion --fish extra/completions/alacritty.fish
              install -dm755 "$out/share/man/man1"
              gzip -c extra/alacritty.man > "$out/share/man/man1/alacritty.1.gz"
              install -Dm644 alacritty.yml $out/share/doc/alacritty.yml
              install -dm755 "$terminfo/share/terminfo/a/"
              tic -xe alacritty,alacritty-direct -o "$terminfo/share/terminfo" extra/alacritty.info
              mkdir -p $out/nix-support
              echo "$terminfo" >> $out/nix-support/propagated-user-env-packages
            '';

            cargoDeps = old.cargoDeps.overrideAttrs (_: {
              inherit src;
              outputHash = "sha256-tY5sle1YUlUidJcq7RgTzkPsGLnWyG/3rtPqy2GklkY=";
            });
          }));

          yabai-git = (pkgs.yabai.overrideAttrs (old: {
            inherit version;
            src = args.yabai-src;
            buildInputs = (old.buildInputs or [ ]) ++ (with pkgs; [
              SkyLight xcode
            ]);
          }));
        };
      }
    );
}
# Overlays:1 ends here
