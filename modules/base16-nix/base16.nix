{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.themes.base16;
  inherit (builtins) pathExists;

  # The theme yaml files only supply 16 hex values, but the templates take
  # a transformation of this data such as rgb. The hacky python script pre-
  # processes the theme file in this way for consumption by the mustache
  # engine below.
  python3 = pkgs.python3.withPackages (ps: [ ps.pyyaml ]);
  preprocess = src:
    pkgs.stdenv.mkDerivation {
      name = "yaml";
      inherit src;
      builder = pkgs.writeText "builder.sh" ''
        slug_all=$(${pkgs.coreutils}/bin/basename $src)
        slug=''${slug_all%.*}
        ${python3}/bin/python3 ${./base16writer.py} $slug < $src > $out
      '';
      allowSubstitutes = false;  # will never be in cache
    };

  # process the user's input theme
  schemeJSONCustom = schemePath:
    importJSON (preprocess schemePath);

in {
  options = with types; {
    themes.base16 = {
      enable = mkEnableOption "Base 16 Color Schemes";
      path = mkOption {
        type = nullOr path;
        default = null;
      };
      extraParams = mkOption {
        type = attrsOf anything;
        default = {};
      };
    };
  };

  config = { lib.base16.theme = schemeJSONCustom cfg.path // cfg.extraParams; };
}
