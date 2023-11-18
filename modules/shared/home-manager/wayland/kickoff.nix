{ config
, lib
, pkgs
, ...
}:
with lib; let
  cfg = config.programs.kickoff;
  tomlFormat = pkgs.formats.toml { };
in
{
  options = {
    programs.kickoff = {
      enable =
        mkEnableOption ''
          wayland launcher'';

      package = mkOption {
        type = types.package;
        default = pkgs.kickoff;
        defaultText = literalExpression "pkgs.kickoff";
        description = "Package providing <command>kickoff</command>.";
      };

      settings = mkOption {
        inherit (tomlFormat) type;
        default = { };
        description = ''
          Configuration written to
          <filename>$XDG_CONFIG_HOME/kickoff/config.toml</filename>
          </para><para>
          See <link xlink:href="https://github.com/j0ru/kickoff/blob/main/assets/default_config.toml"/>
          for the default configuration.
        '';
        example = literalExpression ''
          {
            font_size = 32.0;
            colors = {
              background = "#282c34aa";
              prompt = "#abb2bfff";
            };
          }
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    home.packages = [ cfg.package ];

    xdg.configFile."kickoff/config.toml" = mkIf (cfg.settings != { }) {
      source = tomlFormat.generate "config.toml" cfg.settings;
    };
  };
}
