{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.sketchybar;

  configHome = pkgs.writeTextFile {
    name = "sketchybarrc";
    text = cfg.config;
    destination = "/sketchybar/sketchybarrc";
    executable = true;
  };
in

{
  options = with types; {
    services.sketchybar.enable = mkOption {
      type = bool;
      default = false;
      description = "Whether to enable the sketchybar";
    };

    services.sketchybar.package = mkOption {
      type = path;
      description = "The sketchybar package to use.";
    };

    services.sketchybar.config = mkOption {
      type = str;
      default = "";
      example = literalExpression ''
        sketchybar -m --bar height=25
        echo "sketchybar configuration loaded.."
      '';
      description = ''
        Configuration.
      '';
    };
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [ cfg.package ];

    launchd.user.agents.sketchybar = {
      serviceConfig.ProgramArguments = [ "${cfg.package}/bin/sketchybar" ];

      serviceConfig.KeepAlive = true;
      serviceConfig.RunAtLoad = true;
      serviceConfig.EnvironmentVariables = {
        PATH = "${cfg.package}/bin:${config.environment.systemPath}";
        XDG_CONFIG_HOME = mkIf (cfg.config != "") "${configHome}";
      };
    };
  };
}
