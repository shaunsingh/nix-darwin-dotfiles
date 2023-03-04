{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (lib.options) mkEnableOption mkOption;
  inherit (lib.modules) mkIf;
  cfg = config.programs.swaybg;
in {
  options.programs.swaybg = with lib.types; {
    enable = mkEnableOption "swaybg";

    package = mkOption {
      type = package;
      default = pkgs.swaybg;
      defaultText = literalExpression "pkgs.swaybg";
      description = ''
        swaybg package to use. Set to <code>null</code> to use the default package.
      '';
    };

    image = mkOption {
      type = path;
      description = ''
        Path to image to set as background.
      '';
    };

    mode = mkOption {
      type = enum ["stretch" "fit" "fill" "center" "tile" "solid_color"];
      default = "fill";
      description = ''
        The background mode to use for the image
      '';
    };

    systemdTarget = mkOption {
      type = str;
      default = "graphical-session.target";
      description = ''
        The systemd target that will automatically start the swaybg service.
      '';
    };
  };

  config = mkIf cfg.enable {
    systemd.user.services.swaybg = {
      Unit = {
        Description = "swaybg background service";
        PartOf = ["graphical-session.target"];
        After = ["graphical-session.target"];
      };

      Service = {
        Type = "simple";
        ExecStart = "${cfg.package}/bin/swaybg -i ${cfg.image} -m ${cfg.mode}";
        Restart = "on-failure";
      };

      Install.WantedBy = [cfg.systemdTarget];
    };
  };
}
