{
  config,
  pkgs,
  lib,
  ...
}:
with lib; let
  cfg = config.programs.river;
in {
  options.programs.river.enable = mkEnableOption "river, a dynamic tiling Wayland compositor";

  config = mkIf cfg.enable {
    environment.systemPackages = [pkgs.river];
    hardware.opengl.enable = mkDefault true;
    programs.xwayland.enable = mkDefault true;
  };
}
