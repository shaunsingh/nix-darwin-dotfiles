{
  pkgs,
  lib,
  inputs,
  config,
  ...
}: {
  programs.obs-studio = {
    enable = true;
    plugins = with pkgs.obs-studio-plugins; [
      wlrobs
      obs-pipewire-audio-capture
    ];
  };
}
