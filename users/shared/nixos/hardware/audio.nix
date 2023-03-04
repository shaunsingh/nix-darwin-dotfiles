{
  pkgs,
  lib,
  inputs,
  config,
  ...
}: {
  services.pipewire = {
    enable = true;
    pulse.enable = true;
    alsa.support32Bit = true;
    alsa.enable = true;
  };
  security.rtkit.enable = true;
}
