{ pkgs
, lib
, inputs
, config
, ...
}: {
  imports = [
    ./audio.nix
    ./bluetooth.nix
    ./network.nix
  ];

  zramSwap.enable = true;
  services.upower.enable = true;
  services.fstrim.enable = true;
  hardware.opengl.enable = true;
}
