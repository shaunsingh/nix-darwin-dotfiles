{ pkgs
, lib
, inputs
, ...
}: {
  imports = [
    # auto generated 
    ./hardware-configuration.nix

    # shared settings + overlays
    ../../modules/shared.nix

    # m1 support
    ../../modules/asahi.nix

    # linux specific 
    ../../modules/linux.nix
  ];
}
