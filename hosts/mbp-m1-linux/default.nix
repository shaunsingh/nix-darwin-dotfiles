{ pkgs
, lib
, inputs
, ...
}: {
  imports = [
    ./hardware-configuration.nix
    ../../modules/shared.nix
    ../../modules/m1support.nix
    ../../modules/linux.nix
  ];
}
