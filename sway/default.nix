{
  inputs,
  lib,
  config,
  pkgs,
  ...
}: {
  home-manager.users.nyxtkiosk = import ./home.nix;
}
