{ pkgs
, lib
, inputs
, ...
}: {
  security.pam.enableSudoTouchIdAuth = true;
  imports = [
    ../../modules/shared.nix
    ../../modules/darwin.nix
  ];
}
