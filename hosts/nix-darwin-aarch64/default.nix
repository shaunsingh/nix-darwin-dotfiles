{ pkgs
, lib
, inputs
, ...
}: {
  security.pam.enableSudoTouchIdAuth = true;
  networking.hostName = "shaunsingh-nix-darwin-mbp";
  system.stateVersion = 4;
}
