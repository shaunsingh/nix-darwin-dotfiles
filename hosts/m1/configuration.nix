{ pkgs
, lib
, inputs
, ...
}: {
  security.pam.enableSudoTouchIdAuth = true;
}
