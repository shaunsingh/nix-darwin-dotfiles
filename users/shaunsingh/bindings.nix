{
  pkgs,
  lib,
  inputs,
  config,
  isWayland,
  withSway,
  ...
}: {
  imports = [../../modules/nixos/services/xremap];
  services.xremap = {
    inherit withSway;
    serviceMode = "user";
    userName = "shauryasingh";
    watch = true;
    config = {
      modmap = [
        {
          name = "global";
          remap = {
            "CapsLock" = {
              held = "Meta_L";
              alone = "Esc";
              alone_timeout_millis = 200;
            };
          };
        }
      ];
      keymap =
        [
          {
            name = "global";
            remap = {
              "Super-f" = {launch = "['${pkgs.firefox}/bin/firefox']";};
            };
          }
        ]
        ++ lib.optionals isWayland [
          {
            name = "wayland";
            remap = {
              # "Super-Enter" = { launch = "['${pkgs.foot}/bin/foot']"; };
              "C-w" = {launch = "['${pkgs.foot}/bin/foot']";};
            };
          }
        ];
    };
  };
}
