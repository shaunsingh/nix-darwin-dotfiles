{ pkgs
, lib
, inputs
, config
, ...
}: {
  imports = [
    ./console.nix
    ./env.nix
    ./locale.nix
    ./fonts.nix
    ./xdg.nix
  ];

  programs.dconf.enable = true;
  services.gnome.at-spi2-core.enable = true;
  services.gnome.gnome-keyring.enable = true;
  security.pam.services.login.enableGnomeKeyring = true;
  services.tor = {
    enable = true;
    client.enable = true;
  };
}
