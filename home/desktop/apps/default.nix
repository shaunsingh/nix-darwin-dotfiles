{
  pkgs,
  lib,
  inputs,
  config,
  isWayland,
  ...
}: {
  imports = [
    ./discord.nix
    # ./obs-studio.nix
    ./zathura.nix
  ];

  home.packages = with pkgs;
    [
      # spotify client
      psst
      # font viewer
      font-manager
      # manga
      komikku
      # file manager
      xfce.thunar
      # image viewer
      xfce.ristretto
      xfce.tumbler
      # torrent
      transmission-gtk
    ]
    ++ lib.optionals isWayland [
      # vnc client
      wayvnc
    ];
}
