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
      psst
      font-manager
      komikku
      xfce.thunar
      xfce.ristretto
      xfce.tumbler
      transmission-gtk
    ]
    ++ lib.optionals isWayland [
      wayvnc
    ];
}
