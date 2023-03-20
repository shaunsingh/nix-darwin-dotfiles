{ pkgs
, lib
, inputs
, config
, isWayland
, ...
}: {
  imports = [
    ./discord.nix
    # ./obs-studio.nix
    ./zathura.nix
  ];

  home.packages = with pkgs;
    [
      psst
      zotero
      komikku
      xfce.thunar
      xfce.tumbler
      font-manager
      xfce.ristretto
      transmission-gtk
    ]
    ++ lib.optionals isWayland [
      wayvnc
    ];
}
