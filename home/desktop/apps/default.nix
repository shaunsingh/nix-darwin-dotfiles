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
  ];

  home.packages = with pkgs;
    [
      # spotify client
      psst
      # font viewer
      font-manager
      # manga
      komikku
    ]
    ++ lib.optionals isWayland [
      # vnc client
      wayvnc
    ];
}
