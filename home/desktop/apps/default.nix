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
      # email client
      thunderbird
      # font viewer
      font-manager
    ]
    ++ lib.optionals isWayland [
      # vnc client
      wayvnc
    ];
}
