{ pkgs
, lib
, inputs
, config
, ...
}: {
  # gpg agent
  services.gpg-agent = {
    enable = true;
    enableSshSupport = true;
  };

  # enable xdg
  xdg.enable = true;

  # xdg utilities
  home.packages = with pkgs; [
    xdg-utils
  ];
}
