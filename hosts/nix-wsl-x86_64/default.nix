{ pkgs
, lib
, inputs
, ...
}: {
  imports = [
    inputs.nixos-wsl.nixosModules.wsl
  ];

  wsl = {
    enable = true;
    defaultUser = "shauryasingh";
    startMenuLaunchers = true;
    nativeSystemd = true;

    wslConf.interop.appendWindowsPath = false;

    # Enable native Docker support
    docker-native.enable = true;

    # Enable integration with Docker Desktop (needs to be installed)
    # docker-desktop.enable = true;
  };

  # enable gnome-keyring
  services.gnome = {
    gnome-keyring.enable = true;
  };

  programs.dconf.enable = true;
  security.pam.services.xdm.enableGnomeKeyring = true;

  programs.ssh.startAgent = true;
  services.openssh.ports = [ 2022 ];

  networking.hostName = "shaunsingh-nix-wsl";
}
