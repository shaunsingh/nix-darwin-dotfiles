{ pkgs, modulesPath, ... }:

{
  imports = [ "${modulesPath}/profiles/minimal.nix" ];

  wsl = {
    enable = true;
    defaultUser = "shaurizard";
    startMenuLaunchers = true;
    wslConf.automount.root = "/mnt";
  };

  hardware.opengl.enable = true;

  environment.systemPackages = with pkgs; [
    git
    home-manager
  ];
}