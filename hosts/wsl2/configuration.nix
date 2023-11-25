{ pkgs, modulesPath, ... }:

{
  imports = [ "${modulesPath}/profiles/minimal.nix" ];

  wsl = {
    enable = true;
    defaultUser = "nixos";
    startMenuLaunchers = true;
    wslConf.automount.root = "/mnt";
  };

  users.users.nixos = {
    isNormalUser = true;
    home = "/home/nixos";
    shell = pkgs.fish;

    extraGroups = [
      "wheel"
    ];
  };

  hardware.opengl.enable = true;

  environment.systemPackages = with pkgs; [
    git
    home-manager
  ];
}