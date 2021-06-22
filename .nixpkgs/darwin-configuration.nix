{ config, lib, pkgs, ... }:

{
  system.defaults.NSGlobalDomain._HIHideMenuBar = true;
  system.defaults.dock.autohide = true;
  system.defaults.dock.showhidden = true;
  system.defaults.finder.AppleShowAllExtensions = true;

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  
  environment.systemPackages = with pkgs; [ vim cowsay lolcat curl git htop ripgrep neofetch alacritty starship fortune fish ranger aspell neovim-nightly emacsMacport ];

  services = {
    yabai.enable = true;
    skhd.enable = true;
  };

  nixpkgs.config.allowUnfree = true;

  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url = https://github.com/nix-community/neovim-nightly-overlay/archive/master.tar.gz;
    }))
  ];

  # Use a custom configuration.nix location.
  # $ darwin-rebuild switch -I darwin-config=$HOME/.config/nixpkgs/darwin/configuration.nix
  # environment.darwinConfig = "$HOME/.config/nixpkgs/darwin/configuration.nix";

  # Auto upgrade nix package and the daemon service.
  # services.nix-daemon.enable = true;
  nix.package = pkgs.nixUnstable;

  # Create /etc/bashrc that loads the nix-darwin environment.
  programs.zsh.enable = true;  # default shell on catalina
  programs.fish.enable = true;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
}
