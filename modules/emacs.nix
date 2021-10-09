{ config, pkgs, libs, emacs-overlay, nix-doom-emacs, ... }: {
  imports = [ nix-doom-emacs.hmModule ];
  home-manager.users.shauryasingh.home.programs.doom-emacs = {
    enable = true;
    doomPrivateDir = ../configs/doom;
    emacsPackage = pkgs.emacsGcc;
  };
  home-manager.users.shauryasingh.home.file.".doom.d".source = ../configs/doom;
  fonts.fonts = with pkgs; [
    emacs-all-the-icons-fonts
  ];
  services.emacs = {
      enable = true;
      package = pkgs.doom-emacs;
  };
  home-manager.users.shauryasingh.home.packages = with pkgs; [
    (ripgrep.override { withPCRE2 = true; })
    gnutls
    fd
    gnuplot
    sqlite
    emacsGcc
  ];
}
