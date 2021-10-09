{ pkgs, emacs-overlay, nix-doom-emacs, ... }: {
  # imports = [ nix-doom-emacs.hmModule ];
  # home-manager.users.shauryasingh.home.programs.doom-emacs = {
    # enable = true;
    # doomPrivateDir = ../configs/doom;
    # emacsPackage = pkgs.emacsGcc;
  # };
  # home-manager.users.shauryasingh.home.file.".doom.d".source = ../configs/doom;
  system.activationScripts.postUserActivation.text = ''
     # Clone to $XDG_CONFIG_HOME because Emacs expects this location.
     if [[ ! -d "/Users/shauryasingh/.config/emacs" ]]; then
       git clone --depth 1 https://github.com/hlissner/doom-emacs "/Users/shaurysingh/.config/emacs"
     fi
   '';
  fonts.fonts = with pkgs; [
    emacs-all-the-icons-fonts
  ];
  services.emacs = {
      enable = true;
      # package = pkgs.doom-emacs;
      package = pkgs.emacsGcc;
  };
  home-manager.users.shauryasingh.home.packages = with pkgs; [
    (ripgrep.override { withPCRE2 = true; })
    gnutls
    fd
    gnuplot
    sqlite
    emacsGcc
    (aspellWithDicts (dicts: with dicts; [ en en-computers en-science ]))
    (pkgs.texlive.combine { inherit (pkgs.texlive) scheme-small dvipng dvisvgm l3packages xcolor soul adjustbox collectbox amsmath siunitx cancel mathalpha capt-of chemfig wrapfig mhchem fvextra latexmk cleveref; })
    sdcv
    mu
    msmtp
    isync
  ];
}
