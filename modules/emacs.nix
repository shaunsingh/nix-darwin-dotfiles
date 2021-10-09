{ pkgs, lib, config, ... }: {
  system.activationScripts.postUserActivation.text = ''
     # Clone to $XDG_CONFIG_HOME because Emacs expects this location.
     if [[ ! -d "~/.config/emacs" ]]; then
       git clone --depth 1 https://github.com/hlissner/doom-emacs "~/.config/emacs"
     fi
     # Temporary while I figure out a better solution
     ln -s ~/nix-darwin-dotfiles/configs/doom/ ~/.config/doom
   '';
  # home-manager.users.shauryasingh.home.file."~/.config/doom".source = config.lib.file.mkOutOfStoreSymlink ../configs/doom;
  home-manager.users.shauryasingh.home.packages = with pkgs; [
    (ripgrep.override { withPCRE2 = true; })
    gnutls
    fd
    gnuplot
    sqlite
    (aspellWithDicts (dicts: with dicts; [ en en-computers en-science ]))
    (pkgs.texlive.combine { inherit (pkgs.texlive) scheme-small dvipng dvisvgm l3packages xcolor soul adjustbox collectbox amsmath siunitx cancel mathalpha capt-of chemfig wrapfig mhchem fvextra cleveref latexmk tcolorbox environ; })
    sdcv
    mu
    msmtp
    isync
  ];
}
