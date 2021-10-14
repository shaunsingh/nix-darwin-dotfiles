{ pkgs, lib, config, ... }: {
  system.activationScripts.postUserActivation.text = ''
    # Clone to $XDG_CONFIG_HOME because Emacs expects this location.
    if [[ ! -d "~/.config/emacs" ]]; then
      git clone --depth 1 https://github.com/hlissner/doom-emacs "~/.config/emacs"
    fi
  '';
  fonts = {
    enableFontDir = true;
    fonts = with pkgs; [ alegreya overpass ];
  };
  # services.mbsync.enable = true;
  # services.mbsync.postExec = ''
  #   if pgrep -f 'mu server'; then
  #       ${config.home-manager.users.cmacrae.programs.emacs.package}/bin/emacsclient \
  #         -e '(mu4e-update-index)'
  #   else
  #       ${pkgs.mu}/bin/mu index --nocolor
  #   fi
  # '';
  home-manager.users.shauryasingh.home.packages = with pkgs; [
    gnutls
    gnuplot
    sqlite
    (aspellWithDicts (dicts: with dicts; [ en en-computers en-science ]))
    (pkgs.texlive.combine {
      inherit (pkgs.texlive)
        scheme-small dvipng dvisvgm l3packages xcolor soul adjustbox collectbox
        amsmath siunitx cancel mathalpha capt-of chemfig wrapfig mhchem fvextra
        cleveref latexmk tcolorbox environ;
    })
    sdcv
    emacs
  ];
}
