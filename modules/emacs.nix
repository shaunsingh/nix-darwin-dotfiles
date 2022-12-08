{ pkgs, lib, config, home-manager, inputs, ... }: {
  home.packages = with pkgs; [
    # 29 + xwidgets (emacs-mac +sqlite +vterm) 
    # emacs-mac
    (ripgrep.override { withPCRE2 = true; })
    gnutls # for TLS connectivity
    fd # faster projectile indexing
    zstd # for undo-fu-session/undo-tree compression
    (aspellWithDicts (ds: with ds; [ en en-computers en-science ]))
    sqlite
    sdcv
    gnuplot
    tectonic
    languagetool
  ];
}
