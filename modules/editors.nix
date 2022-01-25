{ pkgs, lib, home-manager, ... }:

let
  emacsSyncScript = pkgs.writeScriptBin "doom-sync-git" ''
    #!${pkgs.runtimeShell}
    export PATH=$PATH:${lib.makeBinPath [ pkgs.git pkgs.sqlite pkgs.unzip ]}
    if [ ! -d $HOME/.config/emacs/.git ]; then
      mkdir -p $HOME/.config/emacs
      git -C $HOME/.config/emacs init
    fi
    if [ $(git -C $HOME/.config/emacs rev-parse HEAD) != ${pkgs.doomEmacsRevision} ]; then
      git -C $HOME/.config/emacs fetch https://github.com/hlissner/doom-emacs.git || true
      git -C $HOME/.config/emacs checkout ${pkgs.doomEmacsRevision} || true
    fi
  '';
in
{
  home-manager.users.shauryasingh.home.packages = with pkgs; [
    (ripgrep.override { withPCRE2 = true; })
    fd
    sqlite
    gnuplot
    pandoc
    # sdcv
    (aspellWithDicts (ds: with ds; [ en en-computers en-science ]))
    tectonic
    emacsSyncScript
    # mu
    # isync
    languagetool
    neovim-nightly
    # neovide
    nodejs-16_x
  ];
}
