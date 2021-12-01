#!/bin/bash
# Installing and notes
# *NOTE: These are available as an executable script [[./extra/install.sh]]*

# Install Nix. I have it setup for multi-user, but you can remove the =--daemon= if you want a single user install

# [[file:../nix-config.org::*Installing and notes][Installing and notes:1]]
    sh <(curl -L https://nixos.org/nix/install) --daemon
# Installing and notes:1 ends here


# Launch an ephemeral shell with git, nixUnstable, and Emacs

# [[file:../nix-config.org::*Installing and notes][Installing and notes:2]]
    nix-shell -p nixUnstable git emacs
# Installing and notes:2 ends here


# Tangle the =.org= files (not needed, but recommend in case I forgot to update tangled files)

# [[file:../nix-config.org::*Installing and notes][Installing and notes:3]]
    git clone --depth 1 https://github.com/shaunsingh/nix-darwin-dotfiles.git ~/nix-darwin-dotfiles/ && cd nix-darwin-dotfiles
    emacs --batch --eval "(progn (require 'org) (setq org-confirm-babel-evaluate nil) (org-babel-tangle-file \"~/nix-darwin-dotfiles/nix-config.org\"))"
    emacs --batch --eval "(progn (require 'org) (setq org-confirm-babel-evaluate nil) (org-babel-tangle-file \"~/nix-darwin-dotfiles/configs/doom/config.org\"))"
# Installing and notes:3 ends here


# 	(if emacs asks you for comment syntax, put `# ` for everything)
# Build, and switch to the dotfiles

# [[file:../nix-config.org::*Installing and notes][Installing and notes:4]]
    nix build ~/nix-darwin-dotfiles\#darwinConfigurations.shaunsingh-laptop.system --extra-experimental-features nix-command --extra-experimental-features flakes
    ./result/sw/bin/darwin-rebuild switch --flake .#shaunsingh-laptop
# Installing and notes:4 ends here


# (note, =--extra-experimental-features= is only needed the first time around. After that the configuration will edit =/etc/nix/nix.conf= to enable flakes and nix-command by default)
# Symlinking with nix (and managing doom with =nix-doom-emacs=) is very finicky, so for now we need to manually symlink them

# [[file:../nix-config.org::*Installing and notes][Installing and notes:5]]
    ln -s ~/nix-darwin-dotfiles/configs/doom/ ~/.config/doom
# Installing and notes:5 ends here


# Install doom emacs

# [[file:../nix-config.org::*Installing and notes][Installing and notes:6]]
    git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.config/emacs
    ~/.config/emacs/bin/doom install
# Installing and notes:6 ends here
