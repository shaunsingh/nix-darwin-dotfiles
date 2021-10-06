#!/bin/bash

set -eu

emacs --batch --eval "(progn (require 'org) (setq org-confirm-babel-evaluate nil) (org-babel-tangle-file \"~/.config/doom/config.org\"))"
rm -rf ~/.emacs.d ~/.config/emacs
git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.config/emacs
~/.config/emacs/bin/doom -y install

echo "Doom Emacs ✅"
