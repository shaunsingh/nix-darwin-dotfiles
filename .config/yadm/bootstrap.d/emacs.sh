#!/bin/bash

set -eu

emacs --batch --eval "(progn (require 'org) (setq org-confirm-babel-evaluate nil) (org-babel-tangle-file \"~/.config/doom/config.org\"))"
git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d
~/.emacs.d/bin/doom -y install

echo "Doom Emacs ✅"
