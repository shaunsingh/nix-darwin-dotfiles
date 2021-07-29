#!/bin/bash

set -eu

git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d
 ~/.emacs.d/bin/doom install

echo "Doom Emacs ✅"
