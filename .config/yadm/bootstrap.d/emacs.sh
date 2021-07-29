#!/bin/bash

set -eu

SYSTEM_TYPE=$(uname -s)

git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d
 ~/.emacs.d/bin/doom install

echo "Doom Emacs ✅"
