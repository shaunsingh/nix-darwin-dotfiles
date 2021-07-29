#!/bin/bash

set -eu

if [ ! command -v brew >/dev/null 2>&1 ]; then
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    echo "Homebrew ✅"
fi

if [ -f "$HOME/Brewfile" ]; then
    brew bundle --file="$HOME/Brewfile"
    echo "Homebrew packages ✅"
fi
