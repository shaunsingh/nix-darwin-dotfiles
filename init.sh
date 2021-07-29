#!/bin/bash

set -eu

SYSTEM_TYPE=$(uname -s)

if [[ "$SYSTEM_TYPE" = "Darwin" || "$SYSTEM_TYPE" = "Linux" ]]; then


    if [ "$SYSTEM_TYPE" = "Linux" ]; then
        sudo apt update && sudo apt -y upgrade && sudo apt install -y build-essential procps curl file git && sudo apt -y autoremove
    fi
    
    if [ ! command -v brew >/dev/null 2>&1 ]; then
        /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
        echo "Homebrew ✅"
    fi

    if [[ "$SYSTEM_TYPE" = "Darwin" ]] && [ ! xcode-select --print-path &>/dev/null ]; then
        xcode-select --install
        echo "Xcode cli tools ✅"
    fi

    if ! brew list yadm >/dev/null; then
        brew install yadm
        echo "yadm ✅"
    fi

    if [ ! -f "$HOME/.config/yadm/bootstrap" ]; then
        yadm clone https://github.com/shaunsingh/vimrc-dotfiles.git --no-bootstrap
    fi

    yadm bootstrap
    yadm decrypt --yadm-archive "$HOME/.config/yadm/archive"

    if git config remote.faraway.url > /dev/null; then
        yadm remote add origin https://github.com/shaunsingh/vimrc-dotfiles.git
    fi

fi
