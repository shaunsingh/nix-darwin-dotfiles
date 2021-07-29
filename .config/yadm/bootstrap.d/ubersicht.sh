#!/bin/bash

set -eu

SYSTEM_TYPE=$(uname -s)

POWERBAR_FOLDER="$HOME/Library/Application Support/Übersicht/widgets/powerbar"

if [ "$SYSTEM_TYPE" = "Darwin" ]; then
    brew install --cask ubersicht
    if [ ! -d "$POWERBAR_FOLDER" ]; then
        git clone https://github.com/shaunsingh/powerbar "$POWERBAR_FOLDER"
    fi
    echo "powerbar ✅"
fi
