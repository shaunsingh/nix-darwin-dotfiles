#!/bin/bash

set -eu

SYSTEM_TYPE=$(uname -s)

POWERBAR_FOLDER="$HOME/Library/Application Support/Übersicht/widgets/powerbar"

if [ "$SYSTEM_TYPE" = "Darwin" ]; then
    if [ ! -d "$SIMPLE_BAR_FOLDER" ]; then
        git clone https://github.com/shaunsingh/powerbar "$POWERBAR_FOLDER"
    fi

    echo "powerbar ✅"
fi
