#!/bin/bash

set -eu

if [ ! command -v nix-env >/dev/null 2>&1 ]; then
    curl -L https://nixos.org/nix/install | sh -s -- --darwin-use-unencrypted-nix-store-volume --daemon
    nix-channel --add https://channels.nixos.org/nixpkgs-unstable/ unstable
    sudo nix-channel --update
    echo "Nix ✅"
fi

if [ -f "$HOME/.nixpkgs/darwin-configuration.nix" ]; then
    nix-build https://github.com/LnL7/nix-darwin/archive/master.tar.gz -A installer
    ./result/bin/darwin-installer
    echo "Nix-Darwin ✅"
fi
