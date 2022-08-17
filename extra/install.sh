#!/bin/bash
sh <(curl -L https://nixos.org/nix/install) --daemon

nix-shell -p nixUnstable git

git clone --depth 1 https://github.com/shaunsingh/nix-darwin-dotfiles.git ~/nix-darwin-dotfiles/ && cd nix-darwin-dotfiles

nix build ~/nix-darwin-dotfiles\#darwinConfigurations.shaunsingh-laptop.system --extra-experimental-features nix-command --extra-experimental-features flakes
./result/sw/bin/darwin-rebuild switch --flake .#shaunsingh-laptop
