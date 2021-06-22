#!/bin/bash

#Clone Dotfile Repo
echo "Cloning Dotfiles"
git clone --depth 1 https://github.com/shaunsingh/vimrc-dotfiles.git
cd vimrc-dotfiles

echo "Installing Dotfiles from Cloned Repository"
cp -u -R .config ~
cp -u -R .doom.d ~
cp -u -R .vim ~
cp -u -R .nixpkgs ~
cp -u .zshrc .skhdrc .yabairc .ideavimrc .vimrc .gitconfig ~

