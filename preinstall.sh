#!/bin/bash

echo " "
echo "Warning: the following script will install multiple applications, packages, and files on your computer. Make sure to read through the file before proceding"
echo " "
echo " "
read -n 1 -s -r -p "Press any key to continue"

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

