#!/bin/bash

#Clone Dotfile Repo
#echo "Cloning Dotfiles"
#git clone --depth 1 https://github.com/shaunsingh/vimrc-dotfiles.git
cd
cd vimrc-dotfiles

echo "Installing Dotfiles from Cloned Repository"
cp -u -R .config ~
cp -u -R .doom.d ~
cp -u .vimrc .gitconfig ~
