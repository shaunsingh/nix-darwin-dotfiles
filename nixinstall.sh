#!/bin/bash

echo " "
echo "Warning: the following script will install multiple applications, packages, and files on your computer. Make sure to read through the file before proceding"
echo " "
echo " "
read -n 1 -s -r -p "Press any key to continue"

echo "Install Nix"
curl -L https://nixos.org/nix/install --darwin-use-unencrypted-nix-store-volume

echo "Install Nix-darwin"
nix-build https://github.com/LnL7/nix-darwin/archive/master.tar.gz -A installer
./result/bin/darwin-installer

echo "Installing Dependencies"
nix-env -iA nixpkgs.ranger
nix-env -iA nixpkgs.ripgrep
nix-env -iA nixpkgs.aspell
nix-env -iA nixpkgs.yabai
nix-env -iA nixpkgs.skhd

echo "Installing Neovim nightly"
nix-env -iA nixpkgs.neovim

echo "Install doom emacs"
nix-env -iA nixpkgs.emacsMacport
git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d
~/.emacs.d/bin/doom install

echo "Installing Latex Packages"
nix-env -iA nixpkgs.texlive.combined.scheme-med
# sudo tlmgr install dvipng dvisvgm l3packages xcolor soul adjustbox collectbox amsmath siunitx cancel mathalpha

#Clone bar into default Übersicht location
echo "installing bar"
# brew install --cask ubersicht
# git clone --depth 1 https://github.com/shaunsingh/zenbar $HOME/Library/Application\ Support/Übersicht/widgets/zenbar
defaults write NSGlobalDomain _HIHideMenuBar -bool true
defaults write com.apple.dock autohide -bool true
cd vimrc-dotfiles

#Clone Dotfile Repo
echo "Cloning Dotfiles"
git clone --depth 1 https://github.com/shaunsingh/vimrc-dotfiles.git

echo "Installing Dotfiles from Cloned Repository"
cp -u -R .config ~
cp -u -R .doom.d ~
cp -u -R .vim ~
cp -u .zshrc .skhdrc .yabairc .ideavimrc .vimrc .gitconfig ~

echo "Syncing emacs"
doom sync

echo "Syncing Neovim"
nvim --headless +PackerSync +qa

echo "Syncing vim"
vim -es -u vimrc -i NONE -c "PlugInstall" -c "qa"

echo "grabbing wallpapers"
cd ~vimrc-dotfiles
cp -R wallpapers ~

echo "Setting up fish"
nix-env -iA nixpkgs.alacritty
nix-env -iA nixpkgs.fish
nix-env -iA nixpkgs.starship
nix-env -iA nixpkgs.fortune
nix-env -iA nixpkgs.cowsay

cd ~
git clone https://github.com/oh-my-fish/plugin-foreign-env.git

echo "Setting fish as Default Prompt"
sudo sh -c 'echo $(which fish) >> /etc/shells'
chsh -s $(which fish)

echo "Cleanup"
killall Finder
killall Dock
osascript -l JavaScript -e "Application('System Events').appearancePreferences.darkMode = true"

echo "Done!"

echo "                Further User Setup                   "
echo "-----------------------------------------------------"
echo "                                                     "
echo "       You can copy over colors.css for pecan        "
echo "   You can re-run doom sync to sync emacs plugins    "
echo "  You can re-run :PackerSync to sync neovim plugins  "
echo "     Install Vimium and Firenvim for Vi in Chrome    "
echo "                                                     "
echo "      Wallpapers are stored in ~/wallpapers          "
echo "   Cloned dotfiles are stored in ~/vimrc-dotfiles    "
echo "                                                     "
echo "      Thats it, thanks for downloading, enjoy :)     "
