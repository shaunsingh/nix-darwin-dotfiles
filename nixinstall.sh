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

echo "Install Nix"
curl -L https://nixos.org/nix/install --darwin-use-unencrypted-nix-store-volume

echo " "
echo "Nix-darwin will ask you if you want to edit configuration.nix. Either take a look and :q, or hit n (no)"
echo " "
read -n 1 -s -r -p "Press any key to continue"

echo "Install Nix-darwin"
nix-build https://github.com/LnL7/nix-darwin/archive/master.tar.gz -A installer
n | ./result/bin/darwin-installer

# echo "Install doom emacs"
git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d
 ~/.emacs.d/bin/doom install

# echo "Installing Latex Packages"
# sudo tlmgr install dvipng dvisvgm l3packages xcolor soul adjustbox collectbox amsmath siunitx cancel mathalpha

#Clone bar into default Übersicht location
echo "installing bar"
git clone --depth 1 https://github.com/shaunsingh/zenbar $HOME/Library/Application\ Support/Übersicht/widgets/zenbar

# echo "Syncing emacs"
doom sync

# echo "Syncing Neovim"
nvim --headless +PackerSync +qa

echo "Syncing vim"
vim -es -u vimrc -i NONE -c "PlugInstall" -c "qa"

echo "grabbing wallpapers"
cd ~vimrc-dotfiles
cp -R wallpapers ~

echo "Setting up fish"
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
