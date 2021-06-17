#!/bin/bash

echo " "
echo "Warning: the following script will install multiple applications, packages, and files on your computer. Make sure to read through the file before proceding"
echo "The script will take a while to run, as several tools need to run from source. I recommend leaving it overnight if you have a slower machine"
echo "Alternatively, you can Install the prebuilt releases for neovim and emacs, and run the select parts of the script manually"
echo " "
echo " "
read -n 1 -s -r -p "Press any key to continue"

echo "Installing Homebrew and it's custom packages"
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"

echo "Enrolling in homebrew dev branch"
export HOMEBREW_DEVELOPER=1

echo "Tapping packages"
brew tap homebrew/cask-fonts
brew tap homebrew/cask
brew tap homebrew/bundle
brew tap homebrew/core
brew tap homebrew/services
brew tap d12frosted/emacs-plus

echo "Installing fonts"
brew install --cask font-alegreya
brew install --cask font-sf-mono
brew install --cask font-fira-code-nerd-font
brew install --cask font-roboto-mono-nerd-font

# echo "Installing other apps"
# brew install --cask intellij-idea
# brew install --cask yt-music
# brew install --cask vscodium

echo "Installing Dependencies"
brew install ranger
brew install ripgrep
brew install aspell
# brew install anaconda

echo "Building Neovim nightly"
brew install luajit --HEAD
brew install neovim --HEAD

# echo "Installing Safari Extensions"
# brew install mas
# mas install 1480933944
# mas install 1440147259

echo "Install doom emacs"
brew install emacs-plus@28 --with-xwidgets --with-native-comp --with-elrumo2-icon
git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d
~/.emacs.d/bin/doom install

echo "Installing Latex Packages"
brew install --cask basictex
sudo tlmgr install dvipng dvisvgm l3packages xcolor soul adjustbox collectbox amsmath siunitx cancel mathalpha

#Clone bar into default Übersicht location
echo "installing bar"
brew install --cask ubersicht
git clone --depth 1 https://github.com/zzzeyez/pecan.git "$HOME/Library/Application Support/Übersicht/widgets/pecan"
cd vimrc-dotfiles

#Clone Dotfile Repo
echo "Cloning Dotfiles"
git clone --depth 1 https://github.com/shaunsingh/vimrc-dotfiles.git

echo "Installing Dotfiles from Cloned Repository"
cp -R .config ~
cp -R .doom.d ~
cp -R .vim ~
cp .zshrc .skhdrc .yabairc .ideavimrc .vimrc .gitconfig ~

echo "Syncing emacs"
doom sync

echo "Syncing Neovim"
nvim --headless +PackerSync +qa

echo "Syncing vim"
vim -es -u vimrc -i NONE -c "PlugInstall" -c "qa"

echo "grabbing wallpapers"
cp -R wallpapers ~

echo "Cleanup"
brew services start yabai
brew services start skhd
# brew update
# brew upgrade
brew cleanup -s
# cd
# rm -R vimrc-dotfiles

echo "Setting up fish"
brew install --cask alacritty
brew install fish
brew install starship
brew install fortune cowsay
echo "Setting fish as Default Prompt"
sudo sh -c 'echo $(which fish) >> /etc/shells'
chsh -s $(which fish)

echo "Done!"

echo "                Further User Setup                   "
echo "-----------------------------------------------------"
echo "                                                     "
echo "                 reboot and run                      "
echo "set -U fish_user_paths $(which brew) $fish_user_paths" 
echo "              to configure homebrew                  "
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
