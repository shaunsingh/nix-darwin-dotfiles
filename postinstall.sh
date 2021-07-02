#!/bin/bash

echo " "
echo "Warning: the following script will install multiple applications, packages, and files on your computer. Make sure to read through the file before proceding"
echo "The script will take a while to run, as several tools need to run from source. I recommend leaving it overnight if you have a slower machine"
echo "Alternatively, you can Install the prebuilt releases for neovim and emacs, and run the select parts of the script manually"
echo " "
echo " "
read -n 1 -s -r -p "Press any key to continue"

echo "Install homebrew"
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

echo "Enrolling in homebrew dev branch"
export HOMEBREW_DEVELOPER=1

echo "Installing fonts"
brew install --cask font-alegreya
brew install --cask font-sf-mono
brew install --cask font-fira-code-nerd-font

echo "Installing Dependencies"
nix-env -iA nixpkgs.aspellDicts.en nixpkgs.cowsay nixpkgs.fish nixpkgs.fortune nixpkgs.ranger nixpkgs.htop nixpkgs.ripgrep nixpkgs.starship
brew install yabai
brew install skhd

echo "Install doom emacs"
brew tap d12frosted/emacs-plus
brew install emacs-plus@28 --with-xwidgets --with-native-comp --with-elrumo2-icon
git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d
 ~/.emacs.d/bin/doom install
doom sync

# echo "Installing Latex Packages"
brew install --cask basictex
sudo tlmgr install dvipng dvisvgm l3packages xcolor soul adjustbox collectbox amsmath siunitx cancel mathalpha capt-of chemfig

#Clone bar into default Übersicht location
brew install --cask ubersicht
echo "installing bar"
git clone --depth 1 https://github.com/shaunsingh/zenbar $HOME/Library/Application\ Support/Übersicht/widgets/zenbar

echo "Building Neovim nightly"
brew install luajit --HEAD
brew install neovim --HEAD
nvim --headless +PackerSync +qa

echo "Syncing vim"
nix-env -iA nixpkgs.vim
nix-env -iA nixpkgs.vimPlugins.nord-vim
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
defaults write NSGlobalDomain _HIHideMenuBar -bool true
defaults write com.apple.dock autohide -bool true
brew services start yabai
brew services start skhd
brew cleanup -s
nix-collect-garbage
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
