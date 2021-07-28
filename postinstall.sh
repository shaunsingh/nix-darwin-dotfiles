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
brew tap homebrew/cask-fonts
brew install --cask font-sf-pro
brew install --cask font-sf-mono
brew install --cask font-fira-code-nerd-font

echo "Installing Dependencies"
brew install ranger htop ripgrep
#brew install --HEAD xorpse/formulae/yabai
#brew install skhd
brew install neofetch

echo "Install doom emacs"
brew tap d12frosted/emacs-plus
brew install emacs-plus@28 --with-xwidgets --with-native-comp --with-elrumo2-icon
git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d
 ~/.emacs.d/bin/doom install

echo "setting up email"
brew install mu
brew install isync
brew install msmtp
mkdir ~/.mbsync
mu init --maildir=~/.mbsync --my-address=shaunsingh0207@gmail.com
mbsync --all

echo "setting up org"
cd
git clone https://github.com/shaunsingh/org.git

echo "Installing Latex Packages"
brew install --cask basictex
sudo tlmgr install dvipng dvisvgm l3packages xcolor soul adjustbox collectbox amsmath siunitx cancel mathalpha capt-of chemfig

#Clone bar into default Übersicht location
brew install --cask ubersicht
echo "installing bar"
cd vimrc-dotfiles
cp -r powerbar $HOME/Library/Application\ Support/Übersicht/widgets/powerbar
cd

echo "Building Neovim nightly"
brew install neovim --HEAD
nvim --headless +PackerSync +qa

echo "grabbing wallpapers"
cd ~vimrc-dotfiles
cp -R wallpapers ~

echo "Setting up fish"
brew install --cask kitty
brew install aspell fish starship

echo "Setting fish as Default Prompt"
sudo sh -c 'echo $(which fish) >> /etc/shells'
chsh -s $(which fish)

echo "Setting up iterm"
defaults write com.googlecode.iterm2.plist PrefsCustomFolder -string "~/.config/iterm2"
defaults write com.googlecode.iterm2.plist LoadPrefsFromCustomFolder -bool true

echo "Cleanup"
#killall Finder
#killall Dock
#osascript -l JavaScript -e "Application('System Events').appearancePreferences.darkMode = true"
#defaults write NSGlobalDomain _HIHideMenuBar -bool true
#defaults write com.apple.dock autohide -bool true
#brew services start yabai
#brew services start skhd
#brew services start emacs-plus@28
brew cleanup -s
echo "Done!"

echo "                Further User Setup                   "
echo "-----------------------------------------------------"
echo "                                                     "
echo "     Install Vimium and Firenvim for Vi in Chrome    "
echo "                                                     "
echo "      Wallpapers are stored in ~/wallpapers          "
echo "   Cloned dotfiles are stored in ~/vimrc-dotfiles    "
echo "                                                     "
echo "      Thats it, thanks for downloading, enjoy :)     "
