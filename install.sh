# This is an installer of all the dependencies and my dotfiles
# Prerequisites to all of the files
# Homebrew PKGs moved to brewdepen.sh
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
#brew tap railwaycat/emacsmacport

echo "Installing fonts"
brew install --cask font-fira-code-nerd-font
brew install --cask font-sf-mono
brew install --cask font-roboto-mono-nerd-font
brew install --cask font-alegreya

echo "Installing apps"
brew install --cask ubersicht
brew install --cask basictex
brew install --cask alacritty
brew install --cask intellij-idea

echo "Installing Formula"
brew install mas
brew install ranger
brew install ripgrep
brew install aspell
brew install fish
brew install starship

echo "Building/Installing Neovim nightly"
brew install luajit --HEAD
brew install neovim --HEAD

echo "Building/Installing Emacs native comp"
brew install emacs-plus@28 --with-xwidgets --with-native-comp --with-no-titlebar --with-elrumo1-icon
#brew install emacs-mac --with-emacs-big-sur-icon --with-no-title-bars --HEAD

echo "Cleanup"
brew services start yabai
brew services start skhd
brew update
brew upgrade
brew cleanup

echo "setting up fish"
sudo sh -c 'echo $(which fish) >> /etc/shells'
chsh -s $(which fish)
set -U fish_user_paths $(which brew) $fish_user_paths

echo "Installing Apps from MAS"
mas install 1480933944
mas upgrade

echo "Install doom emacs"
git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d
~/.emacs.d/bin/doom install

echo "Installing Latex Packages"
sudo tlmgr install dvipng dvisvgm l3packages xcolor soul adjustbox collectbox amsmath siunitx cancel mathalpha

#Clone Dotfile Repo
echo "Cloning Dotfiles"
git clone https://github.com/shaunsingh/vimrc-dotfiles.git

#Clone bar into default Übersicht location
git clone https://github.com/zzzeyez/pecan.git "$HOME/Library/Application Support/Übersicht/widgets/pecan"
cd vimrc-dotfiles

echo "Installing Dotfiles from Cloned Repository"
cp -R .config ~
cp .zshrc .skhdrc .yabairc .gitconfig ~
cp -R .doom.d ~

echo "Installing and syncing emacs"
doom sync -u

echo "Installing and syncing Neovim"
nvim --headless +PackerSync +qa

echo "grabbing wallpapers"
cp -R wallpapers ~

echo "Done!"

echo "                Further User Setup                   "
echo "-----------------------------------------------------"
echo "                                                     "
echo "       You can copy over colors.css for pecan        "
echo "   You can re-run doom sync to sync emacs plugins    "
echo "  You can re-run :PackerSync to sync neovim plugins  "
echo "                                                     "
echo "      Thats it, thanks for downloading, enjoy :)     "
