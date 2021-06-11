# This is an installer of all the dependencies and my dotfiles
# Prerequisites to all of the files
# Homebrew PKGs moved to brewdepen.sh
echo "Installing Homebrew and it's custom packages"
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"

echo "Enrolling in homebrew dev branch"
export HOMEBREW_DEVELOPER=1

#Installing Packages
brew tap homebrew/cask-fonts
brew tap homebrew/cask
brew tap homebrew/bundle
brew tap homebrew/core
brew tap homebrew/services
brew tap railwaycat/emacsmacport

brew install --cask font-fira-code-nerd-font
brew install --cask ubersicht
brew install --cask basictex
#brew install --cask alacritty

brew install ranger
brew install luajit --HEAD
brew install neovim --HEAD
brew install ripgrep
brew install aspell
brew install python@3.9
brew install starship
brew install zsh-autosuggestions
brew install zsh-syntax-highlighting
brew install emacs-mac --with-emacs-big-sur-icon --with-no-title-bars --HEAD

brew services start yabai
brew services start skhd
brew update
brew upgrade
brew cleanup

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

echo "start ubersicht"
brew services start ubersicht

echo "Installing Dotfiles from Cloned Repository"
cp -R .config ~
cp .zshrc .shkdrc .yabairc .gitconfig ~
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
echo "                restart alacritty                    "
echo "   You can re-run doom sync to sync emacs plugins    "
echo "  You can re-run :PackerSync to sync neovim plugins  "
echo "       Thats it, thanks for downloading, enjoy :)    "
