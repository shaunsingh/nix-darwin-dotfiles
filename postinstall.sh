
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
