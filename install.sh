# This is an installer of all the dependencies and my dotfiles
# Prerequisites to all of the files
# Homebrew PKGs moved to brewdepen.sh
echo "Installing Homebrew and it's custom packages"
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"

#Installing Packages
brew tap homebrew/cask-fonts
brew tap homebrew/cask
brew tap homebrew/bundle
brew tap homeberw/core
brew tap homebrew/services
brew tap k-vernooy/tap
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
brew services restart --all
brew cleanup

echo "Installing Latex Packages"
sudo tlmgr install dvipng dvisvgm l3packages xcolor soul adjustbox collectbox amsmath siunitx cancel mathalpha

#Clone Dotfile Repo
echo "Cloning Dotfiles"
git clone https://github.com/shaunsingh/vimrc-dotfiles.git

#Clone bar into default Übersicht location
git clone https://github.com/zzzeyez/pecan.git "$HOME/Library/Application Support/Übersicht/widgets/pecan"
cd vimrc-dotfiles

echo "Creating a backup of all configs in ~/config_backup"

mkdir $HOME/config_backup

# following code is stolen from here: https://blog.sleeplessbeastie.eu/2013/03/12/simple-shell-script-to-backup-selected-directories/
# Parent backup directory
backup_parent_dir="$HOME/config_backup"

# Directories to backup
backup_me="$HOME/scripts/ $HOME/ .config/ $HOME/bin/ $HOME/spicetify_data/ $HOME/.zshrc $HOME/.skhdrc $HOME/.Xresources $HOME/Pictures $HOME/.yabairc"

# Check and create backup directory
backup_date=`date +%Y_%m_%d_%H_%M`
backup_dir=${backup_parent_dir}/configs_${backup_date}
mkdir -p $backup_dir

# Perform backup
for directory in $backup_me
do
        archive_name=`echo ${directory} | sed s/^\\\/// | sed s/\\\//_/g`
        tar pcfzP ${backup_dir}/${archive_name}.tgz ${directory} 2>&1 | tee > ${backup_dir}/${archive_name}.log
done

echo "Installing Dotfiles from Cloned Repository"
cp .config $HOME/.config
cp .zshrc $HOME/.zshrc
cp .skhdrc $HOME/.skhdrc
cp .yabairc $HOME/.yabairc
cp .gitconfig $HOME/.gitconfig
cp .doom.d $HOME/.doom.d

echo "Done!"

echo "                Further User Setup                   "
echo "-----------------------------------------------------"
echo "Open vim and use :PackerSync to install plugins     "
echo "       Thats it, thanks for downloading, enjoy :)    "
