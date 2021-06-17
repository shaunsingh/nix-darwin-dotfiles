# Dotfiles

(slightly outdated image)
<img width="1440" alt="Screen Shot 2021-06-10 at 9 39 53 PM" src="https://user-images.githubusercontent.com/71196912/121620539-84cdef00-ca38-11eb-9219-d2bb15cfcedc.png">
<img width="1440" alt="Screen Shot 2021-06-10 at 9 44 20 PM" src="https://user-images.githubusercontent.com/71196912/121620543-84cdef00-ca38-11eb-9684-bbfd390aa4de.png">


Configurations for the following are included

1. fish/starship/alacritty/zsh
2. yabai/skhd
3. neovim/vim/neovide
4. emacs
5. ranger
6. IdeaVim (intellij)

## Installation

**Warning:** If you want to give these dotfiles a try, you should first fork this repository, review the code, and remove things you don’t want or need. Don’t blindly use my settings unless you know what that entails.

This setup was created first and foremost for macOS. Although most dotfiles should transfer well between macOS and Linux, some might not.

**NOTE** There is now an install file in the repo (`install.sh`), just download the .sh file and run it. If you have a previous version of the dotfiles, You can simply delete the directories and re-install using the script 

The Install script will do the following: 

1. Install Homebrew. 
2. Tap and Install fonts, apps, formula, and cleanup brew. 
3. Build and Install Neovim Nightly 
4. Build and Install Emacs native-comp (28)
5. Install and Setup fish shell
6. Install Vimari (vim emulation for safari)
7. Install Doom Emacs and the required LaTeX packages
8. Clone dotfiles 
9. Copy dotfiles to their recommend locations
10. Grab wallpapers and move them to the home folder 

## Prerequistes

On macOS, all the prequesites should be automatically installed. On linux, make sure you have a package manager, as well as git installed.

If you run into issues with xcode, install the Xcode CLT for macOS (this is needed for Git and our Homebrew Installation)

```zsh
sudo xcode-select --install
```

### Install Homebrew formulae

The install script will automatically install all the apps and dependencies. If you have slower internet or an older computer, it may take a while as some dependencies need to build from source. If you're on linux, or don't want to use homebrew, then you can install the following manually

```
python@3.9 (or latest)
luajit (I use the --HEAD version)
neovim (5.0/nightly required)
ripgrep
starship
skhd + yabai (or a window manager of your choice)
emacs-plus (or an emacs distrubtion of your choice)
ranger
alacritty
fish

MacOS Specific: 
ubersicht (toolbar)
```

Note: Brew bundle will install the native-compilation version of emacs-plus, but it may be buggy on certain machines and configurations. You can install emacs-mac if you want emacs27 instead.

For linux users, either install `emacs` (emacs 27) or `emacs-pgtk-native-comp` (emacs 28) via your package manager of choice.

## Additional configuration

For most dotfiles you can just drop and replace the existing ones. However a few may require extra steps

### Emacs

If you want to upgrade or sync emacs, you can run 

```zsh
~/.emacs.d/bin/doom sync -u
```

### Neovim

Upon starting Neovim, packer should automatically install. You will need to install and sync all plugins. You can do this by running the following.

`:PackerSync`

The plugins will install. After restarting neovim, nvim-treesitter should install and configure parsers. Afterwards. run `:checkhealth` to check for possible issues.

If you want to take advantage of the LSP, you can install language servers using the following command:

`:LspInstall (language)` e.g. `:LspInstall java` to install the java LSP (jdtls)

I also recommend installing [Neovide](https://github.com/Kethku/neovide) or [goneovim](https://github.com/akiyosi/goneovim) if you prefer a gui experience. A goneovim config is included in the dotfiles

## Feedback

Suggestions/improvements
[welcome](https://github.com/shaunsingh/vimrc-dotfiles/issues)!
