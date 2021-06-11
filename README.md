# Dotfiles

<img width="1440" alt="Screen Shot 2021-06-10 at 9 39 53 PM" src="https://user-images.githubusercontent.com/71196912/121620539-84cdef00-ca38-11eb-9219-d2bb15cfcedc.png">
<img width="1440" alt="Screen Shot 2021-06-10 at 9 44 20 PM" src="https://user-images.githubusercontent.com/71196912/121620543-84cdef00-ca38-11eb-9684-bbfd390aa4de.png">


Configurations for the following are included

1. zsh
2. yabai
3. skhd
4. ranger

as well as 

~/.config
1. neovim
2. neovide (gui)
3. goneovim (gui)
4. starship
5. alacritty

~/.doom.d
1. emacs 
   
~/extra
1. homebrew brewfile
2. wallpapers

<img width="1440" alt="Screen Shot 2021-06-10 at 9 39 53 PM" src="https://user-images.githubusercontent.com/71196912/121620528-81d2fe80-ca38-11eb-9ec6-d2fa356bdbd3.png">
<img width="1440" alt="Screen Shot 2021-06-10 at 9 44 20 PM" src="https://user-images.githubusercontent.com/71196912/121620529-826b9500-ca38-11eb-8e8b-f9b9c8eeee15.png">

## Installation

**Warning:** If you want to give these dotfiles a try, you should first fork this repository, review the code, and remove things you don’t want or need. Don’t blindly use my settings unless you know what that entails.

This setup was created first and foremost for macOS. Although most dotfiles should transfer well between macOS and Linux, some might not. 

## Prerequistes 

If you haven't already, install the Xcode CLT for macOS (this is needed for Git and our Homebrew Installation)

```zsh
sudo xcode-select --install
```

Before you can install anything, you must install homebrew, a packagemanager for macOS. 

```zsh
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```

### Install Homebrew formulae

If you have a mac, you can simply `cd` to the vimrc-dotfiles/extra/brew/brewfile, and run 

```zsh
brew bundle
```

to automatically install all the apps and dependencies. If you have slower internet or an older computer, it may take a while as some dependencies need to build from source. If you're on linux, or don't want to use homebrew, then you can install the following manually 

```
python@3.9 (or latest)
luajit (I use the --HEAD version)
neovim (5.0/nightly required)
ripgrep
starship
zsh-syntax-highlighting
zsh-autosuggestions
skhd + yabai (or a window manager of your choice)
emacs-plus (or an emacs distrubtion of your choice)
ranger

MacOS Specific: 
ubersicht (toolbar)
```

Note: Brew bundle will install the native-compilation version of emacs-plus, but it may be buggy on certain machines and configurations. You can install emacs-mac if you want emacs27 instead. 

For linux users, either install `emacs` (emacs 27) or `emacs-pgtk-native-comp` (emacs 28) via your package manager of choice. 

## Additional configuration 

For most dotfiles you can just drop and replace the existing ones. However a few may require extra steps 

### Emacs 

As my config is built on doom emacs, you must install it first 

```zsh
git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d
~/.emacs.d/bin/doom install
```

Drop my dotfiles under /.doom.d and run 

```zsh
~/.emacs.d/bin/doom sync
```

#### latex

If you would like to take advantage of some of the org-mode latex features, and CalcTex, you need to install several packages 

#### Automatic install

Brew bundle will automatically install basictex for you. From there you can run the following command in the terminal to install all required dependencies: 

```zsh
sudo tlmgr install dvipng dvisvgm l3packages xcolor soul adjustbox collectbox amsmath siunitx cancel mathalpha
```

#### Manual Install

If you don't want to utilize brew cask, or if you're usnig linux, you can use the following:

##### Macos

Option 1: Install MacTex (large download, includes everything)
Option 2 (recommended): Install BasicTex run the following command (as sudo)

```zsh
sudo tlmgr install dvipng dvisvgm l3packages xcolor soul adjustbox collectbox amsmath siunitx cancel mathalpha
```

For your convenience, you can also add the bin directory to your path 

##### Linux

On linux you can install either full LaTeX (similar to MacTex) or you can install `TexLive-base` and install packages manually. 

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
