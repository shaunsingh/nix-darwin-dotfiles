# Dotfiles

Configurations for the following are included

1. zsh
2. yabai
3. skhd

as well as 

~/.config
1. neovim
2. goneovim (gui)
3. starship
~/.doom.d
   doom emacs 
~/extra
   homebrew brewfile


## Installation

**Warning:** If you want to give these dotfiles a try, you should first fork this repository, review the code, and remove things you don’t want or need. Don’t blindly use my settings unless you know what that entails. Use at your own risk! 

This setup was created first and foremost for macOS. Although most dotfiles should transfer well between macOS and Linux, some might not

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
bat
python@3.9 (or latest)
luajit (I use the --HEAD version)
neovim (5.0/nightly required)
ripgrep
starship
zsh-syntax-highlighting
skhd + yabai (or a window manager of your choice)
emacs-mac (or an emacs distrubtion of your choice)
ranger
```


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

For your convenience, you can also add the bin directory to your path 

### Neovim 

Upon starting Neovim you will be greated with a plethora of errors. You can solve this by installing the plugins. Open up neovim and run the following 

`:PackerSync`

The plugins will install. After this you can restart neovim and it will install all the required treesitter parsers automatically (this is for better syntax highlighting and completion)

If you want to take advantage of the LSP, you can install language servers using the following command: 

`:LspInstall (language)` e.g. `:LspInstall java` to install the java LSP (jdtls)

I also recommend installing [Neovide](https://github.com/Kethku/neovide) if you prefer a gui experience.

## Yabai 

Although it isn't required, you can install a toolbar such as [Pecan](https://github.com/zzzeyez/pecan) If you don't like the look of the default one.

## Feedback

Suggestions/improvements
[welcome](https://github.com/mathiasbynens/dotfiles/issues)!