# Dotfiles

Configurations for the following are included

1. fish/starship/alacritty/neofetch/tmux
2. yabai/skhd
3. neovim/vim/neovide/goneovim
4. emacs
5. IdeaVim (Intellij)

## Installation 

**Warning:** If you want to give these dotfiles a try, you should first fork this repository, review the code, and remove things you don’t want or need. Don’t blindly use my settings unless you know what that entails.

This setup was created first and foremost for macOS. Although most dotfiles should transfer well between macOS and Linux, some might not. The install script will not function, as it relies on homebrew.

## Install

1. Run `preinstall.sh` to move the required dotfiles
3. Run `postinstall.sh` to install dependencies finalize setup

## Notes and additional configuration

On macOS, all the prerequisites should be automatically installed. On Linux, make sure you have a package manager, as well as git installed.

If you run into issues with xcode, install the Xcode CLT for macOS (this is needed for Git and our Homebrew Installation)

```zsh
sudo xcode-select --install
```

Note: Brew will install the native-compilation version of emacs-plus, but it may be buggy on certain machines and configurations. You can install emacs-mac if you want emacs27 instead. Brew will also install the developer branch of neovim (`nvim 0.6`), you can use neovim 0.5 (stable) but I can't garuntee if it works. 

For linux users, either install `emacs` (emacs 27) or [recommended] `emacs-pgtk-native-comp` (emacs 28) via your package manager of choice. 

### Fonts
Emacs uses 2 fonts not installed by default. SF Mono and SF Pro. Although homebrew should handle the installation process, you can reinstall them if nessecary

On linux, you will have to install 4 fonts 
1. SF Mono
2. SF Pro
3. Menlo
4. FiraCode Nerd Font

### Fish

Fish doesn't detect homebrew by default. You can enable homebrew under fish by running either

`set -U fish_user_paths /usr/local/bin $fish_user_paths`

or for Apple Silicon macs

`set -U fish_user_paths /opt/homebrew/bin $fish_user_paths`


### Emacs

My emacs configuration is designed for org-mode editing, as well as moderate programming use.

If you want to recompile the literate configuration, you can run

```zsh
doom sync -u
```

If you want to update the doom configuration, you can run

```zsh
doom upgrade
```

If you modify your shell configuration, run `doom env` to regenerate env vars

You may get errors due to missing fonts on linux. In which case either switch the fonts to what you need, or use DejaVu fonts: 
```lisp
;;fonts
(setq doom-font (font-spec :family "DejaVu Sans Mono" :size 14 :weight 'light)
      doom-big-font (font-spec :family "DejaVu Sans Mono" :size 20 :weight 'light)
      doom-variable-pitch-font (font-spec :family "DejaVu Sans" :size 16 :weight 'Medium)
      doom-unicode-font (font-spec :family "DejaVu Sans Mono":weight 'light)
      doom-serif-font (font-spec :family "DejaVu Sans Mono" :weight 'Regular))
```

(and adjust the serif writeroom font, from SF Pro to DejaVu Sans)

#### Mu4e and Gmail
Email will have a few issues, since its hardcoded to my account and my machines path. Do the following steps to get email up and running for you

1. modify `~/.mbsyncrc` to include your email and password 
2. replace instances of my name and email in `~/.doom.d/config.org`
3. replace the path to `msmtp` in `~/.doom.d/config.org` if you have an intel mac
4. Rerun the following commands, replace the example with your email: 

```zsh
mu init --maildir=~/.mbsync --my-address=email@example.org
mbsync --all
```

Indexed mail will go under `~/.mbsync/`, you can either manually mbsync or use emacs to update mail 

### Org Mode 

**Note:** If you run my install script, it clones my notes repo, you can either delete it, ignore it, or borrow my notes if you would like.

My org mode config includes two additional plugins, org-agenda and org-roam. Both these plugins need a set directory. 

All org files can go under the created `~/org` dir. Roam files go under `~/org/roam`

The install script creates two files for my agenda, `school.org` and `work.org`. You can edit them to your liking

### Neovim

My neovim configuration is designed for programming and quick text editing. As such, it opens in under 10ms.

Upon starting Neovim, packer should automatically install and sync. In case this step fails, or you want to update plugins, you can run `:PackerSync`

The plugins will install. After restarting neovim, nvim-treesitter should install and configure parsers. Afterwards. run `:checkhealth` to check for possible issues.

If you modify the configuration files for certain plugins, you may have to run `:PackerSync` to apply changes

If you want to take advantage of the LSP, you can install language servers using the following command:

`:LspInstall (language)` e.g. `:LspInstall java` to install the java LSP (jdtls)

I also recommend installing [Neovide](https://github.com/Kethku/neovide) or [goneovim](https://github.com/akiyosi/goneovim) if you prefer a gui experience. A goneovim config is included in the dotfiles

### Intelij

Install the following plugins: 
1. `IdeaVim` for vim emulation 
2. `Nord` for the theme
3. `Material UI` optional but highly recommended

## Feedback

Suggestions/improvements
[welcome](https://github.com/shaunsingh/vimrc-dotfiles/issues)!
