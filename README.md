# Dotfiles

(slightly outdated image)
<img width="1440" alt="Screen Shot 2021-06-10 at 9 39 53 PM" src="https://user-images.githubusercontent.com/71196912/121620539-84cdef00-ca38-11eb-9219-d2bb15cfcedc.png">
<img width="1440" alt="Screen Shot 2021-06-10 at 9 44 20 PM" src="https://user-images.githubusercontent.com/71196912/121620543-84cdef00-ca38-11eb-9684-bbfd390aa4de.png">


Configurations for the following are included

1. fish/starship/alacritty
2. yabai/skhd
3. neovim/vim/neovide/goneovim
4. emacs
5. IdeaVim (intellij)
6. VsCode

## Installation 

**Warning:** If you want to give these dotfiles a try, you should first fork this repository, review the code, and remove things you don’t want or need. Don’t blindly use my settings unless you know what that entails.

This setup was created first and foremost for macOS. Although most dotfiles should transfer well between macOS and Linux, some might not. Nix should allow it to work well on linux, but some packages may not translate as well (e.g. emacsMacport and anything installed with Homebrew)

# Installing Nix

1. Run `preinstall.sh` to install and move the required dotfiles
2. Install nix by running the following:
macOS
```
sh <(curl -L https://nixos.org/nix/install) --darwin-use-unencrypted-nix-store-volume
```
4. Run `postinstall.sh` to finalize setup

## Notes and additional configuration

On macOS, all the prequesites should be automatically installed. On linux, make sure you have a package manager, as well as git installed.

If you run into issues with xcode, install the Xcode CLT for macOS (this is needed for Git and our Homebrew Installation)

```zsh
sudo xcode-select --install
```

Note: Brew will install the native-compilation version of emacs-plus, but it may be buggy on certain machines and configurations. You can install emacs-mac if you want emacs27 instead.

For linux users, either install `emacs` (emacs 27) or [recommended] `emacs-pgtk-native-comp` (emacs 28) via your package manager of choice.

### Emacs

My emacs configuration is designed for org-mode editing, as well as moderate programming use.

If you want to upgrade or sync emacs, you can run

```zsh
doom sync -u
```

If you modify your shell configuration, run `doom env` to regenerate env vars

### Neovim

My neovim configuration is designed for programming and quick text editing. As such, it opens in under 40ms.

Upon starting Neovim, packer should automatically install. You will need to install and sync all plugins. You can do this by running the following.

`:PackerSync`

The plugins will install. After restarting neovim, nvim-treesitter should install and configure parsers. Afterwards. run `:checkhealth` to check for possible issues.

If you modify the configuration files for certain plugins, you may have to run `:PackerSync` to apply changes

If you want to take advantage of the LSP, you can install language servers using the following command:

`:LspInstall (language)` e.g. `:LspInstall java` to install the java LSP (jdtls)

I also recommend installing [Neovide](https://github.com/Kethku/neovide) or [goneovim](https://github.com/akiyosi/goneovim) if you prefer a gui experience. A goneovim config is included in the dotfiles

### VsCode

My VsCode configuration is for those moments when I have to use vscode (pair programming) or for filetypes that work much better in vscode (python notebooks)

You may have to install both the Nord VsCode theme, as well as the Vim emulation extension.

### Intelij

I use intellij for java development, or anything where I need an IDE. You should only need IdeaVim for the configuration to function, but I recommend getting the material theme and nord theme as well.

### Fish

Fish doesn't detect homebrew by default. You can enable homebrew under fish by running either

`set -U fish_user_paths /usr/local/bin $fish_user_paths`

or for Apple Silicon macs

`set -U fish_user_paths /opt/homebrew/bin $fish_user_paths`

## Feedback

Suggestions/improvements
[welcome](https://github.com/shaunsingh/vimrc-dotfiles/issues)!
