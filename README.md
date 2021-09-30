# Dotfiles

Managed via [YADM](https://github.com/TheLocehiliosan/yadm) and nix

## Installation

**Warning:** If you want to give these dotfiles a try, you should first fork this repository, review the code, and remove things you don’t want or need. Don’t blindly use my settings unless you know what that entails.

## Install

### Emacs

After you get everything up and running, be aware you need to tangle the file before installing doom emacs: 
```sh
emacs --batch --eval "(progn (require 'org) (setq org-confirm-babel-evaluate nil) (org-babel-tangle-file \"~/.config/doom/config.org\"))"
git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d
~/.emacs.d/bin/doom install
```

**Personal Note:** this is my configuration for emacs-ng
```     
./configure CFLAGS="-Wl,-rpath,shared,--disable-new-dtags -g -O3 -mtune=native -march=native -fomit-frame-pointer" \
            --prefix=/usr/local/ \
            --with-json --with-modules --with-harfbuzz --with-compress-install \
            --with-threads --with-included-regex --with-zlib --with-cairo --with-libsystemd \
            --with-rsvg --with-native-compilation ${@:3} --with-webrender --without-javascript \
            --without-sound --without-imagemagick --without-makeinfo --without-gpm --without-dbus \
            --without-pop --without-toolkit-scroll-bars --without-mailutils --without-gsettings \
            --with-all   
```

### MacOS

Install homebrew
```sh
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```
Install Xcode CLI
```sh
xcode-select --install
```
Install YADM
```sh
brew install yadm
```
Clone the repo and install it
```sh
yadm clone https://github.com/shaunsingh/vimrc-dotfiles.git --no-bootstrap
yadm bootstrap
yadm remote add origin https://github.com/shaunsingh/vimrc-dotfiles.git
```

**Note:** You may have to chmod the files
```sh
cd ~/.config/yadm
chmod +x *.sh
chmod +x bootstrap.d/*.sh
```

### nixOS

**Note** This is more of a note for myself for when I install on linux machines. I am quite new to nixos, and I recommend you check over my config before installing it. The config also uses wayland+sway, so it may be incompatible with certain gpus

Install via flakes
```zsh
nix-shell -p nixUnstable git
nixos-install --flake https://github.com/shaunsingh/vimrc-dotfiles#shaunsingh-laptop
```

Boot, clone the dotiles
```zsh
yadm clone https://github.com/shaunsingh/vimrc-dotfiles.git --no-bootstrap
yadm remote add origin https://github.com/shaunsingh/vimrc-dotfiles.git
```

## Notes and additional configuration

On macOS, all the prerequisites should be automatically installed. On Linux, make sure you have a package manager, as well as git installed.

Note: Brew will install the native-compilation version of emacs-plus, but it may be buggy on certain machines and configurations. You can install emacs-mac if you want emacs27 instead. Brew will also install the developer branch of neovim (`nvim 0.6`), you can use neovim 0.5 (stable) but I can't garuntee if it works.

For linux users, either install `emacs` (emacs 27) or [recommended] `emacs-pgtk-native-comp` (emacs 28) via your package manager of choice.

### Fonts
My configuruation uses 4 fonts not installed by default. My nix config should
handle installing the fonts, homebrew should as well. If you need to, install
the following fonts manually:

Required:
1. Fira SFMono Nerd Font (https://github.com/shaunsingh/SFMono-Nerd-Font-Ligaturized)

Required for emacs
1. Overpass
2. IBM Plex Sans
3. Alegreya

### Fish

Fish doesn't detect homebrew by default. You can enable homebrew under fish by running either

`set -U fish_user_paths /usr/local/bin $fish_user_paths`

or for Apple Silicon macs

`set -U fish_user_paths /opt/homebrew/bin $fish_user_paths`


### Emacs

If you want to recompile the literate configuration, you can run

```zsh
doom sync -u
```

If you want to update the doom configuration, you can run

```zsh
doom upgrade
```

If you modify your shell configuration, run `doom env` to regenerate env vars

#### Mu4e and Gmail
Email will have a few issues, since its hardcoded to my account and my machines path. Do the following steps to get email up and running for you

1. modify `~/.mbsyncrc` to include your email and password
2. replace instances of my name and email in `~/.doom.d/config.org`
3. replace the path to `msmtp` in `~/.doom.d/config.org` if you have an intel mac
4. Rerun the following commands, replace the example with your email:

```zsh
mu init --maildir=~/.mbsync --my-address=email@example.org
mu index
mbsync --all
```

Indexed mail will go under `~/.mbsync/`, you can either manually mbsync or use emacs to update mail

### Org Mode

My org mode config includes two additional plugins, org-agenda and org-roam.
Both these plugins need a set directory. All org files can go under the created `~/org` dir. Roam files go under `~/org/roam`

### Neovim

My neovim configuration is designed for programming and quick text editing. As such, it opens in under 10ms.

Upon starting Neovim, packer should automatically install and sync. In case this step fails, or you want to update plugins, you can run `:PackerSync`

The plugins will install. After restarting neovim, nvim-treesitter should install and configure parsers. Afterwards. run `:checkhealth` to check for possible issues.

If you modify the configuration files for certain plugins, you may have to run `:PackerSync` to apply changes

If you want to take advantage of the LSP, you can install language servers using the following command:

`:LspInstall (language)` e.g. `:LspInstall java` to install the java LSP (jdtls)

I also recommend installing [Neovide](https://github.com/Kethku/neovide) or [goneovim](https://github.com/akiyosi/goneovim) if you prefer a gui experience. A goneovim config is included in the dotfiles

## Feedback

Suggestions/improvements
[welcome](https://github.com/shaunsingh/vimrc-dotfiles/issues)!
