# Nix-Darwin-Dotfiles
Dotfiles managed via [https://github.com/LnL7/nix-darwin](Nix-Darwin) and [https://github.com/nix-community/home-manager](Home-Manager)

## Showcase
[[./extra/assets/out.png]]

**Warning:** Nix on darwin, especially on arm, still has a few bugs here and there. While its mostly settled down theres still quite a few packages that refuse to build. Feel free to open an issue if you run into problem

## Features 
- Configuration of macOS & packages through nix-darwin and home-manager
- Based on Nix & Flakes for reproducable builds and easy deployment
- Built against clang v15 with the mold linker, no gnu needed :)
- Nightly (git, from source) builds of yabai, sketchybar, emacs, nyxt, wezterm, and neovim
- Systemwide base16 themeing


## Installing and notes
### MacOS 
*NOTE: These are available as an executable script [[./extra/install.sh]]*
Install Nix. I have it setup for multi-user, but you can remove the =--daemon= if you want a single user install
```sh
sh <(curl -L https://nixos.org/nix/install) --daemon
```
Build, and switch to the dotfiles
```sh
nix build ~/nix-darwin-dotfiles\#darwinConfigurations.shaunsingh-laptop.system --extra-experimental-features nix-command --extra-experimental-features flakes
./result/sw/bin/darwin-rebuild switch --flake .#shaunsingh-laptop
```
(note, =--extra-experimental-features= is only needed the first time around. After that the configuration will edit =/etc/nix/nix.conf= to enable flakes and nix-command by default)

### Linux 
todo

## Feedback
Suggestions/improvements [https://github.com/shaunsingh/vimrc-dotfiles/issues](welcome)
