# Nix-Darwin-Dotfiles

Dotfiles for M1/M2 devices for Nix-Darwin or Asahi Linux on NixOS. Managed via [https://github.com/LnL7/nix-darwin](Nix-Darwin) and [https://github.com/nix-community/home-manager](Home-Manager)

## Showcase

[[./extra/assets/out.png]]

## Installing and notes

### MacOS 

Install Nix. 

```sh
sh <(curl -L https://nixos.org/nix/install) --daemon
```

Build, and switch to the dotfiles

```sh
nix build ~/nix-darwin-dotfiles\#darwinConfigurations.shaunsingh-laptop.system --extra-experimental-features nix-command --extra-experimental-features flakes
./result/sw/bin/darwin-rebuild switch --flake .#shaunsingh-laptop
```

(note, `--extra-experimental-features` is only needed the first time around. After that the configuration will edit `/etc/nix/nix.conf` to enable flakes and nix-command by default)

### Linux 
Follow the [nixos-apple-silicon](https://github.com/tpwrules/nixos-apple-silicon/blob/main/docs/uefi-standalone.md) guide to install asahi, uboot, and m1n1 to your machine. Reboot your machine into NixOS. Make sure to set a password for your user. Clone this repository and `cd` into it. Run the following commands. Don't worry if it takes a while, as it needs to build the kernel and certain features from source.

```
sudo nixos-rebuild boot --flake .#shaunsingh-laptop --extra-experimental-features nix-command --extra-experimental-features flakes
reboot
```

The system should reboot into nixOS and ask for your username and password, then autoboot into sway. You may optionally run the following command to free up space.

```
sudo nix-collect-garbage -d
```

## Feedback
Suggestions/improvements [https://github.com/shaunsingh/nix-darwin-dotfiles/issues](welcome)
