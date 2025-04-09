# Nix-Darwin-Dotfiles

NixOS config to run the [NYXT](https://nyxt.atlas.engineer/) browser standalone under either the kiosk compositor [cage](https://github.com/cage-kiosk/cage) or Valve's gaming-oriented microcompositor [gamescope](https://github.com/ValveSoftware/gamescope)

For old config, see the [old-nix-darwin](https://github.com/shaunsingh/nix-darwin-dotfiles/tree/old-nix-darwin) branch

<img src="assets/showcase.png" width="690">

## Notes

The default installation is for an apple-silicon machine using the asahi kernel. If you are using an apple-silicon machine, then replace `apple-silicon-support/hardware-configuration.nix` with your own copy from the output of `nixos-generate-config`.

If you are not using an apple-silicon machine, comment out `./apple-silicon-support` from the imports in `configuration.nix` and import your own copy of `hardware-configuration.nix`. Add/remove/adjust options as needed. By default the hostname is `nyxtkiosk`. Change it as needed

By default, `nyxt4-wrapped/default.nix` builds a pre-release of NYXT 4.0 with WebkitGTK built with support for DRM. As a result, the build may take a while and your config may be unsupported. Adjust as needed

## Installation

This guide assumes you either already have a full installation of NixOS or are in a live CD. If you have trouble installing NixOS, use the official [manual](https://nixos.org/manual/nixos/unstable/)

First clone and enter the repo (`git clone --depth 1 https://github.com/shaunsingh/nyxt-nix-kiosk.git && cd nyxt-nix-kiosk`). Adjust imports in `configuration.nix` and add your `hardware-configuration.nix` as needed

If you are currently in a live CD, run `nixos-install --flake .#nyxtkiosk`

If you are currently in a full installation, run `sudo nixos-rebuild switch --flake .#nyxtkiosk`

Finally, use `passwd` to set a password for the `nyxtkiosk` user, reboot if needed, and enjoy!

## Configuration

By default, my personal config (found under `nyxt4-unwrapped/config/nyxt` and provided with no warranty) is loaded using home-manager, disable this in `nyxt4-unwrapped/default.nix` and supply your own config if you'd like

By default, a copy of my `sway` configuration is imported, as NYXT does not completely fulfill my needs quite yet. It also ships with `greetd` as the standard display maanger, which will automatically log-in and launch NYXT under cage on first boot, then when exit will launch sway. Uncomment `./sway` from the imports in `configuration.nix` and remove it from the `greetd` configuration if you'd like to use just the kiosk mode.

You may also adjust the configuration to do one of the following:

- (recommended) run `nyxt-cage` to launch NYXT under the cage compositor
- Run `nyxt-gamescope` to launch NYXT under Valve's gamescope microcompositor
- Use the TTY as you wish

Adjust the options for `nyxt4-wrapped` in `configuration.nix` to suit your needs. The defaults are as follows. 

```
# configuration for nyxt kiosk
nyxt4-wrapped = {
  display = "eDP-1";
  resolution = "2560x1600";
  scale = 2;
};
```

You may check the former two for your own machine by running `wlr-randr` under `sway` or any wayland compositor. Scale is subjective, adjust as you see fit, generally for a hidpi machine you want 2, stick with 1 for any lower resolution displays.

---

This file was written with `ace-mode` in NYXT
