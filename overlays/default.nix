_:

{
  flake.overlays.default = final: prev: {
    ### -- derivations 
    otf-apple = prev.callPackage ./derivations/otf-apple.nix { };
    phocus-oxocarbon = prev.callPackage ./derivations/phocus-oxocarbon.nix { };
    sf-mono-liga-bin = prev.callPackage ./derivations/sf-mono-liga-bin.nix { };

    ### -- overlays 
    # firefox-unwrapped = prev.firefox-unwrapped.overrideAttrs (old: {
    #   patches = (old.patches or [ ]) ++ [ ./patches/D164578.diff ];
    # });
    wlroots = prev.wlroots.overrideAttrs (old: {
      patches = (old.patches or [ ]) ++ [ ./patches/wlroots-nvidia.diff ];
    });
  };
}
