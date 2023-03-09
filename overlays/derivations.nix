final: prev: rec {
  kickoff = prev.callPackage ../derivations/kickoff.nix {};
  kickoff-dot-desktop = prev.callPackage ../derivations/kickoff-dot-desktop.nix {};
  otf-apple = prev.callPackage ../derivations/otf-apple.nix {};
  phocus-oxocarbon = prev.callPackage ../derivations/phocus-oxocarbon.nix {};
  sf-mono-liga-bin = prev.callPackage ../derivations/sf-mono-liga-bin.nix {};
  xremap = prev.callPackage ../derivations/xremap.nix {};
  rip = prev.callPackage ../derivations/rip.nix {};
  asahi-battery-threshold = prev.callPackage ../derivations/asahi-battery-threshold.nix {};
  webkitgtk =
    if prev.stdenv.isDarwin
    then
      (prev.callPackage ../derivations/webkitgtk.nix {
        harfbuzz = prev.harfbuzzFull;
        inherit (prev.gst_all_1) gst-plugins-base gst-plugins-bad;
        inherit (prev.darwin) apple_sdk;
      })
    else prev.webkitgtk;
  pure-protobuf = prev.callPackage ../derivations/pure-protobuf.nix {
    buildPythonPackage = prev.python310Packages.buildPythonPackage;
  };
  pytest-harvest = prev.callPackage ../derivations/pytest-harvest.nix {};
  pytest-steps = prev.callPackage ../derivations/pytest-steps.nix { inherit pytest-harvest; };
  komikku = prev.callPackage ../derivations/komikku.nix { inherit pure-protobuf pytest-steps; };
}
