{
  pkgs,
  lib,
  inputs,
  config,
  ...
}: {
  programs.fish.enable = true;
  system.activationScripts.postActivation.text = ''
    # Set the default shell as fish for the user. MacOS doesn't do this like nixOS does
    sudo chsh -s ${lib.getBin pkgs.fish}/bin/fish shauryasingh
  '';
}
