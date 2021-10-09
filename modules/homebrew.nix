{ pkgs, ... }: {
  system.activationScripts.postUserActivation.text = ''
    # Install homebrew if it isn't there 
    if [[ ! -d "/opt/homebrew/bin" ]]; then
      /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    fi
  '';
  homebrew = {
    brewPrefix = "/opt/homebrew/bin";
    enable = true;
    autoUpdate = true;
    cleanup = "zap";
    global = {
      brewfile = true;
      noLock = true;
    };
    taps = [
      "homebrew/core"
      "homebrew/cask"
      "homebrew/cask-fonts"
      "d12frosted/emacs-plus"
      "cmacrae/formulae"
    ];
    brews = [
      "cmacrae/formulae/spacebar"
    ];
    casks = [
      "intellij-idea"
      "zoom"
      # Note: Still emulated via rosetta
      "nvidia-geforce-now"
      "discord"
      # Font support is better with homebrew
      "font-alegreya"
      "font-overpass"
    ];
    extraConfig = ''
      brew "d12frosted/emacs-plus/emacs-plus@28", args: ["with-elrumo2-icon", "with-native-comp", "with-xwidgets", "without-imagemagick"]
    '';
  };
}
