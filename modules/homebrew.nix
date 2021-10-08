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
    ];
    casks = [
      "intellij-idea"
      "zoom"
      "kitty"
      # Note: Still emulated via rosetta
      "nvidia-geforce-now"
      "alacritty"
      "discord"
    ];
  };
}
