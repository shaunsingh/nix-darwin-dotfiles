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
      "nvidia-geforce-now"
      "intellij-idea"
      "zoom"
      "discord"
      "alacritty"
      "kitty"
    ];
    # masApps = {
    #  "Vimari" = 148093394;
    #  "Adguard For Safari" = 144014725;
    # };
  };
}
