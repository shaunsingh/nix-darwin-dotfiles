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
      brew "xorpse/formulae/yabai", args: ["HEAD"]
    '';
  };
  services.yabai = {
    enable = true;
    package = builtins.path {
      path = /opt/homebrew;
      filter = (path: type: type == "directory" || builtins.baseNameOf path == "yabai");
    };
    enableScriptingAddition = true;
    config = {
      auto_balance = "on";
      layout = "bsp";
      bottom_padding = 48;
      left_padding = 18;
      right_padding = 18;
      top_padding = 18;
      window_gap = 18;
      mouse_follows_focus = "on";
      mouse_modifier = "fn";
      split_ratio = "0.50";
      window_border = "off";
      window_placement = "second_child";
      window_topmost = "on";
    };

    extraConfig = ''
        # rules
        yabai -m rule --add app='System Preferences' manage=off
    '';
  };

  services.skhd = {
    enable = true;
    package = pkgs.skhd;
    skhdConfig = ''
	# Focus window
	hyper - h : yabai -m window --focus west
	hyper - j : yabai -m window --focus south
	hyper - k : yabai -m window --focus north
	hyper - l : yabai -m window --focus east
	
	# Fill space with window
	hyper - 0 : yabai -m window --grid 1:1:0:0:1:1
	
	# Move window
	hyper - e : yabai -m window --display 1; yabai -m display --focus 1
	hyper - d : yabai -m window --display 2; yabai -m display --focus 2
	hyper - f : yabai -m window --space next; yabai -m space --focus next
	hyper - s : yabai -m window --space prev; yabai -m space --focus prev
	
	# Close current window
	hyper - w : $(yabai -m window $(yabai -m query --windows --window | jq -re ".id") --close)
	
	# Rotate tree
	hyper - r : yabai -m space --rotate 90
	
	# Open application
	hyper - enter : alacritty
	hyper - e : emacs 
	hyper - b : open -a Safari
    '';
  };
}
