{ pkgs, spacebar, ... }: {
  home-manager.users.shauryasingh.home.packages = with pkgs; [
    luarocks
  ];
  system.activationScripts.postUserActivation.text = ''
    # Install homebrew if it isn't there 
    if [[ ! -d "/opt/homebrew/bin" ]]; then
      /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    fi
    # Install spacehammer if it isn't there
    if [ ! -d "~/.hammerspoon" ] ; then
      git clone --depth 1 https://github.com/agzam/spacehammer ~/.hammerspoon
    fi
    if ! command -v fennel &> /dev/null
    then
        ~/.nix-profile/bin/luarocks install fennel --local
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
      # "xorpse/formulae"
      "cmacrae/formulae"
    ];
    casks = [
      "intellij-idea"
      "zoom"
      "hammerspoon"
      ## "kitty"
      # Note: Still emulated via rosetta
      "nvidia-geforce-now"
      "discord"
      # Font support is better with homebrew
      "font-alegreya"
      "font-overpass"
    ];
    # REMOVED: brew "xorpse/formulae/yabai", args: ["HEAD"]
    extraConfig = ''
      brew "d12frosted/emacs-plus/emacs-plus@28", args: ["with-elrumo2-icon", "with-native-comp", "with-xwidgets", "without-imagemagick"]
      brew "cmacrae/formulae/spacebar"
    '';
  };
  # services.yabai = {
  #   enable = true;
  #   package = builtins.path {
  #     path = /opt/homebrew;
  #     filter = (path: type: type == "directory" || builtins.baseNameOf path == "yabai");
  #   };
  #   config = {
  #     auto_balance = "on";
  #     layout = "bsp";
  #     bottom_padding = 48;
  #     left_padding = 18;
  #     right_padding = 18;
  #     top_padding = 18;
  #     window_gap = 18;
  #     mouse_follows_focus = "on";
  #     mouse_modifier = "fn";
  #     split_ratio = "0.50";
  #     window_border = "off";
  #     window_placement = "second_child";
  #     window_topmost = "on";
  #   };
  #   extraConfig = ''
  #       # rules
  #       yabai -m rule --add app='System Preferences' manage=off
  #   '';
  # };
  # services.spacebar = {
  #   enable = true;
  #   package = builtins.path {
  #     path = /opt/homebrew;
  #     filter = (path: type: type == "directory" || builtins.baseNameOf path == "spacebar");
  #   };
  #   services.spacebar.config = {
  #     position                   = "bottom";
  #     display                    = "main";
  #     height                     = 26;
  #     title                      = "on";
  #     spaces                     = "on";
  #     clock                      = "on";
  #     power                      = "on";
  #     padding_left               = 20;
  #     padding_right              = 20;
  #     spacing_left               = 25;
  #     spacing_right              = 25;
  #     text_font                  = ''"Liga SFMono Nerd Font:Medium:13.0"'';
  #     icon_font                  = ''"Liga SFMono Nerd Font:Light:13.0"'';
  #     background_color           = "0xff2E3440";
  #     foreground_color           = "0xffD8DEE9";
  #     space_icon_color           = "0xff8fBcBB";
  #     power_icon_color           = "0xffEBCBDB";
  #     battery_icon_color         = "0xffA3BE8C";
  #     dnd_icon_color             = "0xffA3Be8C";
  #     clock_icon_color           = "0xff81a1C1";
  #     power_icon_strip           = " ";
  #     space_icon                 = "•";
  #     space_icon_strip           = "I II III IV V VI VII VIII IX X";
  #     spaces_for_all_displays    = "on";
  #     display_separator          = "on";
  #     display_separator_icon     = "";
  #     clock_icon                 = "";
  #     dnd_icon                   = "";
  #     clock_format               = ''"%d/%m/%y %R"'';
  #     right_shell                = "on";
  #     right_shell_icon           = "";
  #     right_shell_command        = "whoami";
  #   };
  # };
  # services.skhd = {
  #   enable = true;
  #   package = pkgs.skhd;
  #   skhdConfig = ''
	# # Focus window
  #   	ctrl + alt - h : yabai -m window --focus west
  #   	ctrl + alt - j : yabai -m window --focus south
  #   	ctrl + alt - k : yabai -m window --focus north
  #   	ctrl + alt - l : yabai -m window --focus east

  #   	# Fill space with window
  #   	ctrl + alt - 0 : yabai -m window --grid 1:1:0:0:1:1

  #   	# Move window
  #   	ctrl + alt - e : yabai -m window --display 1; yabai -m display --focus 1
  #   	ctrl + alt - d : yabai -m window --display 2; yabai -m display --focus 2
  #   	ctrl + alt - f : yabai -m window --space next; yabai -m space --focus next
  #   	ctrl + alt - s : yabai -m window --space prev; yabai -m space --focus prev

  #   	# Close current window
  #   	ctrl + alt - w : $(yabai -m window $(yabai -m query --windows --window | jq -re ".id") --close)

  #   	# Rotate tree
  #   	ctrl + alt - r : yabai -m space --rotate 90

  #   	# Open application
  #   	ctrl + alt - enter : alacritty
  #   	ctrl + alt - e : emacs
  #   	ctrl + alt - b : open -a Safari

  #       ctrl + alt - t : yabai -m window --toggle float;\
  #         yabai -m window --grid 4:4:1:1:2:2

  #       ctrl + alt - p : yabai -m window --toggle sticky;\
  #         yabai -m window --toggle topmost;\
  #         yabai -m window --toggle pip
  #   '';
  # };
}
