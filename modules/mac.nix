{ config, pkgs, lib, ... }:
let scripts = ./sketchybar/scripts;
in {
  nix = {
    package = pkgs.nix;
    extraOptions = ''
      system = aarch64-darwin # M1 gang
      extra-platforms = aarch64-darwin
      experimental-features = nix-command flakes
      build-users-group = nixbld
      sandbox = true
    '';
  };
  environment.systemPackages = with pkgs; [
    discord-canary
    alacritty-ligatures
    wezterm-git
    # nyxt-3
    webkitgtk
    firefox-bin
    # emacs-mac
  ];
  fonts = {
    fontDir.enable = false;
    fonts = with pkgs; [ ibm-plex emacs-all-the-icons-fonts sf-mono-liga-bin ];
  };
  programs.fish.enable = true;
  system.activationScripts.postActivation.text = ''
    # Set the default shell as fish for the user. MacOS doesn't do this like nixOS does
    sudo chsh -s ${lib.getBin pkgs.fish}/bin/fish shauryasingh
  '';

  services.yabai = {
    enable = true;
    enableScriptingAddition = true;
    # package = pkgs.yabai-git;
    package = pkgs.yabai;
    config = {
      # layout
      layout = "bsp";
      auto_balance = "off";
      split_ratio = "0.50";
      window_placement = "second_child";
      # Gaps
      window_gap = 18;
      top_padding = 18;
      bottom_padding = 52;
      left_padding = 18;
      right_padding = 18;
      # shadows and borders
      window_shadow = "float";
      window_border = "on";
      window_border_width = 4;
      active_window_border_color = "0xffffdede";
	  normal_window_border_color = "0xff4c566a";
      # mouse
      mouse_follows_focus = "off";
      focus_follows_mouse = "off";
      mouse_modifier = "cmd";
      mouse_action1 = "move";
      mouse_action2 = "resize";
      mouse_drop_action = "swap";
    };
    extraConfig = ''
      # Unload the macOS WindowManager process
      launchctl unload -F /System/Library/LaunchAgents/com.apple.WindowManager.plist > /dev/null 2>&1 & 
      # bar
      yabai -m signal --add event=window_focused action="sketchybar --trigger window_focus"
      yabai -m signal --add event=display_added action="sleep 1 && ${scripts}/create_spaces.sh"
      yabai -m signal --add event=display_removed action="sleep 1 && ${scripts}/create_spaces.sh"
      yabai -m signal --add event=window_created action="sketchybar --trigger windows_on_spaces"
      yabai -m signal --add event=window_destroyed action="sketchybar --trigger windows_on_spaces" 
      ${scripts}/create_spaces.sh
      # rules
      yabai -m rule --add app="^(LuLu|Vimac|Calculator|Software Update|Dictionary|VLC|System Preferences|zoom.us|Photo Booth|Archive Utility|Python|LibreOffice|App Store|Steam|Alfred|Activity Monitor)$" manage=off
      yabai -m rule --add label="Finder" app="^Finder$" title="(Co(py|nnect)|Move|Info|Pref)" manage=off
      yabai -m rule --add label="Safari" app="^Safari$" title="^(General|(Tab|Password|Website|Extension)s|AutoFill|Se(arch|curity)|Privacy|Advance)$" manage=off
      yabai -m rule --add label="About This Mac" app="System Information" title="About This Mac" manage=off
      yabai -m rule --add label="Select file to save to" app="^Inkscape$" title="Select file to save to" manage=off 
    '';
  };

  services.sketchybar = {
    enable = true;
    # package = pkgs.sketchybar-git;
    package = pkgs.sketchybar;
    config = ''
      #!/bin/bash
      
      # Unload the macOS on screen indicator overlay for volume change
      launchctl unload -F /System/Library/LaunchAgents/com.apple.OSDUIHelper.plist > /dev/null 2>&1 & 

      ############## BAR ##############
      sketchybar --bar height=40 \
                       position=bottom \
                       shadow=on \
                       color=0xff161616 \

      ############## GLOBAL DEFAULTS ##############
      sketchybar --default updates=when_shown \
                           icon.font="Liga SFMono Nerd Font:Bold:15.0" \
                           label.font="Liga SFMono Nerd Font:Regular:15.0" \
                           icon.color=0xffffffff \
                           label.color=0xffffffff \
                           background.color=0xff161616 \
                           background.padding_left=9 \
                           background.padding_right=9 \
                           background.height=40

      ############## ITEMS ############### 
      SPACE_ICONS=("一" "二" "三" "四" "五" "六" "七" "八" "九" "十")
      SPACES=()
      sid=0
      for i in "''${!SPACE_ICONS[@]}"
      do 
        sid=$(($i+1))
        sketchybar --add space space.$sid left \
                   --set space.$sid associated_space=$sid \
                                    icon=''${SPACE_ICONS[i]} \
                                    icon.padding_left=12 \
                                    icon.padding_right=12 \
                                    icon.highlight_color=0xffdde1e6 \
                                    background.padding_left=-4 \
                                    background.padding_right=-4 \
                                    background.drawing=on \
                                    label.drawing=off \
                                    click_script="yabai -m space --focus \$SID 2>/dev/null"
      done
      
      sketchybar --add item text1 center \
                 --set text1 icon="nyoom engineering:" \
                      icon.font="Liga SFMono Nerd Font:Regular:15.0"
                      
      sketchybar --add item window_title center \
                 --set window_title    script="${scripts}/window_title.sh" \
                                       icon.drawing=off \
                                       label.color=0xffffffff \
                 --subscribe window_title front_app_switched                     
     

      ############## FINALIZING THE SETUP ##############
      sketchybar --update
    '';
  };

  services.skhd = {
    enable = true;
    package = pkgs.skhd;
    skhdConfig = ''
      ## Navigation (lalt - ...)
      # Space Navigation (four spaces per display): lalt - {1, 2, 3, 4}
      lalt - 1 : DISPLAY="$(yabai -m query --displays --display | jq '.index')"; yabai -m space --focus $((1+4*($DISPLAY - 1)))
      lalt - 2 : DISPLAY="$(yabai -m query --displays --display | jq '.index')"; yabai -m space --focus $((2+4*($DISPLAY - 1)))
      lalt - 3 : DISPLAY="$(yabai -m query --displays --display | jq '.index')"; yabai -m space --focus $((3+4*($DISPLAY - 1)))
      lalt - 4 : DISPLAY="$(yabai -m query --displays --display | jq '.index')"; yabai -m space --focus $((4+4*($DISPLAY - 1)))

      # Window Navigation (through display borders): lalt - {h, j, k, l}
      lalt - h : yabai -m window --focus west  || yabai -m display --focus west
      lalt - j : yabai -m window --focus south || yabai -m display --focus south
      lalt - k : yabai -m window --focus north || yabai -m display --focus north
      lalt - l : yabai -m window --focus east  || yabai -m display --focus east

      # Float / Unfloat window: lalt - space
      lalt - space : yabai -m window --toggle float; sketchybar --trigger window_focus

      # Make window zoom to fullscreen: shift + lalt - f
      shift + lalt - f : yabai -m window --toggle zoom-fullscreen; sketchybar --trigger window_focus

      # Make window zoom to parent node: lalt - f 
      lalt - f : yabai -m window --toggle zoom-parent; sketchybar --trigger window_focus

      ## Window Movement (shift + lalt - ...)
      # Moving windows in spaces: shift + lalt - {h, j, k, l}
      shift + lalt - h : yabai -m window --warp west || $(yabai -m window --display west && sketchybar --trigger windows_on_spaces && yabai -m display --focus west && yabai -m window --warp last) || yabai -m window --move rel:-10:0
      shift + lalt - j : yabai -m window --warp south || $(yabai -m window --display south && sketchybar --trigger windows_on_spaces && yabai -m display --focus south) || yabai -m window --move rel:0:10
      shift + lalt - k : yabai -m window --warp north || $(yabai -m window --display north && sketchybar --trigger windows_on_spaces && yabai -m display --focus north) || yabai -m window --move rel:0:-10
      shift + lalt - l : yabai -m window --warp east || $(yabai -m window --display east && sketchybar --trigger windows_on_spaces && yabai -m display --focus east && yabai -m window --warp first) || yabai -m window --move rel:10:0

      # Toggle split orientation of the selected windows node: shift + lalt - s
      shift + lalt - s : yabai -m window --toggle split

      # Moving windows between spaces: shift + lalt - {1, 2, 3, 4, p, n } (Assumes 4 Spaces Max per Display)
      shift + lalt - 1 : DISPLAY="$(yabai -m query --displays --display | jq '.index')";\
                        yabai -m window --space $((1+4*($DISPLAY - 1)));\
                        sketchybar --trigger windows_on_spaces

      shift + lalt - 2 : DISPLAY="$(yabai -m query --displays --display | jq '.index')";\
                        yabai -m window --space $((2+4*($DISPLAY - 1)));\
                        sketchybar --trigger windows_on_spaces

      shift + lalt - 3 : DISPLAY="$(yabai -m query --displays --display | jq '.index')";\
                        yabai -m window --space $((3+4*($DISPLAY - 1)));\
                        sketchybar --trigger windows_on_spaces

      shift + lalt - 4 : DISPLAY="$(yabai -m query --displays --display | jq '.index')";\
                        yabai -m window --space $((4+4*($DISPLAY - 1)));\
                        sketchybar --trigger windows_on_spaces

      shift + lalt - p : yabai -m window --space prev; yabai -m space --focus prev; sketchybar --trigger windows_on_spaces
      shift + lalt - n : yabai -m window --space next; yabai -m space --focus next; sketchybar --trigger windows_on_spaces

      # Mirror Space on X and Y Axis: shift + lalt - {x, y}
      shift + lalt - x : yabai -m space --mirror x-axis
      shift + lalt - y : yabai -m space --mirror y-axis

      ## Stacks (shift + ctrl - ...)
      # Add the active window to the window or stack to the {direction}: shift + ctrl - {h, j, k, l}
      shift + ctrl - h    : yabai -m window  west --stack $(yabai -m query --windows --window | jq -r '.id'); sketchybar --trigger window_focus
      shift + ctrl - j    : yabai -m window south --stack $(yabai -m query --windows --window | jq -r '.id'); sketchybar --trigger window_focus
      shift + ctrl - k    : yabai -m window north --stack $(yabai -m query --windows --window | jq -r '.id'); sketchybar --trigger window_focus
      shift + ctrl - l    : yabai -m window  east --stack $(yabai -m query --windows --window | jq -r '.id'); sketchybar --trigger window_focus

      # Stack Navigation: shift + ctrl - {n, p}
      shift + ctrl - n : yabai -m window --focus stack.next
      shift + ctrl - p : yabai -m window --focus stack.prev

      ## Resize (ctrl + lalt - ...)
      # Resize windows: ctrl + lalt - {h, j, k, l}
      ctrl + lalt - h    : yabai -m window --resize right:-100:0 || yabai -m window --resize left:-100:0
      ctrl + lalt - j    : yabai -m window --resize bottom:0:100 || yabai -m window --resize top:0:100
      ctrl + lalt - k    : yabai -m window --resize bottom:0:-100 || yabai -m window --resize top:0:-100
      ctrl + lalt - l : yabai -m window --resize right:100:0 || yabai -m window --resize left:100:0

      # Equalize size of windows: ctrl + lalt - e
      ctrl + lalt - e : yabai -m space --balance

      # Enable / Disable gaps in current workspace: ctrl + lalt - g
      ctrl + lalt - g : yabai -m space --toggle padding; yabai -m space --toggle gap

      # Enable / Disable window borders in current workspace: ctrl + lalt - b
      ctrl + lalt - b : yabai -m config window_border off 
      shift + ctrl + lalt - b : yabai -m config window_border on

      ## Misc
      # Open new Alacritty window
      cmd - return : alacritty msg create-window
    '';
  };

  networking.hostName = "shaunsingh-laptop";
  system.stateVersion = 4;

  system.keyboard = {
    enableKeyMapping = true;
    remapCapsLockToEscape = true;
  };
  system.defaults = {
    screencapture = { location = "/tmp"; };
    dock = {
      autohide = true;
      showhidden = true;
      mru-spaces = false;
    };
    finder = {
      AppleShowAllExtensions = true;
      QuitMenuItem = true;
      FXEnableExtensionChangeWarning = true;
    };
    NSGlobalDomain = {
      AppleKeyboardUIMode = 3;
      ApplePressAndHoldEnabled = false;
      AppleFontSmoothing = 1;
      _HIHideMenuBar = true;
      InitialKeyRepeat = 10;
      KeyRepeat = 1;
      "com.apple.mouse.tapBehavior" = 1;
      "com.apple.swipescrolldirection" = true;
    };
  };
}
