# Mac.nix
# There are mac-specific tweaks I need to do. In the future if I switch to nixOS full-time, then I wuold likely need to remove the mac-specific packages. An easy way to do this is just keep them in a seperate file:

# [[file:../nix-config.org::*Mac.nix][Mac.nix:1]]
{ pkgs, lib, spacebar, ... }:
# Mac.nix:1 ends here

# Yabai
# Yabai is my tiling WM of choice. As this is an m1 (=aarch64-darwin=) laptop, I use the donaldguy fork, which enables the SA addon on m1 machines

# [[file:../nix-config.org::*Yabai][Yabai:1]]
let
  # yabai = pkgs.yabai.overrideAttrs (old: rec {
  #   src = pkgs.fetchFromGitHub {
  #     owner = "donaldguy";
  #     repo = "yabai";
  #     rev = "00bf5e46685df0cd3e961c155615ddecc75562ab";
  #     sha256 = "sha256-WlUUuT+dtI0eY6egMt6XXjNB0Ca6KiUB9iRc5XPU5x4=";
  #   };
  # });
# Yabai:1 ends here



# Now to configure the package via nix

# [[file:../nix-config.org::*Yabai][Yabai:2]]
in {
  # services.yabai = {
  #   enable = true;
  #   enableScriptingAddition = false;
  #   package = yabai;
  #   config = {
  #     window_border = "off";
  #     # window_border_width = 5;
  #     # active_window_border_color = "0xff3B4252";
  #     # normal_window_border_color = "0xff2E3440";
  #     focus_follows_mouse = "autoraise";
  #     mouse_follows_focus = "off";
  #     mouse_drop_action = "stack";
  #     window_placement = "second_child";
  #     window_opacity = "off";
  #     window_topmost = "on";
  #     window_shadow = "on";
  #     active_window_opacity = "1.0";
  #     normal_window_opacity = "1.0";
  #     split_ratio = "0.50";
  #     auto_balance = "on";
  #     mouse_modifier = "fn";
  #     mouse_action1 = "move";
  #     mouse_action2 = "resize";
  #     layout = "bsp";
  #     top_padding = 18;
  #     bottom_padding = 46;
  #     left_padding = 18;
  #     right_padding = 18;
  #     window_gap = 18;
  #   };
  # };
# Yabai:2 ends here

# Spacebar
# Spacebar is my bar of choice on macOS. Its lighter than any web-based ubersicht bar, and looks nice

# [[file:../nix-config.org::*Spacebar][Spacebar:1]]
  services.spacebar = {
    enable = true;
    package = pkgs.spacebar;
    config = {
      position = "bottom";
      height = 28;
      title = "on";
      spaces = "on";
      power = "on";
      clock = "off";
      right_shell = "off";
      padding_left = 20;
      padding_right = 20;
      spacing_left = 25;
      spacing_right = 25;
      text_font = ''"Menlo:16.0"'';
      icon_font = ''"Menlo:16.0"'';
      background_color = "0xff2E3440";
      foreground_color = "0xffD8DEE9";
      space_icon_color = "0xff8fBcBB";
      power_icon_strip = " ";
      space_icon_strip = "I II III IV V VI VII VIII IX X";
      spaces_for_all_displays = "on";
      display_separator = "on";
      display_separator_icon = "|";
      clock_format = ''"%d/%m/%y %R"'';
      right_shell_icon = " ";
      right_shell_command = "whoami";
    };
  };
# Spacebar:1 ends here

# SKHD
# Skhd is the hotkey daemon for yabai. As yabai is disabled, it makes sense to disable skhd too for the time being

# [[file:../nix-config.org::*SKHD][SKHD:1]]
  # services.skhd = {
  #   enable = true;
  #   package = pkgs.skhd;
  #   skhdConfig = ''
  #     	ctrl + alt - h : yabai -m window --focus west
  #     	ctrl + alt - j : yabai -m window --focus south
  #     	ctrl + alt - k : yabai -m window --focus north
  #     	ctrl + alt - l : yabai -m window --focus east

  #     	# Fill space with window
  #     	ctrl + alt - 0 : yabai -m window --grid 1:1:0:0:1:1

  #     	# Move window
  #     	ctrl + alt - e : yabai -m window --display 1; yabai -m display --focus 1
  #     	ctrl + alt - d : yabai -m window --display 2; yabai -m display --focus 2
  #     	ctrl + alt - f : yabai -m window --space next; yabai -m space --focus next
  #     	ctrl + alt - s : yabai -m window --space prev; yabai -m space --focus prev

  #     	# Close current window
  #     	ctrl + alt - w : $(yabai -m window $(yabai -m query --windows --window | jq -re ".id") --close)

  #     	# Rotate tree
  #     	ctrl + alt - r : yabai -m space --rotate 90

  #     	# Open application
  #     	ctrl + alt - enter : alacritty
  #     	ctrl + alt - e : emacs
  #     	ctrl + alt - b : open -a Safari
  #       ctrl + alt - t : yabai -m window --toggle float;\
  #         yabai -m window --grid 4:4:1:1:2:2
  #       ctrl + alt - p : yabai -m window --toggle sticky;\
  #         yabai -m window --toggle topmost;\
  #         yabai -m window --toggle pip
  #   '';
  # };
# SKHD:1 ends here

# MacOS Settings
# I like my hostname to be the same as the flake's target

# [[file:../nix-config.org::*MacOS Settings][MacOS Settings:1]]
  networking.hostName = "shaunsingh-laptop";
  system.stateVersion = 4;
# MacOS Settings:1 ends here



# Along with that, lets
# - Increase key repeat rate
# - Remap Caps to Esc
# - Save screenshots to =/tmp=
# - Autohide the dock and menubar
# - Show extensions in Finder (and allow it to "quit")
# - Set macOS to use the dark theme
# - Configure Trackpad and mouse behavior
# - Enable subpixel antialiasing on internal/external displays

# [[file:../nix-config.org::*MacOS Settings][MacOS Settings:2]]
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
      AppleInterfaceStyle = "Dark";
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
# MacOS Settings:2 ends here
