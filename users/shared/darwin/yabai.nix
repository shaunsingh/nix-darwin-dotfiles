{ pkgs
, lib
, inputs
, config
, ...
}: 
let
  scripts = ../../../configs/sketchybar;
in
{
  services.yabai = {
    enable = true;
    enableScriptingAddition = true;
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
}
