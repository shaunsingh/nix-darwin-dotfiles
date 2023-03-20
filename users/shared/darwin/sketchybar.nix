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
  services.sketchybar = {
    enable = true;
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
}
