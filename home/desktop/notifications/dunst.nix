{
  pkgs,
  lib,
  inputs,
  config,
  ...
}: {
  services.dunst = {
    enable = true;
    settings = with config.lib.base16.theme; {
      global = {
        # gen settings
        follow = "mouse";
        width = 300;
        origin = "top-left";
        notification_limit = 0;
        offset = "18x18";
        icon_position = "off";
        # progress
        progress_bar_height = 9;
        progress_bar_frame_width = 0;
        # other gen
        padding = 18;
        horizontal_padding = 18;
        frame_width = 0;
        gap_size = 9;
        font = "Liga SFMono Nerd Font 11";
        format = "<span size='x-large' font_desc='Liga SFMono Nerd Font 9' weight='bold' foreground='#${base04-hex}'>%a</span>\\n%s\\n%b";
        show_indicators = false;
        mouse_left_click = "do_action";
        mouse_middle_click = "close_all";
        mouse_right_click = "close_current";
        ellipsize = "end";
        markup = "full";
      };

      # disable notifs in fullscreen
      fullscreen_delay_everything = {fullscreen = "delay";};

      # colors
      urgency_low = {
        timeout = 3;
        background = "#${baseBLEND-hex}";
        foreground = "#${base04-hex}";
        highlight = "#${base0E-hex}";
      };
      urgency_normal = {
        timeout = 6;
        background = "#${baseBLEND-hex}";
        foreground = "#${base04-hex}";
        highlight = "#${base08-hex}";
      };
      urgency_critical = {
        timeout = 0;
        background = "#${baseBLEND-hex}";
        foreground = "#${base04-hex}";
        highlight = "#${base0C-hex}";
      };
    };
  };
  services.poweralertd.enable = true;
}
