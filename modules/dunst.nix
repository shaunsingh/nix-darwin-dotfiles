{ pkgs
, lib
, inputs
, config
, ...
}:
{
  services.dunst = {
    enable = true;

    settings = with config.lib.base16.theme; {
      global = {
        follow = "mouse";
        width = 300;
        origin = "top-center";
        alignment = "left";
        vertical_alignment = "center";
        ellipsize = "middle";
        offset = "15x15";
        padding = 18;
        horizontal_padding = 18;
        progress_bar = true;
        progress_bar_height = 8;
        progress_bar_min_width = 150;
        progress_bar_max_width = 300;
        progress_bar_frame_width = 0;
        frame_width = 0;
        separator_height = 2;
        transparency = 0;
        gap_size = 8;
        line_height = 0;
        notification_limit = 0;
        idle_threshold = 120;
        history_length = 20;
        show_age_threshold = 60;
        markup = "full";
        font = "Liga SFMono Nerd Font 12";
        format = "<span size='x-large' font_desc='Liga SFMono Nerd Font 9' weight='bold' foreground='#${base04-hex}'>%a</span>\\n%s\\n%b";
        word_wrap = "yes";
        sort = "yes";
        shrink = "no";
        indicate_hidden = "yes";
        sticky_history = "yes";
        ignore_newline = "no";
        show_indicators = "no";
        stack_duplicates = true;
        always_run_script = true;
        hide_duplicate_count = false;
        ignore_dbusclose = false;
        force_xwayland = false;
        force_xinerama = false;
        mouse_left_click = "do_action";
        mouse_middle_click = "close_all";
        mouse_right_click = "close_current";
      };

      fullscreen_delay_everything = { fullscreen = "delay"; };

      urgency_low = {
        timeout = 3;
        background = "#${base00-hex}";
        foreground = "#${base04-hex}";
        highlight = "#${base0E-hex}";
      };
      urgency_normal = {
        timeout = 6;
        background = "#${base00-hex}";
        foreground = "#${base04-hex}";
        highlight = "#${base08-hex}";
      };
      urgency_critical = {
        timeout = 0;
        background = "#${base00-hex}";
        foreground = "#${base04-hex}";
        highlight = "#${base0C-hex}";
      };
    };
  };
}
