{ pkgs
, lib
, inputs
, config
, ...
}: {
  wayland.windowManager.river = {
    enable = true;
    systemdIntegration = true;
    extraSessionVariables.XDG_SESSION_DESKTOP = "river";
    config = {
      focusFollowsCursor = false;
      layoutGenerator.arguments = "-view-padding 6 -outer-padding 12";
      backgroundColor = "${config.lib.base16.theme.base04}";
      border = {
        color = with config.lib.base16.theme; {
          focused = "${base05}";
          unfocused = "${base01}";
        };
        width = 9;
      };
      keybindings =
        let
          mod = "Mod4";
          pwr = base: exp: lib.foldl (x: _: x * base) 1 (lib.range 1 exp);
          allTags = (pwr 2 32) - 1;
          concatAttrs = lib.fold (x: y: x // y) { };
          tagBinds =
            concatAttrs
              (map
                (i:
                  let
                    tags = pwr 2 (i - 1);
                  in
                  {
                    "${mod} ${toString i}" = "spawn 'riverctl set-focused-tags ${toString tags} && ${pkgs.eww}/bin/eww update active-tag=${toString tags}'";
                    "${mod}+Shift ${toString i}" = "spawn 'riverctl set-view-tags ${toString tags} && ${pkgs.eww}/bin/eww update passive-tag=${toString tags}'";
                    "${mod}+Control ${toString i}" = "spawn 'riverctl toggle-focused-tags ${toString tags} && ${pkgs.eww}/bin/eww update active-tag=${toString tags}'";
                    "${mod}+Shift+Control ${toString i}" = "spawn 'riverctl toggle-view-tags ${toString tags} && ${pkgs.eww}/bin/eww update passive-tag=${toString tags}'";
                  })
                (lib.range 1 9));
        in
        {
          normal =
            tagBinds
            // {
              "${mod} Q" = "close";
              "${mod}+Shift Return" = "zoom";
              "${mod} Z" = "focus-output next";
              "${mod}+Shift Z" = "send-to-output next";
              "${mod} Space" = "toggle-float";
              "${mod} F" = "toggle-fullscreen";

              "${mod} J" = "focus-view next";
              "${mod} K" = "focus-view previous";
              "${mod}+Shift J" = "swap next";
              "${mod}+Shift K" = "swap previous";

              "${mod} H" = ''send-layout-cmd rivertile "main-ratio -0.05"'';
              "${mod} L" = ''send-layout-cmd rivertile "main-ratio +0.05"'';
              "${mod}+Shift H" = ''send-layout-cmd rivertile "main-count +1"'';
              "${mod}+Shift L" = ''send-layout-cmd rivertile "main-count -1"'';

              "${mod} 0" = "set-focused-tags ${toString allTags}";
              "${mod}+Shift 0" = "set-view-tags ${toString allTags}";

              "${mod} F11" = "enter-mode passthrough";

              "${mod} Return" = "spawn '${pkgs.foot}/bin/foot'";
              "${mod} D" = "spawn '${pkgs.kickoff}/bin/kickoff'";
              "${mod} P" = "spawn '${pkgs.screenshot}/bin/screenshot'";
              "${mod}+Shift P" = "spawn '${pkgs.ocrScript}/bin/wl-ocr'";
              "${mod} V" = "spawn '${pkgs.volume}/bin/volume -d 5'";
              "${mod} B" = "spawn '${pkgs.volume}/bin/volume -i 5'";
              "${mod}+Shift V" = "spawn '${pkgs.playerctl}/bin/playerctl play-pause'";
              "${mod}+Shift B" = "spawn '${pkgs.volume}/bin/volume -t'";
              "${mod} N" = "spawn '${pkgs.brightness}/bin/brightness set 5%-'";
              "${mod} M" = "spawn '${pkgs.brightness}/bin/brightness set 5%+'";
              "${mod}+Shift N" = "spawn '${pkgs.playerctl}/bin/playerctl previous'";
              "${mod}+Shift M" = "spawn '${pkgs.playerctl}/bin/playerctl next'";
            };
          pointer = {
            "${mod} BTN_LEFT" = "move-view";
            "${mod} BTN_RIGHT" = "resize-view";
          };
          passthrough = {
            "${mod} F11" = "enter-mode normal";
          };
          locked = { };
        };
    };
    extraConfig = ''
      # configure floats and popups
      riverctl float-filter-add app-id 'float'
      riverctl float-filter-add app-id 'popup'

      # input config
      configure_pointers() {
          riverctl list-inputs \
              | grep 'type: pointer' -B 1 \
              | grep -vE 'type: pointer|^--$' \
              | xargs -I '{}' riverctl input '{}' $@
      }

      # Touchpad config
      configure_pointers tap enabled
      configure_pointers accel-profile flat
      configure_pointers natural-scroll enabled
      configure_pointers disable-while-typing disabled
      riverctl hide-cursor when-typing disabled
    '';
  };
  services.kanshi.systemdTarget = "river-session.target";
  systemd.user.services.launch-eww = {
    Unit = {
      Description = "Eww Bar";
      PartOf = [ "river-session.target" ];
    };
    Service = {
      ExecStart = "${config.programs.eww.package}/bin/eww open bar && ${pkgs.eww-wayland-git}/bin/eww open bar2";
      Restart = "on-failure";
    };
    Install.WantedBy = [ "river-session.target" ];
  };
}
