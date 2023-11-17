{ config
, pkgs
, inputs
, lib
, withRiver
, withSway
, ...
}:
let
  dependencies = with pkgs;
    [
      kickoff
      brightnessctl
      pamixer
      coreutils
    ]
    ++ lib.optionals withRiver [
      river
    ]
    ++ lib.optionals withSway [
      sway
    ];
  ewwYuck = pkgs.writeText "eww.yuck" (''
    (defwidget bar []
      (centerbox :orientation "v"
                 :halign "center"
        (box :class "segment-top"
             :valign "start"
             :orientation "v"
          (tags))
        (box :class "segment-center"
             :valign "center"
             :orientation "v"
          (time)
          (date))
        (box :class "segment-bottom"
             :valign "end"
             :orientation "v"
          (menu)
          (brightness)
          (volume)
          (battery)
          (current-tag))))

    (defwidget time []
      (box :class "time"
           :orientation "v"
        hour min type))

    (defwidget date []
      (box :class "date"
           :orientation "v"
        year month day))

    (defwidget menu []
      (button :class "icon"
              :orientation "v"
              :onclick "''${EWW_CMD} open --toggle notifications-menu"
         ""))

    (defwidget brightness []
      (button :class "icon"
              :orientation "v"
        (circular-progress :value brightness-level
                           :thickness 3)))

    (defwidget volume []
      (button :class "icon"
              :orientation "v"
        (circular-progress :value volume-level
                           :thickness 3)))

    (defwidget battery []
      (button :class "icon"
              :orientation "v"
              :onclick ""
        (circular-progress :value "''${EWW_BATTERY['macsmc-battery'].capacity}"
                           :thickness 3)))

    (defwidget current-tag []
      (button :class "current-tag"
              :orientation "v"
              :onclick "kickoff & disown"
        "''${active-tag}"))

    (defvar active-tag "1")
    (defpoll hour :interval "1m" "date +%I")
    (defpoll min  :interval "1m" "date +%M")
    (defpoll type :interval "1m" "date +%p")

    (defpoll day   :interval "10m" "date +%d")
    (defpoll month :interval "1h"  "date +%m")
    (defpoll year  :interval "1h"  "date +%y")

    ;; this is updated by the helper script
    (defvar brightness-level 66)
    (defvar volume-level 33)
  ''
  + (
    if withSway
    then ''
      (defwidget tags []
        (box :class "tags"
             :orientation "v"
             :halign "center"
          (for tag in tags
            (box :class {active-tag == tag.tag ? "active" : "inactive"}
              (button :onclick "swaymsg workspace ''${tag.tag} ; ''${EWW_CMD} update active-tag=''${tag.tag}"
                "''${tag.label}")))))

      (defvar tags '[{ "tag": 1, "label": "一" },
                     { "tag": 2, "label": "二" },
                     { "tag": 3, "label": "三" },
                     { "tag": 4, "label": "四" },
                     { "tag": 5, "label": "五" },
                     { "tag": 6, "label": "六" },
                     { "tag": 7, "label": "七" },
                     { "tag": 8, "label": "八" },
                     { "tag": 9, "label": "九" },
                     { "tag": 0, "label": "" }]')

    ''
    else ''
      (defwidget tags []
        (box :class "tags"
             :orientation "v"
             :halign "center"
          (for tag in tags
            (box :class {active-tag == tag.tag ? "active" : "inactive"}
              (button :onclick "riverctl set-focused-tags ''${tag.tag} ; ''${EWW_CMD} update active-tag=''${tag.tag}"
                "''${tag.label}")))))

      (defvar tags '[{ "tag": 1, "label": "一" },
                     { "tag": 2, "label": "二" },
                     { "tag": 4, "label": "三" },
                     { "tag": 8, "label": "四" },
                     { "tag": 16, "label": "五" },
                     { "tag": 32, "label": "六" },
                     { "tag": 64, "label": "七" },
                     { "tag": 128, "label": "八" },
                     { "tag": 256, "label": "九" }]')

    ''
  )
  + ''
    (defwindow bar
      :monitor 0
      :stacking "bottom"
      :geometry (geometry
                  :height "100%"
                  :anchor "left center")
      :exclusive true
      (bar))

    (defwindow bar2
      :monitor 1
      :stacking "bottom"
      :geometry (geometry
                  :height "100%"
                  :anchor "left center")
      :exclusive true
      (bar))
  '');

  ewwScss = pkgs.writeText "eww.scss" (with config.lib.base16.theme; ''
    $baseTR: rgba(13,13,13,0.13);
    $base00: #${baseBLEND-hex};
    $base01: #${base01-hex};
    $base02: #${base02-hex};
    $base03: #${base03-hex};
    $base04: #${base04-hex};
    $base05: #${base05-hex};
    $base06: #${base06-hex};
    $base07: #${base07-hex};
    $base08: #${base08-hex};
    $base09: #${base09-hex};
    $base0A: #${base0A-hex};
    $base0B: #${base0B-hex};
    $base0C: #${base0C-hex};
    $base0D: #${base0D-hex};
    $base0E: #${base0E-hex};
    $base0F: #${base0F-hex};
    $baseIBM: #${baseIBM-hex};

    * {
      all: unset;
    }

    window {
      font-family: "Liga SFMono Nerd Font";
      font-size: 13px;
      background-color: rgba(0,0,0,0);
      color: $base04;
      & > * {
        margin: 0px 0px 12px 12px;
      }
    }

    .tags {
      margin-top: 9px;
    }

    .active {
      color: $base06;
      padding: 6px 9px 6px 6px;
      background-color: $baseTR;
      border-left: 3px solid $base0C;
    }

    .segment-center {
      margin-top: 18px;
      padding: 9px;
    }

    .time {
      color: $base06;
      font-weight: bold;
      margin-bottom: 6px;
    }

    .date {
      margin-top: 6px;
    }

    .icon {
      background-color: $base01;
      padding: 9px;
      margin: 4.5px 0px;
      border-radius: 3px;
    }

    .current-tag {
      color: $base00;
      background-color: $base0E;
      padding: 9px;
      margin: 4.5px 0px;
      border-radius: 3px;
    }
  '');

  ewwConf = pkgs.linkFarm "ewwConf" [
    {
      name = "eww.scss";
      path = ewwScss;
    }
    {
      name = "eww.yuck";
      path = ewwYuck;
    }
  ];
in
{
  programs.eww = {
    enable = true;
    package = pkgs.eww-wayland-git;
    configDir = ewwConf;
  };
  systemd.user.services.eww = {
    Unit = {
      Description = "Eww daemon";
      PartOf = [ "graphical-session.target" ];
    };
    Service = {
      Environment = "PATH=/run/wrappers/bin:${lib.makeBinPath dependencies}";
      ExecStart = "${config.programs.eww.package}/bin/eww daemon --no-daemonize";
      Restart = "on-failure";
    };
    Install.WantedBy = [ "graphical-session.target" ];
  };
}
