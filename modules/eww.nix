{ config
, pkgs
, inputs
, lib
, ...
}:
let
  ewwYuck = pkgs.writeText "eww.yuck" ''
;; bar
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
    
;; (defwidget tags []
;;   (box :class "tags"
;;        :orientation "v"
;;        :halign "center"
;;     (for tag in tags
;;       (box :class {active-tag == tag.tag ? "active" : "inactive"}
;;         (button :onclick "riverctl set-focused-tags ''${tag.tag} ; ''${EWW_CMD} update active-tag=''${tag.tag}"
;;           "''${tag.label}")))))

(defwidget tags []
  (box :class "tags"
       :orientation "v"
       :halign "center"
    (for tag in tags
      (box :class {active-tag == tag.tag ? "active" : "inactive"}
        (button :onclick "swaymsg workspace ''${tag.tag} ; ''${EWW_CMD} update active-tag=''${tag.tag}"
          "''${tag.label}")))))

(defwidget time []
  (box :class "time"
       :orientation "v" 
    hour min))

(defwidget date []
  (box :class "date"
       :orientation "v" 
    day month))

(defwidget menu []
  (button :class "icon"
          :orientation "v"
          :onclick "''${EWW_CMD} open --toggle notifications-menu"
     ""))

(defwidget brightness []
  (button :class "icon"
          :orientation "v"
          :onclick "''${EWW_CMD} open --toggle brightness-controls"
    (circular-progress :value brightness-level
                       :thickness 3)))

(defwidget volume []
  (button :class "icon"
          :orientation "v"
          :onclick "''${EWW_CMD} open --toggle volume-controls"
    (circular-progress :value volume-level
                       :thickness 3)))

(defwidget battery []
  (button :class "icon"
          :orientation "v"
          :onclick ""
    (circular-progress :value battery-level
                       :thickness 3)))

(defwidget current-tag []
  (button :class "current-tag"
          :orientation "v"
          :onclick "${pkgs.rofi-wayland}/bin/rofi -no-lazy-grab -show drun -theme index & disown"
    "''${active-tag}"))

;; Variables
;; (defvar tags '[{ "tag": 1, "label": "一" }, 
;;                { "tag": 2, "label": "二" }, 
;;                { "tag": 4, "label": "三" }, 
;;                { "tag": 8, "label": "四" }, 
;;                { "tag": 16, "label": "五" }, 
;;                { "tag": 32, "label": "六" }, 
;;                { "tag": 64, "label": "七" }, 
;;                { "tag": 128, "label": "八" }, 
;;                { "tag": 256, "label": "九" }]')

(defvar tags '[{ "tag": 1, "label": "一" }, 
               { "tag": 2, "label": "二" }, 
               { "tag": 3, "label": "三" }, 
               { "tag": 4, "label": "四" }, 
               { "tag": 5, "label": "五" }, 
               { "tag": 6, "label": "六" }, 
               { "tag": 7, "label": "七" }, 
               { "tag": 8, "label": "八" },
               { "tag": 9, "label": "九" }]')

(defvar active-tag "1")

(defpoll hour :interval "1m" "date +%H")
(defpoll min  :interval "1m" "date +%M")

(deflisten brightness-level 
   `${pkgs.brightnessctl}/bin/brightnessctl -m -d apple-panel-bl | cut -d, -f4 | tr -d %`)
(deflisten volume-level 
   `${pkgs.pamixer}/bin/pamixer --get-volume`)
(deflisten battery-level :initial 100
   `cat /sys/class/power_supply/macsmc-battery/capacity`)

(defpoll day   :interval "10m" "date +%d")
(defpoll month :interval "1h"  "date +%m")

(defwindow bar
  :monitor 0
  :stacking "fg"
  :geometry (geometry 
              :x 0 
              :y 0 
              :height "100%" 
              :anchor "left center")
  :exclusive true
  (bar))

;; popups for brightness/volume
(defwidget brightness-controls []
  (box :class "menu-controls"
    (button :class "control"
            :onclick "${pkgs.brightness}/bin/brightness set 5%+ && ''${EWW_CMD} update brightness-level=`${pkgs.brightnessctl}/bin/brightnessctl -m -d apple-panel-bl | cut -d, -f4 | tr -d %`"
       "+")
    (button :class "control"
            :onclick "${pkgs.brightness}/bin/brightness set 5%- && ''${EWW_CMD} update brightness-level=`${pkgs.brightnessctl}/bin/brightnessctl -m -d apple-panel-bl | cut -d, -f4 | tr -d %`"
       "-")))

(defwidget volume-controls []
  (box :class "menu-controls"
    (button :class "control"
            :onclick "${pkgs.volume}/bin/volume -i 5 && ''${EWW_CMD} update volume-level=`${pkgs.pamixer}/bin/pamixer --get-volume`"
       "+")
    (button :class "control"
            :onclick "${pkgs.volume}/bin/volume -d 5 && ''${EWW_CMD} update volume-level=`${pkgs.pamixer}/bin/pamixer --get-volume`"
       "-")))

(defwindow brightness-controls
  :monitor 0
  :stacking "fg"
  :geometry (geometry 
              :x 0 
              :y 0 
              :width "30%" 
              :height "15%" 
              :anchor "center")
  (brightness-controls))

(defwindow volume-controls
  :monitor 0
  :stacking "fg"
  :geometry (geometry 
              :x 0 
              :y 0 
              :width "30%" 
              :height "15%" 
              :anchor "center")
  (volume-controls))
  '';

  ewwScss = pkgs.writeText "eww.scss" ''
* {
  all: unset;
}

window {
  font-family: "Liga SFMono Nerd Font";
  font-size: 13px;
  background: #161616;
  color: #dde1e6;
}

.tags {
  margin-top: 9px;
}

.active {
  color: #ffffff;
  background-color: #262626;
  padding: 6px 9px 6px 6px;
  border-left: 3px solid #ff7eb6;
  border-radius: 3px;
}

.segment-center {
  background-color: #262626;
  padding: 9px;
  margin: 0px 9px 9px;
  border-radius: 3px;
}

.time {
  color: #ffffff;
  font-weight: bold;
  margin-bottom: 6px;
}

.date {
  margin-top: 6px;
}

.icon {
  background-color: #262626;
  padding: 9px 9px;
  margin: 0px 9px 9px;
  border-radius: 3px;
}

.current-tag {
  color: #161616;
  background-color: #be95ff;
  padding: 9px 0px 9px;
  margin: 0px 9px 9px;
  border-radius: 3px;
}

.menu-controls {
  font-size: 45px;
}

.control {
  background-color: #262626;
  margin: 15px;
}
  '';

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
    package = pkgs.eww;
    configDir = ewwConf;
  };
}
