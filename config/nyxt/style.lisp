(in-package #:nyxt-user)

;;; COLORSCHEME

(defmacro define-palette (&rest colors)
  "Helper macro to set global variables for `theme' colors"
  `(progn ,@(loop for (name hex)
                  in colors
                  collect `(defparameter ,name ,hex "Color used for `theme'"))))

(defun make-important (property)
  (str:concat property " !important"))

;; oxocarbon dark
(define-palette (*base00-* "#161616")
                (*base01-* "#262626")
                (*base02-* "#393939")
                (*base03-* "#525252")
                (*base04-* "#dde1e6")
                (*base05-* "#f2f4f8")
                (*base06-* "#ffffff")
                (*base07-* "#08bdba")
                (*base08-* "#3ddbd9")
                (*base09-* "#78a9ff")
                (*base0A-* "#ee5396")
                (*base0B-* "#33b1ff")
                (*base0C-* "#ff7eb6")
                (*base0D-* "#42be65")
                (*base0E-* "#be95ff")
                (*base0F-* "#82cfff")
                (*font* "SF Pro Display")
                (*mono* "Liga SFMono Nerd Font"))

;; internal pages
(define-configuration :web-buffer
  ((style
    (theme:themed-css (theme *browser*) 
      `(body
        :background-color ,*base00-*
        :color ,*base04-*
        :font-family ,*font*
        :margin "4% 6%")
      `("details > summary"
        :list-style "none")
      `("h1"
        :color ,*base06-*)
      `("#subtitle"
        :color ,*base0C-*)
      `("h1, #subtitle"
        :font-size "63px"
        :margin-bottom "-3px")
      `("h2"
        :font-size "27px"
        :margin "0"
        :margin-bottom "-3px"
        :color ,*base0C-*)
      `("h3"
        :font-size "18px"
        :color ,*base0E-*)
      `("h4"
        :color ,*base0F-*)
      `("h5"
        :color ,*base09-*)
      `("pre"
        :padding "9px"
        :padding-top "6px"
        :padding-bottom "12px")
      `("h2, h3, h4, h5, p"
        :margin-left "9px") 
      `("a, li, ul, pre"
        :color ,*base05-*)
      `(".action"
        :color ,*base0B-*)
      `("a code, p code, code, pre"
        :text-wrap "wrap"
        :font-family ,*mono*
        :background-color ,*base01-*
        :color ,*base06-*)
      `("a:hover, a:active"
        :color ,*base06-*)
      `("#buttons"
        :margin-left "13px")
      `(".button"
        :margin "3px")
      `("hr, .button"
        :background-color ,*base01-*
        :border-color ,*base01-*
        :color ,*base04-*
        :border-radius "0")
      `(".button:hover"
        :background-color ,*base02-*
        :color ,*base06-*)))))

;; prompt
(define-configuration :prompt-buffer
  ((style
    (theme:themed-css (theme *browser*)
      `(*
        :font-family ,*mono*
        :font-size "13px")
      `(body
        :background-color ,*base00-*
        :color ,*base04-*
        :margin "0")
      '("#root"
        :height "100%"
        :display "grid"
        :grid-template-rows "auto 1fr")
      `("#prompt-area"
        :background-color ,*base02-*
        :color ,*base05-*
        :overflow "hidden"
        :white-space "nowrap"
        :display "grid"
        :grid-template-columns "auto auto 1fr auto auto")
      `("#prompt"
        :text-overflow "ellipsis")
      `("#prompt, #prompt-input, #prompt-modes, #close-button"
        :padding "3px"
        :padding-top "6px"
        :padding-left "9px")
      `("#prompt-modes, #close-button"
        :padding-right "3px"
        :padding-left "3px"
        :background-color ,*base01-*)
      `("#prompt-extra"
        :padding-right "9px")
      `("#prompt-modes"
        :padding-left "9px")
      `("#prompt-input"
        :background-color ,*base01-*
        :min-width "10ch")
      `("#close-button"
        :text-align "right")
      `(button
        :color ,*base04-*
        :background "transparent"
        :text-decoration "none"
        :border "none"
        :font "inherit"
        :outline "inherit")
      `((:and .button :hover)
        :cursor "pointer"
        :color ,*base06-*
        :font-weight "bold")
      `(".button svg path"
        :stroke ,*base04-*)
      `(".button:hover svg path"
        :stroke ,*base06-*)
      `(input
        :background-color ,*base01-*
        :padding "0"
        :border-image-width "0")
      `("#input"
        :border "none"
        :color ,*base06-*
        :outline "none"
        :width "100%"
        :autofocus "true")
      '(".source"
        :margin-top "2px")
      `(".source-name"
        :background-color ,*base01-*
        :color ,*base05-*
        :display "flex"
        :justify-content "space-between"
        :align-items "stretch")
      '(".source-name > div"
        :line-height "26px")
      '(".source-name > div > button"
        :padding "5px 5px 5px 0px"
        :min-height "100%")
      `("#next-source > svg"
        :margin-left "9px")
      `("#next-source > svg, #previous-source > svg"
        :stroke ,*base06-*
        :margin-bottom "2px"
        :height "5px")
      '("#previous-source"
        :padding 0)
      '("#next-source"
        :padding 0)
      `("#suggestions"
        :color ,*base05-*
        :margin-right "3px"
        :overflow "hidden")
      `(".suggestion-and-mark-count"
        :font-family ,*mono*)
      `(".source-content"
        :box-sizing "border-box"
        :padding-left "16px"
        :margin-left "2px"
        :width "100%"
        :table-layout "fixed"
        (td
         :color ,*base05-*
         :overflow "hidden"
         :text-overflow "ellipsis"
         :white-space "nowrap"
         :height "20px"
         :padding-left "4px")
        ("tr:not(:first-child)"
         :font-family ,*mono*)
        ("tr:hover"
         :background-color ,*base0C-*
         ;;:color ,*base00-*
         :cursor "pointer"
         :font-weight "bold")
        (th
         :background-color ,*base01-*
         :color ,*base06-*
         :font-weight "normal"
         :padding-left "4px"
         :text-align "left"))
      `("#selection"
        :background-color ,*base0B-*)
      `(.marked
        :background-color ,*base0B-*)
      `(.selected
        :background-color ,*base06-*)))))

;; message buffer
(define-configuration (window)
 ((message-buffer-height 21)
  (message-buffer-style
   (theme:themed-css (theme *browser*)
    `(*
      :font-family ,*mono*
      :font-size "11px")
    `(body
      :background-color ,*base00-*
      :color ,*base05-*
      :padding 0
      :padding-left "9px"
      :margin "3px")))))

;; gopher etc. 
(define-configuration nyxt/mode/small-web:small-web-mode
  ((style (str:concat
            %slot-value%
            (theme:themed-css (theme *browser*)
              `("pre"
                :background-color ,*base00-*)
              `("a.button.search"
                :color ,*base04-*
                :border-color ,*base04-*)
              `("a.button.error"
                :color ,*base0C-*
                :border-color ,*base0C-*))))))
