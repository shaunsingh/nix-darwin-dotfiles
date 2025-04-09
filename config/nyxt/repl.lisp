(in-package #:nyxt-user)

;; REPL

;; this was removed in pre-release4, re-implement

;; style
(define-configuration nyxt/mode/repl:repl-mode
  ((style (str:concat
            %slot-value%
            (theme:themed-css (theme *browser*)
              `(".input-area"
                :background-color ,*base02-*)
              `("#cells"
                :overflow "clip")
              `("code"
                :font-family ,*font*
                :font-size "18px"
                :background "transparent"
                :color ,*base05-*
                :margin "9px")
              `("textarea"
                :background-color ,*base01-*
                :color ,*base05-*
                :padding "9px"
                :padding-top "6px"
                :padding-bottom "12px")
              `(".cell-actions"
                :margin-left "13px")
              `("code, textarea, .cell-actions"
                :margin "9px 9px 0px 9px"))))))
