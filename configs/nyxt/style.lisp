(in-package #:nyxt-user)

;; This automatically darkens WebKit-native interfaces and sends the
;; "prefers-color-scheme: dark" to all the supporting websites.
(setf (uiop:getenv "GTK_THEME") "Adwaita:dark")

;; This only works on the versions of Nyxt after 3.0. For the
;; backwards-compatible solution, see previous versions of this
;; file.
(define-configuration browser
  ((theme (make-instance
           'theme:theme
           :dark-p t
           :background-color "#161616"
           :on-background-color "#ffffff"
           :accent-color "#ff7eb6"
           :primary-color "#dde1e6"
           :on-primary-color "#161616"
           :secondary-color "f2f4f8"
           :on-secondary-color "#ffffff"))))

;;; Dark-mode is a simple mode for simple HTML pages to color those in
;;; a darker palette. I don't like the default gray-ish colors,
;;; though. Thus, I'm overriding those to be a bit more laconia-like.
(define-configuration nyxt/style-mode:dark-mode
  ((style
    #+(and nyxt-3 (not nyxt-3-pre-release-1))
    (theme:themed-css (theme *browser*)
      `(*
        :background-color ,(if (theme:dark-p theme:theme)
                               theme:background
                               theme:on-background)
        "!important"
        :background-image none "!important"
        :color ,(if (theme:dark-p theme:theme)
                    theme:on-background
                    theme:background)
        "!important")
      `(a
        :background-color ,(if (theme:dark-p theme:theme)
                               theme:background
                               theme:on-background)
        "!important"
        :background-image none "!important"
        :color ,theme:primary "!important"))
    #+nyxt-3-pre-release-1
    (theme:themed-css (theme *browser*)
      (*
       :background-color (str:concat
                          (if (theme:dark-p theme:theme)
                              theme:background
                              theme:on-background)
                          " !important")
       :background-image "none !important"
       :color (str:concat
               (if (theme:dark-p theme:theme)
                   theme:on-background
                   theme:background)
               " !important"))
      (a
       :background-color (str:concat
                          (if (theme:dark-p theme:theme)
                              theme:background
                              theme:on-background)
                          " !important")
       :background-image "none !important"
       :color (str:concat theme:primary " !important"))))))
