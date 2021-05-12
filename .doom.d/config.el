;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;personal info
(setq user-full-name "Shaurya Singh"
      user-mail-address "shaunsingh@gmail.com")

;;fonts
(setq doom-font (font-spec :family "SF Mono" :size 12)
      doom-variable-pitch-font (font-spec :family "SF Pro Display" :size 12.5)
      ivy-posframe-font (font-spec :family "SF Mono" :size 12.5))

;;set theme
(load-theme 'doom-moonlight t)

;;neotree theme
(setq doom-themes-neotree-file-icons t)

(require 'ivy-posframe)
;; display at `ivy-posframe-style'
(setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center)))
(ivy-posframe-mode 1)

;;org direcotry
(setq org-directory "~/org/")
(setq evil-want-fine-undo t)                       ; By default while in insert all changes are one big blob. Be more granular

;;modeline (icons, config, battery)
(display-time-mode 1)                             ; Enable time in the mode-line
(display-battery-mode 1)
(setq doom-modeline-icon (display-graphic-p))
(setq doom-modeline-major-mode-icon t)
(setq doom-modeline-major-mode-color-icon t)
(setq doom-modeline-enable-word-count t)

;;default mode org mode
(setq-default major-mode 'org-mode)

;;cute start screen
(setq fancy-splash-image "/Users/shauryasingh/.doom.d/cute-demon.png")
(setq +doom-dashboard-banner-padding '(0 . 4))

;;tell company to always do autocomplete
(after! company
  (setq company-idle-delay 0.2
        company-dabbrev-downcase 0
        company-minimum-prefix-length 1
        company-show-numbers t))
;;
(set-company-backend!
  '(text-mode
    markdown-mode
    gfm-mode)
  '(:seperate
    company-ispell
    company-files
    company-yasnippet))

;;ligatures in org mode
(setq +ligatures-in-modes '(org-mode))
(setq +ligatures-extras-in-modes '(org-mode))

;;java home for java-lsp
(setenv "JAVA_HOME"  "/Library/Java/JavaVirtualMachines/zulu-11.jdk/Contents/Home")
(setq lsp-java-java-path "/Library/Java/JavaVirtualMachines/zulu-11.jdk/Contents/Home/bin/java")

(global-subword-mode 1)
