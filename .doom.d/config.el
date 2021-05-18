;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;personal info
(setq user-full-name "Shaurya Singh"
      user-mail-address "shaunsingh0207@gmail.com")

;;fonts
(setq doom-font (font-spec :family "SF Mono" :size 12)
      doom-variable-pitch-font (font-spec :family "SF Pro Display" :size 12.5))

;;set theme
;;(setq doom-theme 'doom-nord)
(setq doom-theme 'doom-moonlight)

;;ivy vs-code style
(require 'ivy-posframe)
(setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center)))

;;org directory
(setq org-directory "~/org/")
(setq evil-want-fine-undo t)                       ; By default while in insert all changes are one big blob. Be more granular

;;modeline (icons, config, battery)
(display-time-mode 1)                             ; Enable time in the mode-line
(display-battery-mode 1)
(setq doom-modeline-major-mode-icon t)
(setq doom-modeline-enable-word-count t)

;;cute start screen
(setq fancy-splash-image "/Users/shauryasingh/.doom.d/cute-demon.png")
(setq +doom-dashboard-banner-padding '(0 . 4))

;;tell company to always do autocomplete
(after! company
  (setq company-idle-delay 0.2
        company-dabbrev-downcase 0))
(set-company-backend!
  '(text-mode
    markdown-mode
    gfm-mode)
  '(:seperate
    company-ispell
    company-files
    company-yasnippet))

;;java home for java-lsp
(setenv "JAVA_HOME"  "/Library/Java/JavaVirtualMachines/zulu-11.jdk/Contents/Home")
(setq lsp-java-java-path "/Library/Java/JavaVirtualMachines/zulu-11.jdk/Contents/Home/bin/java")

;;keybindings
(map! :leader
      :desc "hop to word"
      "w w" #'avy-goto-word-0)


(map! :leader
      :desc "hop to line"
      "l" #'avy-goto-line)

;;(add-to-list 'default-frame-alist '(fullscreen . fullboth))
;;(setq ns-use-native-fullscreen t)
