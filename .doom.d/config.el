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

;;disable cursorline
(remove-hook 'doom-first-buffer-hook #'global-hl-line-mode)

;;ivy vs-code style
(require 'ivy-posframe)
(setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center)))

;;make treemacs thinner
(after! treemacs
  (setq treemacs-width 25))  ; default is 35

;;let yabai handle frames instead of splits
(setq pop-up-frames t)

;;org directory
(setq org-directory "~/org/")
(setq evil-want-fine-undo t)                       ; By default while in insert all changes are one big blob. Be more granular

;; line numbers
(setq display-line-numbers-type t)

;;modeline (icons, config, battery)
(display-time-mode 1)                             ; Enable time in the mode-line
(display-battery-mode 1)
(setq doom-modeline-major-mode-icon t)
(setq doom-modeline-enable-word-count t)
(setq doom-modeline-modal-icon t)

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
    company-yasnippet
    company-ispell
    company-files))

;;disable line numbers in writeroom mode
(setq +zen-mixed-pitch-modes nil)
(setq +zen-text-scale 0)
(add-hook! 'writeroom-mode-hook (centaur-tabs-local-mode (if writeroom-mode +1 -1)))
;;(add-hook! 'writeroom-mode-hook (minimap-mode (if writeroom-mode +1 -1)))
(add-hook 'writeroom-mode-enable-hook #'doom-disable-line-numbers-h)
(add-hook 'writeroom-mode-disable-hook #'doom-enable-line-numbers-h)


;;java home for java-lsp
(setenv "JAVA_HOME"  "/Library/Java/JavaVirtualMachines/zulu-11.jdk/Contents/Home")
(setq lsp-java-java-path "/Library/Java/JavaVirtualMachines/zulu-11.jdk/Contents/Home/bin/java")

;;keybindings
(map! :leader
      :desc "hop to word" "w w" #'avy-goto-word-0)

(map! :leader
      :desc "hop to line"
      "l" #'avy-goto-line)

;;set emacs to fullscreen (i'm already using a wm)
;;(add-to-list 'default-frame-alist '(fullscreen . fullboth))
;;(setq ns-use-native-fullscreen t)

;;use pdf-tools (not installed rn)
;;(setq +latex-viewers '(pdf-tools))

;;stupid warnings
(setq load-prefer-newer t)

;; transparency for fun
(set-frame-parameter (selected-frame) 'alpha '(92 92))
(add-to-list 'default-frame-alist '(alpha 92 92))

;;minimap on start
(add-hook 'window-setup-hook #'minimap-mode)
