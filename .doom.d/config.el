;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-


;;personal info
(setq user-full-name "Shaurya Singh"
      user-mail-address "shaunsingh@gmail.com")

;;fonts
(setq doom-font (font-spec :family "FiraCode Nerd Font" :size 12)
      doom-variable-pitch-font (font-spec :family "Verdana" :size 13)
      ivy-posframe-font (font-spec :family "FiraCode Nerd Font" :size 15))

;;set theme
(setq doom-theme 'doom-moonlight)

;;org direcotry
(setq org-directory "~/org/")

;;line numbers, trash, undo limits
(setq display-line-numbers-type t)
(setq-default
 delete-by-moving-to-trash t                      ; Delete files to trash
 window-combination-resize t                      ; take new window space from all other windows (not just current)
 x-stretch-cursor t)                              ; Stretch cursor to the glyph width

(setq undo-limit 80000000                         ; Raise undo-limit to 80Mb
      evil-want-fine-undo t                       ; By default while in insert all changes are one big blob. Be more granular
      auto-save-default t                         ; Nobody likes to loose work, I certainly don't
      truncate-string-ellipsis "…")               ; Unicode ellispis are nicer than "...", and also save /precious/ space

(display-time-mode 1)                             ; Enable time in the mode-line
(display-battery-mode 1)

;;set split behavior
(setq evil-vsplit-window-right t
      evil-split-window-below t)

;;default mode org mode
(setq-default major-mode 'org-mode)

;;cute start screen
(setq fancy-splash-image "/Users/shauryasingh/.doom.d/cute-demon.png")
(setq +doom-dashboard-banner-padding '(0 . 3))

;;use tabnine
(use-package company-tabnine :ensure t)

;;tell company to always do autocomplete + tabnine
(after! company
  (setq company-idle-delay 1
        company-minimum-prefix-length 2)
  (setq company-show-numbers t)
  (add-hook 'evil-normal-state-entry-hook #'company-abort)) ;; make aborting less annoying.
(set-company-backend!
  '(text-mode
    markdown-mode
    gfm-mode)
  '(:seperate
    company-ispell
    company-files
    company-yasnippet
    company-tabnine))

;;whichkey faster
(setq which-key-idle-delay 0.5)

;;make zen mode smaller
(setq +zen-text-scale 0.8)

;; keybindings
(map! :leader
      :desc "Mixed-pitch mode"
      "t m" #'mixed-pitch-mode)

(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;;ligatures in org mode
(setq +ligatures-in-modes '(org-mode))
(setq +ligatures-extras-in-modes '(org-mode))

(global-subword-mode 1)
