;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-


;;personal info
(setq user-full-name "Shaurya Singh"
      user-mail-address "shaunsingh@gmail.com")

;;fonts
(setq doom-font (font-spec :family "FiraCode Nerd Font" :size 12)
      doom-variable-pitch-font (font-spec :family "Verdana" :size 12)
      ivy-posframe-font (font-spec :family "FiraCode Nerd Font" :size 13))

;;set theme
;;(setq doom-theme 'doom-vibrant)
(setq doom-theme 'doom-moonlight)
;;(setq doom-theme 'doom-nord)

(require 'ivy-posframe)
;; display at `ivy-posframe-style'
(setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center)))
(ivy-posframe-mode 1)

;;minimap on startup
;;(add-hook 'window-setup-hook #'minimap-mode)

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

;;tell company to always do autocomplete + tabnine
(after! company
  (setq company-idle-delay 0.5
        company-dabbrev-downcase 0
        company-minimum-prefix-length 1)
  (setq company-show-numbers t)
  (add-hook 'evil-normal-state-entry-hook #'company-abort)) ;; make aborting less annoying.
(set-company-backend!
  '(text-mode
    markdown-mode
    gfm-mode)
  '(:seperate
    company-ispell
    company-files
    company-yasnippet))

(setq-default history-length 1000)
(setq-default prescient-history-length 1000)

;;whichkey faster
(setq which-key-idle-delay 0.5)

;;make zen mode smaller
;;(setq +zen-text-scale 0.8)

;;treemacs smaller
(add-hook 'treemacs-mode-hook (lambda () (text-scale-decrease 1)))
(after! treemacs
  (setq treemacs-width 30))

;;ligatures in org mode
(setq +ligatures-in-modes '(org-mode))
(setq +ligatures-extras-in-modes '(org-mode))

(global-subword-mode 1)
