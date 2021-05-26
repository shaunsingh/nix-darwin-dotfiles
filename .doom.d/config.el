;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;personal info
(setq user-full-name "Shaurya Singh"
      user-mail-address "shaunsingh0207@gmail.com")

;;fonts
(setq doom-font (font-spec :family "SF Mono" :size 13)
      doom-big-font (font-spec :family "SF Pro Display" :size 20)
      doom-variable-pitch-font (font-spec :family "Roboto Mono" :size 15)
      doom-unicode-font (font-spec :family "SF Mono")
      doom-serif-font (font-spec :family "Overpass" :weight 'light))

;;set theme
(setq doom-theme 'doom-flatwhite)
;;(setq doom-theme 'doom-moonlight)
;;(setq doom-theme 'doom-one-light)

;;disable cursorline
(remove-hook 'doom-first-buffer-hook #'global-hl-line-mode)

;;ivy vs-code style
(require 'ivy-posframe)
(setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center)))

;;make treemacs thinner
(setq treemacs-width 25)

;;let yabai handle frames instead of splits
(setq pop-up-frames t)

;;org directory
(setq org-directory "~/.org"                      ; let's put files here
      org-use-property-inheritance t              ; it's convenient to have properties inherited
      org-log-done 'time                          ; having the time a item is done sounds convenient
      org-list-allow-alphabetical t               ; have a. A. a) A) list bullets
      org-export-in-background t                  ; run export processes in external emacs process
      org-catch-invisible-edits 'smart            ; try not to accidently do weird stuff in invisible regions
      org-export-with-sub-superscripts '{})       ; don't treat lone _ / ^ as sub/superscripts, require _{} / ^{}

(setq undo-limit 80000000                          ;I mess up too much
      evil-want-fine-undo t)                       ;By default while in insert all changes are one big blob. Be more granular
(setq org-startup-with-inline-images t)            ;inline images in org mode
(setq +ligatures-in-modes '(org-mode))
(setq +ligatures-extras-in-modes '(org-mode))      ;ligatures in org mode
(add-hook! 'org-mode-hook #'+org-pretty-mode #'mixed-pitch-mode) ;enter mixed pitch mode in org mode

;; line numbers
(setq display-line-numbers-type t)

;;modeline (icons, config, battery)
(display-time-mode 1)                              ;Enable time in the mode-line
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
;;(setq +zen-mixed-pitch-modes nil)
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
(setq +latex-viewers '(pdf-tools))

;;start with latex preview
(after! org (setq org-startup-with-latex-preview t)
  (plist-put org-format-latex-options :scale 1)) ;make latex size the same as others

;;auto toggle between preview/raw latex
(use-package! org-fragtog
  :hook (org-mode . org-fragtog-mode))

;;stupid warnings (somehow fixed itself)
;;(setq load-prefer-newer t)

;; transparency for fun
(set-frame-parameter (selected-frame) 'alpha '(92 92))
(add-to-list 'default-frame-alist '(alpha 92 92))

;;disable line dividers
(custom-set-faces!
  `(vertical-border :background ,(doom-color 'bg) :foreground ,(doom-color 'bg)))

;;make minimap transparent
(setq minimap-highlight-line nil)
(custom-set-faces!
  `(minimap-active-region-background :background unspecified))

;;minimap on startup
(add-hook 'window-setup-hook #'minimap-mode)

;;general settings
(setq truncate-string-ellipsis "…")        ;default ellipses suck
(setq-default x-stretch-cursor t)          ;make the cursor the size of the char under it (tabs)
(setq-default delete-by-moving-to-trash t) ;delete to system trash instead
