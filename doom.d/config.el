;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;personal info
(setq user-full-name "Shaurya Singh"
      user-mail-address "shaunsingh0207@gmail.com")

;;fonts
(setq doom-font (font-spec :family "SF Mono" :size 12)
      doom-variable-pitch-font (font-spec :family "SF Pro Display" :size 12.5))

;;set theme
(setq doom-theme 'doom-nord)
;;(setq doom-theme 'doom-moonlight)

;;ivy vs-code style
(require 'ivy-posframe)
(setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center)))

;;make treemacs thinner
(after! treemacs
  (setq treemacs-width 25))  ; default is 35

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
    company-ispell
    company-files
    company-yasnippet))

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

;; calc configuration
(setq calc-angle-mode 'deg  ; angles are degrees
      calc-symbolic-mode t) ; keeps expressions like \sqrt{2} irrational for as long as possible
;; calctex configuration (stolen from https://tecosaur.github.io/emacs-config/config.html#calc-calctex)
(use-package! calctex
  :commands calctex-mode
  :init
  (add-hook 'calc-mode-hook #'calctex-mode)
  :config
  (setq ;; calctex-additional-latex-packages "
;; \\usepackage[usenames]{xcolor}
;; \\usepackage{soul}
;; \\usepackage{adjustbox}
;; \\usepackage{amsmath}
;; \\usepackage{amssymb}
;; \\usepackage{siunitx}
;; \\usepackage{cancel}
;; \\usepackage{mathtools}
;; \\usepackage{mathalpha}
;; \\usepackage{xparse}
;; \\usepack
;; age{arevmath}"
        calctex-additional-latex-macros
        (concat calctex-additional-latex-macros
                "\n\\let\\evalto\\Rightarrow"))
  (defadvice! no-messaging-a (orig-fn &rest args)
    :around #'calctex-default-dispatching-render-process
    (let ((inhibit-message t) message-log-max)
      (apply orig-fn args)))
  ;; Fix hardcoded dvichop path (whyyyyyyy)
  (let ((vendor-folder (concat (file-truename doom-local-dir)
                               "straight/"
                               (format "build-%s" emacs-version)
                               "/calctex/vendor/")))
    (setq calctex-dvichop-sty (concat vendor-folder "texd/dvichop")
          calctex-dvichop-bin (concat vendor-folder "texd/dvichop")))
  (unless (file-exists-p calctex-dvichop-bin)
    (message "CalcTeX: Building dvichop binary")
    (let ((default-directory (file-name-directory calctex-dvichop-bin)))
      (call-process "make" nil nil nil))))

;;use pdf-tools (not installed rn)
;;(setq +latex-viewers '(pdf-tools))

;;stupid warnings
(setq load-prefer-newer t)

;; transparency for fun
(set-frame-parameter (selected-frame) 'alpha '(85 85))
(add-to-list 'default-frame-alist '(alpha 85 85))

;;minimap on start
(add-hook 'window-setup-hook #'minimap-mode)
