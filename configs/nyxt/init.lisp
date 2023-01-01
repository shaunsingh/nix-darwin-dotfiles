(in-package #:nyxt-user)

;;; Reset ASDF registries to allow loading Lisp systems from
;;; everywhere.
#+nyxt-3 (reset-asdf-registries)

;;; Load quicklisp. Not sure it works.
#-quicklisp
(let ((quicklisp-init
       (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(defvar *web-buffer-modes*
  '(nyxt/emacs-mode:emacs-mode
    #+nyxt-3-pre-release-1 nyxt/auto-mode:auto-mode
    nyxt/blocker-mode:blocker-mode nyxt/force-https-mode:force-https-mode
    nyxt/reduce-tracking-mode:reduce-tracking-mode
    #+nyxt-3 nyxt/user-script-mode:user-script-mode
    #+nyxt-3 nyxt/bookmarklets-mode:bookmarklets-mode)
  "The modes to enable in web-buffer by default.
Extension files (like dark-reader.lisp) are to append to this list.
Why the variable? Because it's too much hassle copying it everywhere.")

;;; Loading files from the same directory.
;;; Can be done individually per file, dolist is there to simplify it.
#+nyxt-3
(define-nyxt-user-system-and-load nyxt-user/basic-config
  :components ("commands" "style" "unpdf"))

;;; Loading extensions and third-party-dependent configs. See the
;;; matching files for where to find those extensions.
;;;
;;; Usually, though, it boils down to cloning a git repository into
;;; your `*extensions-path*' (usually ~/.local/share/nyxt/extensions)
;;; and adding a `load-after-system' (Nyxt 2) /
;;; `define-nyxt-user-system-and-load' (Nyxt 3) line mentioning a
;;; config file for this extension.
(defmacro load-after-system* (system file)
  #+nyxt-3
  `(define-nyxt-user-system-and-load ,(gensym "NYXT-USER/")
     :depends-on (,system) :components (,file)))

(load-after-system* :nx-search-engines "search-engines")
(load-after-system* :nx-kaomoji "kaomoji")
(load-after-system* :nx-ace "ace.lisp")
(load-after-system* :nx-freestance-handler "freestance")
#+nyxt-3 (load-after-system* :nx-dark-reader "dark-reader")

(flet ((construct-autofill (&rest args)
         (apply #+nyxt-3 #'nyxt/autofill-mode:make-autofill
                args)))
  (defvar *autofills*
    (list (construct-autofill :name "Crunch" :fill "test")
          *debug-autofill*)))

;;; Autofils are abstracted into a mode of their own on 3.*.
#+nyxt-3
(define-configuration nyxt/autofill-mode:autofill-mode
  ((nyxt/autofill-mode:autofills *autofills*)))

;;; Those are settings that every type of buffer should share.
(define-configuration (web-buffer prompt-buffer nyxt/editor-mode:editor-buffer)
  ;; Vim keybindings
  ((default-modes (append '(vi-normal-mode) %slot-default%))))

(define-configuration web-buffer
  (#+nyxt-3
   (search-always-auto-complete-p nil)))

(define-configuration prompt-buffer
  ;; This is to hide the header is there's only one source.
  ;; There also used to be other settings to make prompt-buffer a bit
  ;; more minimalist, but those are internal APIs :(
  ((hide-single-source-header-p t)))

;; Basic modes setup for web-buffer.
(define-configuration web-buffer
  ((default-modes `(,@*web-buffer-modes*
                    #+nyxt-3 ,@%slot-value%))))

;;; Set new buffer URL (a.k.a. start page, new tab page).
;;; It does not change the first buffer opened if you're on 2.*.
#+nyxt-3
(define-configuration browser
  ((default-new-buffer-url (quri:uri "https://github.com"))))

;;; Enable proxy in nosave (private, incognito) buffers.
(define-configuration nosave-buffer
  ((default-modes `(nyxt/proxy-mode:proxy-mode
                    ,@*web-buffer-modes*
                    #+nyxt-3 ,@%slot-value%))))

;;; Set up QWERTY home row as the hint keys.
#+nyxt-3
(define-configuration nyxt/hint-mode:hint-mode
  ((nyxt/hint-mode:hints-alphabet "DSJKHLFAGNMXCWEIO")
   ;; Same as default except it doesn't hint images
   (nyxt/hint-mode:hints-selector "a, button, input, textarea, details, select")))

;;; This makes auto-rules to prompt me about remembering this or that
;;; mode when I toggle it.
#+nyxt-3-pre-release-1
(define-configuration nyxt/auto-mode:auto-mode
  ((nyxt/auto-mode:prompt-on-mode-toggle t)))
#+(and nyxt-3 (not nyxt-3-pre-release-1))
(define-configuration modable-buffer
  ((prompt-on-mode-toggle-p t)))

;;; Setting WebKit-specific settings. Not exactly the best way to
;;; configure Nyxt. See
;;; https://webkitgtk.org/reference/webkit2gtk/stable/WebKitSettings.html
;;; for the full list of settings you can tweak this way.
(defmethod ffi-buffer-make :after ((buffer buffer))
  (let* ((settings (webkit:webkit-web-view-get-settings
                    ;; It's not exactly 3.*, it's rather
                    ;; 3-pre-release-3+, but I'm too lazy to conjure
                    ;; this complexity right now.
                    #+nyxt-3 (nyxt/renderer/gtk::gtk-object buffer))))
    (setf
     ;; Resizeable textareas. It's not perfect, but still a cool feature to have.
     (webkit:webkit-settings-enable-resizable-text-areas settings) t
     ;; Write console errors/warnings to the shell, to ease debugging.
     (webkit:webkit-settings-enable-write-console-messages-to-stdout settings) t
     ;; "Inspect element" context menu option available at any moment.
     (webkit:webkit-settings-enable-developer-extras settings) t
     ;; Use SF Pro 16 as the default font.
     (webkit:webkit-settings-default-font-family settings) "SF Pro"
     (webkit:webkit-settings-default-font-size settings) 16
     ;; Use SF Mono 14 as the monospace font.
     (webkit:webkit-settings-monospace-font-family settings) "Liga SFMono Nerd Font"
     (webkit:webkit-settings-default-monospace-font-size settings) 14)))

;; This is to strip UTM-parameters of all the links. Upstream Nyxt
;; doesn't have it because it may break some websites.
#+nyxt-3
(define-configuration nyxt/reduce-tracking-mode:reduce-tracking-mode
  ((nyxt/reduce-tracking-mode:query-tracking-parameters
    (append '("utm_source" "utm_medium" "utm_campaign" "utm_term" "utm_content")
            %slot-value%))))

;; Enable Nyxt-internal debugging, but only in binary mode and after
;; startup if done (there are conditions raised at startup, and I
;; don't want to catch those, hanging my Nyxt).
(unless nyxt::*run-from-repl-p*
  (hooks:add-hook *after-startup-hook* #'toggle-debug-on-error))
