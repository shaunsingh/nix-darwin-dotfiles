(in-package #:nyxt-user)

;;; allow loading lisps systems from anywhere
(reset-asdf-registries)

;; load quicklisp if available
#-quicklisp
(let ((quicklisp-init
       (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(define-configuration browser
  ;; enable --remote --eval code eval
  ((remote-execution-p t)
   ;; set nvim as external editor
   (external-editor-program
    (list "foot" "nvim"))))

;; don't restore history on startup
;; (define-configuration browser 
;;   ((restore-session-on-startup-p nil)))

;; style config files to load
(define-nyxt-user-system-and-load nyxt-user/style-config
  :components ("base16"                             ;; nixOS generated base16 themes
               "style"                              ;; general themeing
               "status"))                           ;; status-buffer themeing

;; modes to enable in web-buffer
(defvar *web-buffer-modes*
  '(nyxt/vi-mode:vi-normal-mode                     ;; vi bindings
    nyxt/style-mode:dark-mode;                      ;; dark mode
    nyxt/blocker-mode:blocker-mode                  ;; basic adblock
    ;; nyxt/force-https-mode:force-https-mode          ;; force https
    nyxt/user-script-mode:user-script-mode          ;; user scripts
    nyxt/reduce-tracking-mode:reduce-tracking-mode  ;; reduce tracking
    nyxt/bookmarklets-mode:bookmarklets-mode)       ;; javascript bookmarks
 "The modes to enable in web-buffer by default")

;; additional config files to load
(define-nyxt-user-system-and-load nyxt-user/extra-config
  :components ("startpage"                          ;; personal startpage
               "commands"                           ;; user commands
               "split"                              ;; support for "splits"
               "tor"                                ;; .onion management
               "ace"                                ;; support for ace text editor
               "mpv"                                ;; viewing videos/audio in mpv
               "fetch"))                            ;; system fetch

;; additional extensions to load
(defmacro load-after-system* (system &optional file)
  "Helper macro to load configuration for extensions.
Loads a newly-generated ASDF system depending on SYSTEM.
FILE, if provided, is loaded after the generated system successfully
loads."
  `(define-nyxt-user-system-and-load ,(gensym "NYXT-USER/")
     :depends-on (,system) ,@(when file
                                `(:components (,file)))))

;; (ql:quickload "nx-notmuch")
;; (load-after-system* :nx-notmuch)
;; (load-after-system* :nx-kaomoji "kaomoji")
(load-after-system* :nx-dark-reader "dark-reader")
(load-after-system* :nx-search-engines "search-engines")

;; additional files to unconventionally load
(defmethod files:resolve ((profile nyxt:nyxt-profile) (file nyxt/bookmark-mode:bookmarks-file))
  #p"~/nix-darwin-dotfiles/configs/nyxt/local/bookmarks.lisp")
(defmethod files:resolve ((profile nyxt:nyxt-profile) (file nyxt/no-procrastinate-mode:no-procrastinate-hosts-file))
  #p"~/nix-darwin-dotfiles/configs/nyxt/local/no-procrastinate-hosts")
(defmethod files:resolve ((profile nyxt:nyxt-profile) (file auto-rules-file))
  #p"~/nix-darwin-dotfiles/configs/nyxt/local/auto-rules.lisp")

;; simple web-buffer customization
(define-configuration web-buffer
  (;; dont autocomplete when unessecary
   (search-always-auto-complete-p nil)
   ;; basic mode setup for web-buffer
   (default-modes `(,@*web-buffer-modes*
        ,@%slot-value%))))

;; we wan't to be in insert mode in the prompt buffer
(define-configuration (prompt-buffer)
  ((default-modes `(nyxt/vi-mode:vi-insert-mode
         ,@%slot-value%))
  ;; hide the header if there's only one source.
  (hide-single-source-header-p t)))

;; enable proxy in nosave (private, incognito) buffers.
(define-configuration nosave-buffer
  ((default-modes `(nyxt/proxy-mode:proxy-mode
                    ,@*web-buffer-modes*
                    ,@%slot-value%))))

;; use qwerty keys for hint mode
(define-configuration nyxt/hint-mode:hint-mode
  ((nyxt/hint-mode:hints-alphabet "DSJKHLFAGNMXCWEIO")
   ;; same as default except it doesn't hint images
   (nyxt/hint-mode:hints-selector "a, button, input, textarea, details, select")))

;; strip UTM info from websites in tracking mode
(define-configuration nyxt/reduce-tracking-mode:reduce-tracking-mode
  ((nyxt/reduce-tracking-mode:query-tracking-parameters
    (append '("utm_source" "utm_medium" "utm_campaign" "utm_term" "utm_content")
            %slot-value%))))

;; webkit config 
;; this was mostly guessing, thouogh this link provides basic docs for the options
;; https://webkitgtk.org/reference/webkit2gtk/stable/class.Settings.html#properties
(defmethod ffi-buffer-make :after ((buffer buffer))
  (let* ((settings (webkit:webkit-web-view-get-settings
                    (nyxt/renderer/gtk::gtk-object buffer))))
    (setf
     ;; enable encrypted (DRM) content if possible
     (webkit:webkit-settings-enable-encrypted-media settings) t
     ;; enable resizable text areas
     (webkit:webkit-settings-enable-resizable-text-areas settings) t
     ;; enable inspect
     (webkit:webkit-settings-enable-developer-extras settings) t
     ;; enable webgl support
     (webkit:webkit-settings-enable-webgl settings) t
     ;; use SF Pro Text as proportional font
     (webkit:webkit-settings-default-font-family settings) "SF Pro Text"
     (webkit:webkit-settings-default-font-size settings) 15
     ;; use Liga SFMono Nerd Font as monospace font
     (webkit:webkit-settings-monospace-font-family settings) "Liga SFMono Nerd Font"
     (webkit:webkit-settings-default-monospace-font-size settings) 13
     ;; use Unifont for icons
     (webkit:webkit-settings-pictograph-font-family settings) "Unifont")))
