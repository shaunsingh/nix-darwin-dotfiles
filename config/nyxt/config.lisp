(in-package #:nyxt-user)

;;; this file was created and edited in NYXT with ace-mode

(defvar *buffer-modes*
  '(vi-normal-mode)
 "Modes to enable in buffer by default")

;; don't hint images
(define-configuration nyxt/mode/hint:hint-mode
  ((nyxt/mode/hint:hints-alphabet "DSJKHLFAGNMXCWEIO")
   (nyxt/mode/hint:hints-selector "a, button, input, textarea, details, select")))

;; add custom user agent and block utm
(define-configuration nyxt/mode/reduce-tracking:reduce-tracking-mode
  ((nyxt/mode/reduce-tracking:query-tracking-parameters
    (append '("utm_source" "utm_medium" "utm_campaign" "utm_term" "utm_content")
            %slot-value%))
    (preferred-user-agent
     "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/112.0.0.0 Safari/537.36")))

(defmethod files:resolve ((profile nyxt:nyxt-profile) (file nyxt/mode/bookmark:bookmarks-file))
  "Reroute the bookmarks to the config directory."
  #p"~/.config/nyxt/bookmarks.lisp")

;; style config
(define-nyxt-user-system-and-load nyxt-user/style-config
  :components ("style"
               "status"))

;; extensions
(define-nyxt-user-system-and-load nyxt-user/extra-config
  :components ("startpage"
	       "commands"
	       "hardware"
	       "launcher"
	       "wayland"
	       "zola"
               "tor"
	       "term"
               "mpv"
               "ace"
               "repl"
               "ai"
               "search-engines"
               "fetch"))

;; simple web-buffer customization
(define-configuration buffer
  (;; basic mode setup for web-buffer
   (default-modes `(,@*buffer-modes*
        ,@%slot-value%))))

;; we wan't to be in insert mode in the prompt buffer, don't show source if theres only one
(define-configuration (prompt-buffer)
  ((default-modes `(vi-insert-mode
         ,@%slot-value%))
   (hide-single-source-header-p t)))

(defmethod ffi-buffer-make :after ((buffer nyxt/renderer/gtk::gtk-buffer))
  "Setting WebKit-specific settings. See https://webkitgtk.org/reference/webkit2gtk/stable/WebKitSettings.html
for the full list of settings you can tweak this way."
  (when (slot-boundp buffer 'nyxt/renderer/gtk::gtk-object)
    (let* ((settings (webkit:webkit-web-view-get-settings
                      (nyxt/renderer/gtk::gtk-object buffer))))
      (setf
       ;; drm
       (webkit:webkit-settings-enable-encrypted-media settings) t
       ;; misc
       (webkit:webkit-settings-enable-resizable-text-areas settings) t
       (webkit:webkit-settings-enable-write-console-messages-to-stdout settings) t
       (webkit:webkit-settings-enable-developer-extras settings) t
       ;; fonts
       (webkit:webkit-settings-default-font-family settings) "SF Pro Text"
       (webkit:webkit-settings-default-font-size settings) 15
       (webkit:webkit-settings-monospace-font-family settings) "Liga SFMono Nerd Font"
       (webkit:webkit-settings-default-monospace-font-size settings) 13)))
  (cffi:foreign-funcall
   "webkit_web_view_set_background_color"
   :pointer (g:pointer (nyxt/renderer/gtk:gtk-object buffer))
   :pointer (cffi:foreign-alloc
             :double
             :count 4
	     ;;#161616 fp doule literal
             :initial-contents '(0.08627450980392157d0 0.08627450980392157d0 0.08627450980392157d0 1.0d0))))
