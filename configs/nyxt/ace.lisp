(in-package #:nyxt-user)

;; this file was written and edited with ace inside nyxt :)

(define-mode ace-mode (nyxt/editor-mode:editor-mode nyxt/passthrough-mode:passthrough-mode)
  "Mode for usage with the Ace editor."
  ((style
    (theme:themed-css (theme *browser*)
      ("#editor"
       :position "absolute"
       :top "0"
       :right "0"
       :bottom "0"
       :left "0")))
   (extensions
    nil
    :type list)
   (theme
    nil
    :type (maybe string))
   (keybindings
    nil
    :type (maybe string))
   (epilogue
    nil
    :type (maybe string))))

(defmethod nyxt/editor-mode::markup ((ace ace-mode))
  (spinneret:with-html-string
    (:head
     (:style (style ace)))
    (:body
     (:script
      :src "https://cdnjs.cloudflare.com/ajax/libs/ace/1.15.2/ace.min.js"
      :crossorigin "anonymous"
      :type "text/javascript"
      :charset "utf-8"
      "")
     (dolist (ext (extensions ace))
       (:script
        :src (quri:render-uri (quri:uri ext))
        :crossorigin "anonymous"
        :type "text/javascript"
        :charset "utf-8"
        ""))
     (:div :id "editor" "")
     (:script
      (:raw
       (ps:ps
         (defparameter editor (ps:chain ace (edit "editor")))
         (when (ps:lisp (theme ace))
           (ps:chain editor (set-theme (ps:lisp (theme ace)))))
         (ps:chain editor (set-keyboard-handler
                           (ps:@ (require (ps:lisp (keybindings ace))) handler))))))
     (:script
      (:raw (epilogue ace))))))

(defmethod nyxt/editor-mode::set-content ((ace ace-mode) content)
  (ps-eval :buffer (buffer ace)
    (ps:chain editor session (set-value (ps:lisp content)))))

(defmethod nyxt/editor-mode::get-content ((ace ace-mode))
  (ps-eval :buffer (buffer ace) (ps:chain editor (get-value))))

(defmethod set-option ((ace ace-mode) option value)
  (ps-eval :buffer (buffer ace)
    (ps:chain editor (set-option (ps:lisp option) (ps:lisp value)))))

(defun options ()
  (alexandria:hash-table-keys (ps-eval (ps:chain editor (get-options)))))

(define-configuration ace-mode
  ((extensions
     (mapcar
       (lambda (name)
         (quri:merge-uris (quri:uri name)
        (quri:uri "https://cdnjs.cloudflare.com/ajax/libs/ace/1.15.2/")))
       '(;; vim keybinding support
         "keybinding-vim.min.js"
         ;; themes
         "theme-xcode.min.js"
         ;; language modes
         "mode-java.min.js"
         "mode-lisp.min.js"
         ;; language snippets
         "snippets/java.min.js"
         ;; language workers
         "worker-base.min.js"
         ;; extensions
         "ext-beautify.min.js"
         "ext-code_lens.min.js"
         "ext-keybinding_menu.min.js"
         "ext-language_tools.min.js"
         "ext-modelist.min.js"
         "ext-searchbox.min.js"
         "ext-settings_menu.min.js"
         "ext-split.min.js"
         "ext-whitespace.min.js")))
    (epilogue
      (str:concat
       (ps:ps
         (flet (;; utility function to easily require under ace
                (req (ext)
                  (ps:chain ace (require ext)))
                ;; emacs/insert binding wrapper
                (bind (key command)
                  (ps:chain editor commands (bind-key key command)))
                ;; vim normal mode binding wrapper
                (nmap (key command)
                  (ps:chain (req "ace/keyboard/vim") -vim (map key command "normal")))
                ;; vim insert mode binding wrapper
                (imap (key command)
                  (ps:chain (req "ace/keyboard/vim") -vim (map key command "normal"))))
           ;; load extensions
           (req "ace/ext/searchbox")
           (req "ace/ext/split")
           (req "ace/ext/language_tools")
           (req "ace/ext/code_lens")
           (req "ace/ext/whitespace")
           (ps:chain (req "ace/ext/settings_menu") (init editor))
           (ps:chain (req "ace/ext/keybinding_menu") (init editor))
           (ps:chain editor session
                     (set-mode (ps:chain (req "ace/ext/modelist")
                                         (get-mode-for-path (ps:@ window location href)) mode)))
           (ps:chain editor commands
                     (add-command (ps:chain ace (require "ace/ext/beautify") commands 0)))
           ;; editor config
           (ps:chain editor (set-option "cursorStyle" "wide"))          ;; static cursor
           (ps:chain editor (set-option "readOnly" nil))                ;; set read and write file
           (ps:chain editor (set-option "fontSize" 15))                 ;; bigger default font
           (ps:chain editor (set-option "showLineNumbers" nil))         ;; disable line numbers
           (ps:chain editor (set-option "showPrintMargin" t))           ;; enable print margin (colorline)
           (ps:chain editor (set-option "displayIndentGuides" nil))     ;; disable indent markers
           (ps:chain editor (set-option "hScrollBarAlwaysVisible" nil)) ;; don't always show scrollbar (h)
           (ps:chain editor (set-option "vScrollBarAlwaysVisible" nil)) ;; don't always show scrollbar (v)
           (ps:chain editor (set-option "useSoftTabs" t))               ;; use spaces instead of tabs
           (ps:chain editor (set-option "enableSnippets" t))            ;; enable snippet support
           (ps:chain editor (set-option "highlightActiveLine" t))       ;; highlight current line
           (ps:chain editor (set-option "enableBasicAutocompletion" t)) ;; enable (basic) autocompleetion
           ;; vim bindings
           (nmap "j" "gj")
           (nmap "k" "gk")
           ;; emacs bindings
           (bind "Ctrl-Alt-?" (lambda (editor)
                                 (ps:chain editor (show-keyboard-shortcuts))))
           ;; add support for saving with :w
           (ps:chain (req "ace/keyboard/vim") -vim 
                     (define-ex "write" "w" (lambda (cm input)
                                              (ps:chain cm ace (exec-command "save")))))
           ;; load workers
           (req "ace/worker/base")))))))

(define-configuration ace-mode
  ((style (str:concat
           %slot-value%
           (theme:themed-css (theme *browser*)
             ("#kbshortcutmenu"
              :background-color theme:background
              :color theme:on-background))))
   (:theme "ace/theme/xcode")
   (:keybindings "ace/keyboard/vim")))

;; use ace for editor-mode by default
(define-configuration nyxt/editor-mode::editor-buffer
  ((default-modes `(ace-mode ,@%slot-value%))))
