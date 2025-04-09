(in-package #:nyxt-user)

;;; ACE

;; this file was written and edited with ace inside nyxt :)
;; reimplements `editor-mode` removed in nyxt4-prerelease4
;; defines `ace-mode`, handling files in `editor-mode`
;; modified from https://github.com/atlas-engineer/nx-ace
;; with the following changes 

;; - updated ace version
;; - support for `ace-linter` and language-servers
;; - custom oxocarbon theme
;; - vim/ex utility commands
;; - proper handling of saves splits etc. 

(define-mode my-editor-mode ()
  "General-purpose editor mode, meant to be subclassed")

(defgeneric get-content (editor-submode)
  (:method ((editor my-editor-mode))
    (declare (ignore editor))
    (echo-warning "Editor buffer cannot edit files without configured editor mode."))
  (:documentation "Get the content of the EDITOR-SUBMODE as a string."))

(defgeneric set-content (editor-submode content)
  (:method ((editor my-editor-mode) (content t))
    (declare (ignore editor))
    (echo-warning "Editor buffer cannot edit files without configured editor mode. 
See `describe-class my-editor-mode' for details."))
  (:documentation "Set the content of EDITOR-SUBMODE to the string CONTENT."))

(defgeneric markup (editor-submode)
  (:method ((editor my-editor-mode))
    (spinneret:with-html-string
      (:head
       (:nstyle (style (buffer editor))))
      (:body
       (:p "Please configure an editor mode to use an editor buffer. See "
           (:code "describe-class") " for " (:code "my-editor-buffer")
           " to see the list of functions to implement."))))
  (:documentation "Return an HTML string representation of the file to be edited."))

(define-class my-editor-buffer (network-buffer ;; questionable, but needed for `buffer-load'.
                             context-buffer modable-buffer document-buffer input-buffer)
  ((nyxt:title "*Editor*"))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:export-predicate-name-p t)
  (:metaclass user-class)
  (:documentation "Buffer to edit files"))

(defmethod nyxt:default-modes :around ((buffer my-editor-buffer))
  (set-difference (call-next-method) '(document-mode base-mode)))

(defmethod file ((buffer my-editor-buffer))
  (uiop:parse-native-namestring (quri:uri-path (url buffer))))

(define-internal-scheme "editor"
    (lambda (url buffer)
      (let ((mode (find-submode 'my-editor-mode buffer))
            (file (quri:uri-path (quri:uri url))))
        (uiop:chdir (uiop:pathname-directory-pathname file))
        (run-thread "editor content setting"
          (sleep 2)
          (set-content mode (uiop:read-file-string file)))
        (markup mode))))

(defmethod editor ((my-editor-buffer my-editor-buffer))
  (let ((mode (find-submode 'my-editor-mode my-editor-buffer)))
    (unless (eq 'my-editor-mode (serapeum:class-name-of mode))
      mode)))

(defmethod write-file-with-editor ((buffer my-editor-buffer) &key (if-exists :error))
  (cond
    ((editor buffer)
     (handler-case
         (alexandria:write-string-into-file (get-content (editor buffer))
                                            (file buffer)
                                            :if-exists if-exists)
       (file-error (e)
         (echo-warning "Cannot write ~a: ~a" (file buffer) e)
         nil)))
    (t
     (echo-warning "Editor buffer cannot write file without configured editor mode.")
     nil)))

(defun prompt-for-file ()
  (uiop:native-namestring
   (pathname
    (prompt1
     :prompt "Open file"
     :extra-modes 'nyxt/mode/file-manager:file-manager-mode
     :input (uiop:native-namestring (uiop:getcwd))
     :sources
     (list (make-instance 'nyxt/mode/file-manager:file-source
                          :name "Existing file"
                          :actions-on-return #'identity)
           (make-instance 'prompter:raw-source
                          :name "Create new file"))))))

(define-command my-editor-open-file (&key (buffer (current-buffer)) (file (prompt-for-file)))
  "Open my file.

BUFFER is of type `my-editor-buffer'."
  (buffer-load (quri:make-uri :scheme "editor" :path file) :buffer buffer))

(define-command my-editor-write-file (&key (buffer (current-buffer)))
  "Write my file to storage.

BUFFER is of type `my-editor-buffer'."
  (if (uiop:file-exists-p (file buffer))
      (if-confirm ((format nil "Overwrite ~s?" (file buffer))
                   :yes "overwrite" :no "cancel")
          (echo "File ~s ~:[not ~;~]saved."
                (file buffer) (write-file-with-editor buffer :if-exists :overwrite)))
      (echo "File ~s ~:[not ~;~]saved." (file buffer) (write-file-with-editor buffer))))

(define-command-global edit-file (&optional (file (prompt-for-file)))
  "Open a new editor and query my FILE to edit in it."
  (set-current-buffer (make-instance 'my-editor-buffer
                                     :url (quri:make-uri :scheme "editor" :path file))))

(defun prompt-for-editor-user-file ()
  (uiop:native-namestring
   (files:expand
    (prompt1 :prompt "Edit user file"
             :sources 'nyxt::user-file-source))))

(define-command-global edit-user-file (&optional (file (prompt-for-editor-user-file)))
  (set-current-buffer (make-instance 'my-editor-buffer
                                     :url (quri:make-uri :scheme "editor" :path file))))

(define-auto-rule '(match-scheme "editor")
  :included '(my-editor-mode))

;; extend our new mode. heavily inspired by nx-ace
(define-mode ace-mode (my-editor-mode) ;; nyxt/mode/passthrough:passthrough-mode) ; TODO buggy 
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
   (keybindings
    nil
    :type (maybe string))
   (epilogue
    nil
    :type (maybe string))))

(defmethod markup ((ace ace-mode))
  (spinneret:with-html-string
    (:head
     (:style (style ace)))
    (:body
     (:script
      :src "https://cdnjs.cloudflare.com/ajax/libs/ace/1.39.1/ace.min.js"
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
         (ps:chain editor (set-keyboard-handler
                           (ps:@ (require (ps:lisp (keybindings ace))) handler))))))
     (:script
      (:raw (epilogue ace))))))

(defmethod set-content ((ace ace-mode) content)
  (ps-eval :buffer (buffer ace)
    (ps:chain editor session (set-value (ps:lisp content)))))

(defmethod get-content ((ace ace-mode))
  (ps-eval :buffer (buffer ace) (ps:chain editor (get-value))))

(defmethod set-option ((ace ace-mode) option value)
  (ps-eval :buffer (buffer ace)
    (ps:chain editor (set-option (ps:lisp option) (ps:lisp value)))))

(defun options ()
  (alexandria:hash-table-keys (ps-eval (ps:chain editor (get-options)))))

(define-configuration ace-mode
  ((keyscheme-map
    (define-keyscheme-map "my-editor-mode" ()
      nyxt/keyscheme:default
      (list
       "C-r" 'reload-current-buffer
       "f11" 'toggle-fullscreen)
      nyxt/keyscheme:cua
      (list
       "C-o" 'my-editor-open-file
       "C-s" 'my-editor-write-file
       "C-w" 'delete-current-buffer
       "C-tab" 'switch-buffer)
      nyxt/keyscheme:emacs
      (list
       "C-x C-f" 'my-editor-open-file
       "C-x C-s" 'my-editor-write-file
       "C-x C-k" 'delete-current-buffer
       "C-x b" 'switch-buffer)
      nyxt/keyscheme:vi-normal
      (list
       "C-o" 'my-editor-open-file
       "C-s" 'my-editor-write-file
       "C-w" 'delete-current-buffer
       "C-tab" 'switch-buffer
       "C-space" 'execute-command))) ;; due to passthrough not working
   (style (str:concat
           %slot-value%
           (theme:themed-css (theme *browser*)
              `(".oxocarbon"
               :color ,*base05-*
               :background-color ,*base00-*)
              `(".oxocarbon .ace_gutter"
               :color ,*base03-*
               :background-color "#131313")
              `(".oxocarbon .ace_print-margin"
               :width "1px"
               :background-color ,*base01-*)
              `(".oxocarbon .ace_cursor"
               :color ,*base06-*)
              `(".oxocarbon .ace_marker-layer .ace_selection"
               :background-color ,*base02-*)
              `(".oxocarbon .ace_marker-layer .ace_step"
               :background-color ,*base0A-*)
              `(".oxocarbon .ace_marker-layer .ace_active-line"
               :background-color ,*base01-*)
              `(".oxocarbon .ace_gutter-active-line"
               :color ,*base06-*)
              `(".oxocarbon .ace_marker-layer .ace_selected-word"
               :background-color ,*base02-*)
              `(".oxocarbon .ace_fold"
               :color ,*base04-*)
              ;; worker extension
              `(".oxocarbon.ace_editor.ace_autocomplete"
               :border "0"
               :background-color ,*base01-*
               :color ,*base04-*)
              `(".oxocarbon.ace_editor.ace_autocomplete .ace_marker-layer .ace_active-line"
               :border "0"
               :background-color ,*base02-*)
              `(".oxocarbon.ace_editor.ace_autocomplete .ace_completion-meta"
               :border "0"
               :color ,*base04-*)
              `(".oxocarbon.ace_editor.ace_autocomplete .ace_line-hover"
               :border "0"
               :background-color ,*base0C-*
               :color ,*base05-*)
              `(".oxocarbon.ace_editor.ace_autocomplete .ace_completion-highlight"
               :color ,*base06-*)
              ;; token styles
              `(".oxocarbon .ace_comment"
               :color ,*base03-*)
              `(".oxocarbon .ace_keyword"
               :color ,*base0C-*)
              `(".oxocarbon .ace_constant.ace_numeric"
               :color ,*base0F-*)
              `(".oxocarbon .ace_constant.ace_character"
               :color ,*base07-*)
              `(".oxocarbon .ace_constant.ace_character.ace_escape"
               :color ,*base08-*)
              `(".oxocarbon .ace_constant.ace_character.ace_language"
               :color ,*base09-*)
              `(".oxocarbon .ace_constant.ace_character.ace_other"
               :color ,*base09-*)
              `(".oxocarbon .ace_support.ace_function"
               :font-weight "bold"
               :color ,*base0C-*)
              `(".oxocarbon .ace_support.ace_constant"
               :color ,*base07-*)
              `(".oxocarbon .ace_support.ace_class"
               :color ,*base08-*)
              `(".oxocarbon .ace_support.ace_type"
               :color ,*base08-*)
              `(".oxocarbon .ace_storage"
               :color ,*base09-*)
              `(".oxocarbon .ace_storage.ace_type"
               :color ,*base08-*)
              `(".oxocarbon .ace_invalid"
               :color ,*base0A-*)
              `(".oxocarbon .ace_invalid.ace_deprecated"
               :color ,*base03-*)
              `(".oxocarbon .ace_string"
               :color ,*base0E-*)
              `(".oxocarbon .ace_variable"
               :color ,*base0F-*)
              `(".oxocarbon .ace_variable.ace_parameter"
               :color ,*base04-*)
              `(".oxocarbon .ace_entity.ace_other.ace_attribute-name"
               :color ,*base0B-*)
              `(".oxocarbon .ace_entity.ace_name.ace_tag"
               :color ,*base0F-*)
              `(".oxocarbon .ace_invisible"
               :color ,*base03-*))))
   (:keybindings "ace/keyboard/vim")
   (extensions
     (mapcar
       (lambda (name)
         (quri:merge-uris (quri:uri name)
        (quri:uri "https://cdnjs.cloudflare.com/ajax/libs/ace/1.39.1/")))
       '(;; vim keybinding support
         "keybinding-vim.min.js"
         ;; language modes
         "mode-java.min.js"
         "mode-lisp.min.js"
         "mode-nix.min.js"
         "mode-c_cpp.min.js"
         "mode-rust.min.js"
         "mode-makefile.min.js"
         "mode-markdown.min.js"
         "mode-sh.min.js"
         "mode-lua.min.js"
         "mode-python.min.js"
         "mode-ocaml.min.js"
         ;; language snippets
         "snippets/java.min.js"
         "snippets/nix.min.js"
         "snippets/c_cpp.min.js"
         "snippets/rust.min.js"
         "snippets/makefile.min.js"
         "snippets/markdown.min.js"
         "snippets/sh.min.js"
         "snippets/lua.min.js"
         "snippets/python.min.js"
         ;; language workers
         "worker-base.min.js"
         ;; extensions
         "ext-language_tools.min.js"   ;; basic autocompletion/snippets
         "ext-whitespace.min.js"       ;; detect spacing/indent
         "ext-settings_menu.min.js"    ;; view and adjust settings
         "ext-keybinding_menu.min.js"  ;; view and adjust keybindings
         "ext-modelist.min.js")))      ;; detect mode based on filepath
    (epilogue
      (str:concat
       (ps:ps
         (flet ((req (ext)
                  (ps:chain ace (require ext)))
                (em-bind (key command)
                  (ps:chain editor commands (bind-key key command)))
                (vi-map (key command mode)
                  (ps:chain (req "ace/keyboard/vim") -code-mirror -vim (map key command mode)))
                (vi-noremap (key command mode)
                  (ps:chain (req "ace/keyboard/vim") -code-mirror -vim (noremap key command mode)))
                (vi-define-ex (name pre fn)
                  (ps:chain (req "ace/keyboard/vim") -code-mirror -vim (define-ex name pre fn))))
           ;; set theme
           (ps:chain editor (set-theme (parenscript:create css-class "oxocarbon" is-dark t)))
           ;; load extensions
           (req "ace/ext/language_tools")
           (req "ace/ext/whitespace")
           (ps:chain (req "ace/ext/settings_menu") (init editor))
           (ps:chain (req "ace/ext/keybinding_menu") (init editor))
           (ps:chain editor session
                     (set-mode (ps:chain (req "ace/ext/modelist")
                                         (get-mode-for-path (ps:@ window location href)) mode)))
           ;; editor config 
           (ps:chain editor (set-option "cursorStyle" "wide"))          ;; static cursor
           (ps:chain editor (set-option "readOnly" nil))                ;; set read and write file
           (ps:chain editor (set-option "fontSize" 15))                 ;; bigger default font
           (ps:chain editor (set-option "showLineNumbers" nil))         ;; disable line numbers
           (ps:chain editor (set-option "showPrintMargin" t))           ;; enable print margin (colorline)
           (ps:chain editor (set-option "wrap" "free"))                 ;; wrap to end of screen 
           (ps:chain editor (set-option "displayIndentGuides" nil))     ;; disable indent markers
           (ps:chain editor (set-option "hScrollBarAlwaysVisible" nil)) ;; don't always show scrollbar (h)
           (ps:chain editor (set-option "vScrollBarAlwaysVisible" nil)) ;; don't always show scrollbar (v)
           (ps:chain editor (set-option "animatedScroll" t))            ;; animate smooth scrollling 
           (ps:chain editor (set-option "useSoftTabs" t))               ;; use spaces instead of tabs
           (ps:chain editor (set-option "enableSnippets" t))            ;; enable snippet support
           (ps:chain editor (set-option "enableBasicAutocompletion" t)) ;; enable autocomplete support (basic)
           (ps:chain editor (set-option "enableLiveAutocompletion" t))  ;; enable autocomplete support (live)
           (ps:chain editor (set-option "highlightActiveLine" t))       ;; highlight current line
           ;; vim bindings
           (vi-noremap "j" "gj" "normal")
           (vi-noremap "k" "gk" "normal")
           ;; vim ex commands (ace)
           (vi-define-ex "help" "h" (lambda ()
                                        (ps:chain editor (show-keyboard-shortcuts))))
           (vi-define-ex "settings" "se" (lambda ()
                                            (ps:chain editor (show-settings-menu))))
           ;; vim ex commands (nyxt
           ;; (vi-define-ex "write" "w" (editor-write-file))
           ;; (vi-define-ex "quit" "q" (delete-current-buffer))
           ;; (vi-define-ex "wq" "wq" (lambda ()
           ;;                           (editor-write-file)
           ;;                           (delete-current-buffer)))
           ;; (vi-define-ex "edit" "e" (editor-open-file))
           ;; (vi-define-ex "buffer" "b" (switch-buffer))
           ;; load workers
           (req "ace/worker/base")))))))

(defmethod nyxt:default-modes append ((buffer my-editor-buffer))
  "Add `my-editor-mode' and `ace-mode' to `my-editor-buffer' by default."
  (list 'my-editor-mode 'ace-mode))
