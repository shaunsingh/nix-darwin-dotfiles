(in-package #:nyxt-user)

;; this file was written and edited with ace inside nyxt :)
;; defines `ace-mode`, handling files in `editor-mode`
;; modified from https://github.com/atlas-engineer/nx-ace
;; with the following changes 

;; - updated ace version
;; - support for `ace-linter` and language-servers
;; - custom oxocarbon theme
;; - vim/ex utility commands

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
      :src "https://cdnjs.cloudflare.com/ajax/libs/ace/1.15.3/ace.min.js"
      :crossorigin "anonymous"
      :type "text/javascript"
      :charset "utf-8"
      "")
     (:script
      :src "https://www.unpkg.com/ace-linters@0.6.0/build/ace-linters.js"
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
  ((style (str:concat
           %slot-value%
           (theme:themed-css (theme *browser*)
              `(".oxocarbon"
               :color ,*base05*
               :background-color ,*base00*)
              `(".oxocarbon .ace_gutter"
               :color ,*base03*
               :background-color "#131313")
              `(".oxocarbon .ace_print-margin"
               :width "1px"
               :background-color ,*base01*)
              `(".oxocarbon .ace_cursor"
               :color ,*base06*)
              `(".oxocarbon .ace_marker-layer .ace_selection"
               :background-color ,*base02*)
              `(".oxocarbon .ace_marker-layer .ace_step"
               :background-color ,*base0A*)
              `(".oxocarbon .ace_marker-layer .ace_active-line"
               :background-color ,*base01*)
              `(".oxocarbon .ace_gutter-active-line"
               :color ,*base06*)
              `(".oxocarbon .ace_marker-layer .ace_selected-word"
               :background-color ,*base02*)
              `(".oxocarbon .ace_fold"
               :color ,*base04*)
              ;; token styles
              `(".oxocarbon .ace_comment"
               :color ,*base03*)
              `(".oxocarbon .ace_keyword"
               :color ,*base0C*)
              `(".oxocarbon .ace_constant.ace_numeric"
               :color ,*base0F*)
              `(".oxocarbon .ace_constant.ace_character"
               :color ,*base07*)
              `(".oxocarbon .ace_constant.ace_character.ace_escape"
               :color ,*base08*)
              `(".oxocarbon .ace_constant.ace_character.ace_language"
               :color ,*base09*)
              `(".oxocarbon .ace_constant.ace_character.ace_other"
               :color ,*base09*)
              `(".oxocarbon .ace_support.ace_function"
               :font-weight "bold"
               :color ,*base0C*)
              `(".oxocarbon .ace_support.ace_constant"
               :color ,*base07*)
              `(".oxocarbon .ace_support.ace_class"
               :color ,*base08*)
              `(".oxocarbon .ace_support.ace_type"
               :color ,*base08*)
              `(".oxocarbon .ace_storage"
               :color ,*base09*)
              `(".oxocarbon .ace_storage.ace_type"
               :color ,*base08*)
              `(".oxocarbon .ace_invalid"
               :color ,*base0A*)
              `(".oxocarbon .ace_invalid.ace_deprecated"
               :color ,*base03*)
              `(".oxocarbon .ace_string"
               :color ,*base0E*)
              `(".oxocarbon .ace_variable"
               :color ,*base0F*)
              `(".oxocarbon .ace_variable.ace_parameter"
               :color ,*base04*)
              `(".oxocarbon .ace_entity.ace_other.ace_attribute-name"
               :color ,*base0B*)
              `(".oxocarbon .ace_entity.ace_name.ace_tag"
               :color ,*base0F*)
              `(".oxocarbon .ace_invisible"
               :color ,*base03*))))
   (:keybindings "ace/keyboard/vim")
   (extensions
     (mapcar
       (lambda (name)
         (quri:merge-uris (quri:uri name)
        (quri:uri "https://cdnjs.cloudflare.com/ajax/libs/ace/1.15.3/")))
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
         "ext-searchbox.min.js"        ;; used for cmd/ctrl-f dialogue
         "ext-whitespace.min.js"       ;; detect spacing/indent
         "ext-split.min.js"            ;; enable split functionality
         "ext-settings_menu.min.js"    ;; view and adjust settings
         "ext-keybinding_menu.min.js"  ;; view and adjust keybindings
         "ext-modelist.min.js"         ;; detect mode based on filepath
         "ext-beautify.min.js")))      ;; formatting support
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
           (req "ace/ext/searchbox")
           (req "ace/ext/whitespace")
           (req "ace/ext/split")
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
           (vi-noremap "j" "gj" "normal")
           (vi-noremap "k" "gk" "normal")
           ;; vim ex commands
           (vi-define-ex "write" "w" (lambda (cm input)
                                        (ps:chain cm ace (exec-command "save"))))
           (vi-define-ex "help" "h" (lambda ()
                                        (ps:chain editor (show-keyboard-shortcuts))))
           (vi-define-ex "settings" "se" (lambda ()
                                            (ps:chain editor (show-settings-menu))))
           ;; load workers
           (req "ace/worker/base")
           ;; register ace linters
           (ps:chain -language-provider (from-cdn "https://www.unpkg.com/ace-linters@0.6.0/build/")
                     (register-editor editor))))))))

;; use ace for editor-mode by default
(define-configuration nyxt/editor-mode::editor-buffer
  ((default-modes `(ace-mode ,@%slot-value%))))
