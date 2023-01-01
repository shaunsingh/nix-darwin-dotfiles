(in-package #:nyxt-user)

;;;; This is a configuration for the Ace editor Nyxt integration
;;;; (https://github.com/atlas-engineer/nx-ace).

(define-configuration nx-ace:ace-mode
  ((nx-ace:extensions
    (mapcar
     (lambda (name)
       (quri:merge-uris (quri:uri name)
                        (quri:uri "https://cdnjs.cloudflare.com/ajax/libs/ace/1.9.6/")))
     '("keybinding-vim.min.js"
       ;; Theme
       "theme-xcode.min.js"
       ;; Language modes
       "mode-c_cpp.min.js"
       "mode-asciidoc.min.js"
       "mode-clojure.min.js"
       "mode-csharp.min.js"
       "mode-css.min.js"
       "mode-diff.min.js"
       "mode-dot.min.js"
       "mode-forth.min.js"
       "mode-fsharp.min.js"
       "mode-gitignore.min.js"
       "mode-glsl.min.js"
       "mode-golang.min.js"
       "mode-haskell.min.js"
       "mode-html.min.js"
       "mode-ini.min.js"
       "mode-java.min.js"
       "mode-javascript.min.js"
       "mode-json.min.js"
       "mode-jsx.min.js"
       "mode-julia.min.js"
       "mode-kotlin.min.js"
       "mode-latex.min.js"
       "mode-lisp.min.js"
       "mode-lua.min.js"
       "mode-makefile.min.js"
       "mode-markdown.min.js"
       "mode-mediawiki.min.js"
       "mode-nix.min.js"
       "mode-objectivec.min.js"
       "mode-perl.min.js"
       "mode-plain_text.min.js"
       "mode-python.min.js"
       "mode-r.min.js"
       "mode-robot.min.js"
       "mode-ruby.min.js"
       "mode-rust.min.js"
       "mode-scala.min.js"
       "mode-scheme.min.js"
       "mode-sh.min.js"
       "mode-snippets.min.js"
       "mode-sql.min.js"
       "mode-svg.min.js"
       "mode-tex.min.js"
       "mode-text.min.js"
       "mode-tsx.min.js"
       "mode-typescript.min.js"
       "mode-xml.min.js"
       "mode-yaml.min.js"
       ;; Snippets
       "snippets/c_cpp.min.js"
       "snippets/css.min.js"
       "snippets/html.min.js"
       "snippets/javascript.min.js"
       "snippets/json.min.js"
       "snippets/latex.min.js"
       "snippets/lisp.min.js"
       "snippets/makefile.min.js"
       "snippets/markdown.min.js"
       "snippets/plain_text.min.js"
       "snippets/python.min.js"
       "snippets/scheme.min.js"
       "snippets/snippets.min.js"
       "snippets/tex.min.js"
       "snippets/text.min.js"
       "snippets/yaml.min.js"
       ;; Language Workers
       "worker-base.min.js"
       "worker-css.min.js"
       "worker-html.min.js"
       "worker-javascript.min.js"
       "worker-json.min.js"
       "worker-xml.min.js"
       "worker-yaml.min.js"
       ;; Extensions
       "ext-language_tools.min.js"
       "ext-emmet.min.js"
       "ext-keybinding_menu.min.js"
       "ext-modelist.min.js"
       "ext-searchbox.min.js"
       "ext-settings_menu.min.js"
       "ext-themelist.min.js"
       "ext-beautify.min.js"
       "ext-prompt.min.js"
       "ext-split.min.js"
       "ext-whitespace.min.js"
       "ext-statusbar.min.js")))))

(define-configuration nx-ace:ace-mode
  ((nx-ace::theme "ace/theme/xcode")
   (nx-ace::keybindings "ace/keyboard/vim")))

(define-configuration nx-ace:ace-mode
  ((nx-ace:epilogue
    (str:concat
     (ps:ps
       (flet ((req (ext)
                (ps:chain ace (require ext)))
              (bind (key command)
                (ps:chain editor commands (bind-key key command))))
         (req "ace/ext/searchbox")
         (req "ace/ext/split")
         (req "ace/ext/themelist")
         (req "ace/ext/emmet")
         (req "ace/ext/language_tools")
         (req "ace/worker/javascript")
         (ps:chain editor (set-option "fontSize" 16))
         (ps:chain editor (set-option "enableBasicAutocompletion" t))
         (ps:chain editor (set-option "enableSnippets" t))
         (ps:chain editor session
                   (set-mode (ps:chain (req "ace/ext/modelist")
                                       (get-mode-for-path (ps:@ window location href)) mode)))
         (req "ace/ext/split")
         (ps:chain (req "ace/ext/settings_menu") (init editor))
         (ps:chain (req "ace/ext/keybinding_menu") (init editor))
         (ps:chain editor commands
                   (add-command (ps:chain ace (require "ace/ext/beautify") commands 0)))))))))

(define-configuration nx-ace:ace-mode
  ((style (str:concat
           %slot-value%
           (theme:themed-css (theme *browser*)
             #+nyxt-3-pre-release-1
             ("#kbshortcutmenu"
              :background-color theme:background
              :color theme:on-background)
             #+(and nyxt-3 (not nyxt-3-pre-release-1))
             `("#kbshortcutmenu"
               :background-color ,theme:background
               :color ,theme:on-background))))
   (nx-ace::keybindings "ace/keyboard/vim")))

(define-configuration nyxt/editor-mode::editor-buffer
  ((default-modes `(nx-ace:ace-mode ,@%slot-value%))))
