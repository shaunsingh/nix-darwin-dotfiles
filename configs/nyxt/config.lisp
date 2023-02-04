(in-package #:nyxt-user)

;; don't restore on startup
;; (define-configuration browser 
;;   ((restore-session-on-startup-p nil)))

;; modes to enable in web-buffer
(defvar *web-buffer-modes*
  '(nyxt/vi-mode:vi-normal-mode
    nyxt/blocker-mode:blocker-mode 
    nyxt/force-https-mode:force-https-mode
    ;; nyxt/reduce-tracking-mode:reduce-tracking-mode
    ;; nyxt/user-script-mode:user-script-mode
    nyxt/bookmarklets-mode:bookmarklets-mode))

(define-configuration web-buffer
  ((default-modes `(,@*web-buffer-modes*
                    ,@%slot-value%))))

;; don't autocomplete when its unessecary 
(define-configuration web-buffer
  ((search-always-auto-complete-p t)))

;; enable proxy in nosave (private, incognito) buffers.
(define-configuration nosave-buffer
  ((default-modes `(nyxt/proxy-mode:proxy-mode
                    ,@*web-buffer-modes*
                    ,@%slot-value%))))

;; we wan't to be in insert mode in the pronmpt buffer
(define-configuration (prompt-buffer)
   ((default-modes `(nyxt/vi-mode:vi-insert-mode
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

;; commands 
(define-command-global open-in-firefox ()
   "Open url in firefox"
   (uiop:run-program 
     (list "firefox" (render-url 
		       (url (current-buffer))))))

;; webkit config 
(defmethod ffi-buffer-make :after ((buffer buffer))
  (let* ((settings (webkit:webkit-web-view-get-settings
                     (nyxt/renderer/gtk::gtk-object buffer))))
    (setf
     (webkit:webkit-settings-enable-developer-extras settings) t
     (webkit:webkit-settings-enable-resizable-text-areas settings) t
     ;; (webkit:webkit-settings-default-font-family settings) "SF Pro Text"
     (webkit:webkit-settings-default-font-family settings) "Liga SFMono Nerd Font"
     (webkit:webkit-settings-default-font-size settings) 11
     (webkit:webkit-settings-monospace-font-family settings) "Liga SFMono Nerd Font"
     (webkit:webkit-settings-default-monospace-font-size settings) 9)))

;; tells websites to use dark mode by default
(setf (uiop:getenv "GTK_THEME") "Adwaita:dark")

;; oxocarbon theme
(define-configuration browser
  ((theme (make-instance
	   'theme:theme
	   :dark-p t
	   :background-color "#161616"
	   :on-background-color "#dde1e6"
	   :accent-color "#ff7eb6"
	   :primary-color "#f2f4f8"
	   :on-primary-color "#262626"
	   :secondary-color "#393939"
	   :on-secondary-color "#ffffff"))))

;; (define-configuration prompt-buffer
;;   ((style (str:concat %slot-value%
;;                       (theme:themed-css (theme *browser*)
;; 		       '((body
;; 			  :margin "8px"
;; 			  :overflow "hidden"
;; 			  :line-height "18px")
;; 		         ("#prompt-area"
;; 		          :background-color "#161616"
;; 		          :color "#dde1e6"
;; 			  :display "grid"
;; 			  :grid-template-columns "auto auto 1fr auto"
;; 			  :width "100%")
;; 		         ("#input"
;; 		          :background-color "#131313"
;; 		          :color "#ffffff")
;; 		         (".source-name"
;; 		          :background-color "#161616"
;; 		          :color "#dde1e6")
;; 		         (".source-content"
;; 		          :background-color "#161616"
;; 		          :color "#dde1e6")
;; 		         (".source-content th"
;; 		          :background-color "#262626"
;; 			  :color "#dde1e6")
;; 		         ("#selection"
;; 		          :background-color "#262626"
;; 		          :color "#ffffff")
;; 		         (".selected"
;; 		          :background-color "#262626"
;; 		          :color "#ffffff")
;; 		         (".marked"
;; 		          :color "#dde1e6"
;; 		          :font-weight "bold"
;; 		          :background-color "#393939")))))))

;; status-buffer & message-buffer
(define-configuration status-buffer
  ((height 31)))

(defun my-status-style ()
  (theme:themed-css (theme *browser*)
     (body
      :margin "9px"
      :background-color "#262626"
      :color "#dde1e6")
     (\.loader
      :border-style "solid"
      :border-color "transparent"
      :border-top-color "#ff7eb6"
      :border-left-color "#ff7eb6"
      :display "inline-block"
      :animation "spin 0.5s linear infinite")
     ("@keyframes spin"
      ("0%" :transform "rotate(0deg)")
      ("100%" :transform "rotate(360deg)"))
     ("#container"
      :display "flex"
      :white-space "nowrap"
      :overflow "hidden")
     ("#vi-mode"
      :padding-left "9px")
     ("#buffers"
      :padding-left "9px")
     ("#url"
      :color "#ffffff"
      :padding-left "9px"
      :font-weight "bold")
     ("#percentage"
      :padding-left "12px")
     ("#modes"
      :padding-left "9px")
     ("#url a.button"
      :color "inherit")
     (".button"
      :color "inherit")))

(defun my-format-status-url (buffer)
  (let ((url (render-url (url buffer))))
    (spinneret:with-html-string
     (:a :class "button"
	 :title url
	 (if (str:emptyp url)
	     (title buffer)
	     (format nil " ~a — ~a~@[ [~d]~]"
		     (str:prune 50 url :ellipsis "…")
		     (title buffer)
		     (when (find (url buffer) (remove buffer (buffer-list))
                                      :test #'url-equal :key #'url)
		       (id buffer))))))))

(define-parenscript %percentage ()
  (defun percentage ()
    (let* ((height-of-window (ps:@ window inner-height))
           (content-scrolled (ps:@ window page-y-offset))
           (body-height (if (not (or (eql window undefined)
                                     (eql (ps:@ window document) undefined)
                                     (eql (ps:chain window
                                                    document
                                                    (get-elements-by-tag-name "body"))
                                          undefined)
                                     (eql (ps:chain window
                                                    document
                                                    (get-elements-by-tag-name "body")
                                                    0)
                                          undefined)
                                     (eql (ps:chain window
                                                    document
                                                    (get-elements-by-tag-name "body")
                                                    0
                                                    offset-height)
                                          undefined)))
                          (ps:chain window
                                    document
                                    (get-elements-by-tag-name "body")
                                    0
                                    offset-height)
                          0))
           (total (- body-height height-of-window))
           (prc (* (/ content-scrolled total) 100)))
      (if (> prc 100)
          100
          (round prc))))
  (percentage))

(defmethod format-status ((status status-buffer))
  (let ((buffer (current-buffer (window status))))
    (setf (style status) (my-status-style))
    (spinneret:with-html-string
      (:div :id "container"
            (:div :id "vi-mode"
		  "U:**-")
 	    (:div :id "buffers"
 		  (format nil "[~a]" (length (buffer-list))))
            (:div :id "url"
                  (:raw
                   (format-status-load-status status)
                   (my-format-status-url buffer)))
	    (:div :id "percentage"
		  (format nil "~:[0~;~:*~a~]%" (%percentage)))
            (:div :id "modes"
		  (format nil "(~d)" (nyxt::modes-string buffer)))))))

(define-configuration (window)
  ((message-buffer-height 13)))

(define-configuration status-buffer
  ((height 27)))

;; startpage
(define-internal-page-command-global startpage ()
  (buffer "*startpage*")
  (spinneret:with-html-string
    (:nstyle (style buffer))
    (:p :class "motto"
     "nyoom engineering _"
     (:br)
     "私たちのミッションは"
     (:br)
     "先端工学を用いて上質で"
     (:br)
     "機能的なデザインの"
     (:br)
     "製品を作り出すことです。")
    (:p :class "version"
     (format nil "nyxt ~a ~a" (name nyxt::*renderer*) nyxt::+version+)
     (:br) 
     (format nil "kernel ~a ~a" (software-type) (software-version))
     (:br) 
     (format nil "lisp ~a ~a" (lisp-implementation-type) (lisp-implementation-version)))
    (:ul 
     (:li (:a :href (nyxt-url 'nyxt:manual) "manual"))
     (:li (:a :href (nyxt-url 'nyxt:tutorial) "tutorial"))
     (:li (:a :href (nyxt-url 'nyxt:describe-bindings) "bindings"))
     (:li (:a :href (nyxt-url 'nyxt:changelog) "changelog"))
     (:li (:a :href (nyxt-url 'nyxt/bookmark-mode:list-bookmarks) "bookmarks"))
     (:li (:a :href (nyxt-url 'nyxt/annotate-mode:show-annotations) "annotations")))
    (:h2 "work")
    (:ul 
      (:li (:a :href "https://mail.google.com" "gmail"))
      (:li (:a :href "https://annas-archive.org" "textbooks"))
      (:li (:a :href "https://www.collegeboard.org" "collegeboard")))
    (:h2 "dev")
    (:ul 
      (:li (:a :href "https://nyxt.atlas.engineer" "nyxt"))
      (:li (:a :href "https://github.com" "github"))
      (:li (:a :href "https://this-week-in-neovim.org" "TWI neovim")))
    (:h2 "fun")
    (:ul 
      (:li (:a :href "https://www.twitch.tv" "twitch"))
      (:li (:a :href "https://yewtu.be/feed/popular" "invidious"))
      (:li (:a :href "https://zoro.to" "zoroanime")))
    (:h2 "social")
    (:ul 
      (:li (:a :href "https://staging.fosscord.com/login" "discord"))
      (:li (:a :href "https://www.instagram.com" "instagram"))
      (:li (:a :href "https://hackerweb.app" "hackernews")))
    (:h2 "history")
    (:ul (:raw (nyxt::history-html-list :limit 10)))))

;; extensions
(defmacro load-after-system* (system file)
  `(define-nyxt-user-system-and-load ,(gensym "NYXT-USER/")
     :depends-on (,system) :components (,file)))

(load-after-system* :nx-dark-reader "dark-reader")
(load-after-system* :nx-search-engines "search-engines")

;; use ace as the editor 
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
      :src "https://cdnjs.cloudflare.com/ajax/libs/ace/1.14.0/ace.min.js"
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
			  (quri:uri "https://cdnjs.cloudflare.com/ajax/libs/ace/1.14.0/")))
       '("keybinding-vim.min.js"
         "theme-xcode.min.js"
         "mode-abap.min.js"
         "mode-abc.min.js"
         "mode-actionscript.min.js"
         "mode-ada.min.js"
         "mode-alda.min.js"
         "mode-apache_conf.min.js"
         "mode-apex.min.js"
         "mode-applescript.min.js"
         "mode-aql.min.js"
         "mode-asciidoc.min.js"
         "mode-asl.min.js"
         "mode-assembly_x86.min.js"
         "mode-autohotkey.min.js"
         "mode-batchfile.min.js"
         "mode-bibtex.min.js"
         "mode-c9search.min.js"
         "mode-c_cpp.min.js"
         "mode-cirru.min.js"
         "mode-clojure.min.js"
         "mode-cobol.min.js"
         "mode-coffee.min.js"
         "mode-coldfusion.min.js"
         "mode-crystal.min.js"
         "mode-csharp.min.js"
         "mode-csound_document.min.js"
         "mode-csound_orchestra.min.js"
         "mode-csound_score.min.js"
         "mode-csp.min.js"
         "mode-css.min.js"
         "mode-curly.min.js"
         "mode-d.min.js"
         "mode-dart.min.js"
         "mode-diff.min.js"
         "mode-django.min.js"
         "mode-dockerfile.min.js"
         "mode-dot.min.js"
         "mode-drools.min.js"
         "mode-edifact.min.js"
         "mode-effel.min.js"
         "mode-ejs.min.js"
         "mode-elixir.min.js"
         "mode-elm.min.js"
         "mode-erlang.min.js"
         "mode-forth.min.js"
         "mode-fortran.min.js"
         "mode-fsharp.min.js"
         "mode-fsl.min.js"
         "mode-ftl.min.js"
         "mode-gcode.min.js"
         "mode-gherkin.min.js"
         "mode-gitignore.min.js"
         "mode-glsl.min.js"
         "mode-gobstones.min.js"
         "mode-golang.min.js"
         "mode-graphqlschema.min.js"
         "mode-groovy.min.js"
         "mode-haml.min.js"
         "mode-handlebars.min.js"
         "mode-haskell.min.js"
         "mode-haskell_cabal.min.js"
         "mode-haxe.min.js"
         "mode-hjson.min.js"
         "mode-html.min.js"
         "mode-html_elixir.min.js"
         "mode-html_ruby.min.js"
         "mode-ini.min.js"
         "mode-io.min.js"
         "mode-ion.min.js"
         "mode-jack.min.js"
         "mode-jade.min.js"
         "mode-java.min.js"
         "mode-javascript.min.js"
         "mode-jexl.min.js"
         "mode-jexl.min.js"
         "mode-json.min.js"
         "mode-json5.min.js"
         "mode-jsoniq.min.js"
         "mode-jsp.min.js"
         "mode-jssm.min.js"
         "mode-jsx.min.js"
         "mode-julia.min.js"
         "mode-kotlin.min.js"
         "mode-latex.min.js"
         "mode-latte.min.js"
         "mode-less.min.js"
         "mode-liquid.min.js"
         "mode-lisp.min.js"
         "mode-livescript.min.js"
         "mode-logiql.min.js"
         "mode-logtalk.min.js"
         "mode-lsl.min.js"
         "mode-lua.min.js"
         "mode-luapage.min.js"
         "mode-lucene.min.js"
         "mode-makefile.min.js"
         "mode-markdown.min.js"
         "mode-mask.min.js"
         "mode-matlab.min.js"
         "mode-maze.min.js"
         "mode-mediawiki.min.js"
         "mode-mel.min.js"
         "mode-mips.min.js"
         "mode-mixal.min.js"
         "mode-mushcode.min.js"
         "mode-mysql.min.js"
         "mode-nginx.min.js"
         "mode-nim.min.js"
         "mode-nix.min.js"
         "mode-nsis.min.js"
         "mode-nunjucks.min.js"
         "mode-onjectivec.min.js"
         "mode-ocaml.min.js"
         "mode-partiql.min.js"
         "mode-pascal.min.js"
         "mode-perl.min.js"
         "mode-pgsql.min.js"
         "mode-php.min.js"
         "mode-php_laravel_blade.min.js"
         "mode-pig.min.js"
         "mode-plain_text.min.js"
         "mode-powershell.min.js"
         "mode-praat.min.js"
         "mode-prisma.min.js"
         "mode-prolog.min.js"
         "mode-properties.min.js"
         "mode-protobuf.min.js"
         "mode-puppet.min.js"
         "mode-python.min.js"
         "mode-qml.min.js"
         "mode-r.min.js"
         "mode-raku.min.js"
         "mode-razor.min.js"
         "mode-rdoc.min.js"
         "mode-red.min.js"
         "mode-redshift.min.js"
         "mode-rhtml.min.js"
         "mode-robot.min.js"
         "mode-rst.min.js"
         "mode-ruby.min.js"
         "mode-rust.min.js"
         "mode-sac.min.js"
         "mode-sass.min.js"
         "mode-scad.min.js"
         "mode-scala.min.js"
         "mode-scheme.min.js"
         "mode-scrypt.min.js"
         "mode-scss.min.js"
         "mode-sh.min.js"
         "mode-sjs.min.js"
         "mode-slim.min.js"
         "mode-smarty.min.js"
         "mode-smithy.min.js"
         "mode-snippets.min.js"
         "mode-soy_template.min.js"
         "mode-space.min.js"
         "mode-sparql.min.js"
         "mode-sql.min.js"
         "mode-sqlserver.min.js"
         "mode-stylus.min.js"
         "mode-svg.min.js"
         "mode-swift.min.js"
         "mode-tcl.min.js"
         "mode-terraform.min.js"
         "mode-tex.min.js"
         "mode-text.min.js"
         "mode-textile.min.js"
         "mode-toml.min.js"
         "mode-tsx.min.js"
         "mode-turtle.min.js"
         "mode-twig.min.js"
         "mode-typescript.min.js"
         "mode-vala.min.js"
         "mode-vbscript.min.js"
         "mode-velocity.min.js"
         "mode-verilog.min.js"
         "mode-vhdl.min.js"
         "mode-visualforce.min.js"
         "mode-wollok.min.js"
         "mode-xml.min.js"
         "mode-xquery.min.js"
         "mode-yaml.min.js"
         "mode-zeek.min.js"
         "snippets/abap.min.js"
         "snippets/abc.min.js"
         "snippets/actionscript.min.js"
         "snippets/ada.min.js"
         "snippets/alda.min.js"
         "snippets/apache_conf.min.js"
         "snippets/apex.min.js"
         "snippets/applescript.min.js"
         "snippets/aql.min.js"
         "snippets/asciidoc.min.js"
         "snippets/asl.min.js"
         "snippets/assembly_x86.min.js"
         "snippets/autohotkey.min.js"
         "snippets/batchfile.min.js"
         "snippets/bibtex.min.js"
         "snippets/c9search.min.js"
         "snippets/c_cpp.min.js"
         "snippets/cirru.min.js"
         "snippets/clojure.min.js"
         "snippets/cobol.min.js"
         "snippets/coffee.min.js"
         "snippets/coldfusion.min.js"
         "snippets/crystal.min.js"
         "snippets/csharp.min.js"
         "snippets/csound_document.min.js"
         "snippets/csound_orchestra.min.js"
         "snippets/csound_score.min.js"
         "snippets/csp.min.js"
         "snippets/css.min.js"
         "snippets/curly.min.js"
         "snippets/d.min.js"
         "snippets/dart.min.js"
         "snippets/diff.min.js"
         "snippets/django.min.js"
         "snippets/dockerfile.min.js"
         "snippets/dot.min.js"
         "snippets/drools.min.js"
         "snippets/edifact.min.js"
         "snippets/effel.min.js"
         "snippets/ejs.min.js"
         "snippets/elixir.min.js"
         "snippets/elm.min.js"
         "snippets/erlang.min.js"
         "snippets/forth.min.js"
         "snippets/fortran.min.js"
         "snippets/fsharp.min.js"
         "snippets/fsl.min.js"
         "snippets/ftl.min.js"
         "snippets/gcode.min.js"
         "snippets/gherkin.min.js"
         "snippets/gitignore.min.js"
         "snippets/glsl.min.js"
         "snippets/gobstones.min.js"
         "snippets/golang.min.js"
         "snippets/graphqlschema.min.js"
         "snippets/groovy.min.js"
         "snippets/haml.min.js"
         "snippets/handlebars.min.js"
         "snippets/haskell.min.js"
         "snippets/haskell_cabal.min.js"
         "snippets/haxe.min.js"
         "snippets/hjson.min.js"
         "snippets/html.min.js"
         "snippets/html_elixir.min.js"
         "snippets/html_ruby.min.js"
         "snippets/ini.min.js"
         "snippets/io.min.js"
         "snippets/ion.min.js"
         "snippets/jack.min.js"
         "snippets/jade.min.js"
         "snippets/java.min.js"
         "snippets/javascript.min.js"
         "snippets/jexl.min.js"
         "snippets/jexl.min.js"
         "snippets/json.min.js"
         "snippets/json5.min.js"
         "snippets/jsoniq.min.js"
         "snippets/jsp.min.js"
         "snippets/jssm.min.js"
         "snippets/jsx.min.js"
         "snippets/julia.min.js"
         "snippets/kotlin.min.js"
         "snippets/latex.min.js"
         "snippets/latte.min.js"
         "snippets/less.min.js"
         "snippets/liquid.min.js"
         "snippets/lisp.min.js"
         "snippets/livescript.min.js"
         "snippets/logiql.min.js"
         "snippets/logtalk.min.js"
         "snippets/lsl.min.js"
         "snippets/lua.min.js"
         "snippets/luapage.min.js"
         "snippets/lucene.min.js"
         "snippets/makefile.min.js"
         "snippets/markdown.min.js"
         "snippets/mask.min.js"
         "snippets/matlab.min.js"
         "snippets/maze.min.js"
         "snippets/mediawiki.min.js"
         "snippets/mel.min.js"
         "snippets/mips.min.js"
         "snippets/mixal.min.js"
         "snippets/mushcode.min.js"
         "snippets/mysql.min.js"
         "snippets/nginx.min.js"
         "snippets/nim.min.js"
         "snippets/nix.min.js"
         "snippets/nsis.min.js"
         "snippets/nunjucks.min.js"
         "snippets/onjectivec.min.js"
         "snippets/ocaml.min.js"
         "snippets/partiql.min.js"
         "snippets/pascal.min.js"
         "snippets/perl.min.js"
         "snippets/pgsql.min.js"
         "snippets/php.min.js"
         "snippets/php_laravel_blade.min.js"
         "snippets/pig.min.js"
         "snippets/plain_text.min.js"
         "snippets/powershell.min.js"
         "snippets/praat.min.js"
         "snippets/prisma.min.js"
         "snippets/prolog.min.js"
         "snippets/properties.min.js"
         "snippets/protobuf.min.js"
         "snippets/puppet.min.js"
         "snippets/python.min.js"
         "snippets/qml.min.js"
         "snippets/r.min.js"
         "snippets/raku.min.js"
         "snippets/razor.min.js"
         "snippets/rdoc.min.js"
         "snippets/red.min.js"
         "snippets/redshift.min.js"
         "snippets/rhtml.min.js"
         "snippets/robot.min.js"
         "snippets/rst.min.js"
         "snippets/ruby.min.js"
         "snippets/rust.min.js"
         "snippets/sac.min.js"
         "snippets/sass.min.js"
         "snippets/scad.min.js"
         "snippets/scala.min.js"
         "snippets/scheme.min.js"
         "snippets/scrypt.min.js"
         "snippets/scss.min.js"
         "snippets/sh.min.js"
         "snippets/sjs.min.js"
         "snippets/slim.min.js"
         "snippets/smarty.min.js"
         "snippets/smithy.min.js"
         "snippets/snippets.min.js"
         "snippets/soy_template.min.js"
         "snippets/space.min.js"
         "snippets/sparql.min.js"
         "snippets/sql.min.js"
         "snippets/sqlserver.min.js"
         "snippets/stylus.min.js"
         "snippets/svg.min.js"
         "snippets/swift.min.js"
         "snippets/tcl.min.js"
         "snippets/terraform.min.js"
         "snippets/tex.min.js"
         "snippets/text.min.js"
         "snippets/textile.min.js"
         "snippets/toml.min.js"
         "snippets/tsx.min.js"
         "snippets/turtle.min.js"
         "snippets/twig.min.js"
         "snippets/typescript.min.js"
         "snippets/vala.min.js"
         "snippets/vbscript.min.js"
         "snippets/velocity.min.js"
         "snippets/verilog.min.js"
         "snippets/vhdl.min.js"
         "snippets/visualforce.min.js"
         "snippets/wollok.min.js"
         "snippets/xml.min.js"
         "snippets/xquery.min.js"
         "snippets/yaml.min.js"
         "snippets/zeek.min.js"
         "worker-base.min.js"
         "worker-coffee.min.js"
         "worker-css.min.js"
         "worker-html.min.js"
         "worker-javascript.min.js"
         "worker-json.min.js"
         "worker-lua.min.js"
         "worker-php.min.js"
         "worker-xml.min.js"
         "worker-xquery.min.js"
         "worker-yaml.min.js"
         "ext-beautify.min.js"
         "ext-code_lens.min.js"
         "ext-keybinding_menu.min.js"
         "ext-language_tools.min.js"
         "ext-modelist.min.js"
         "ext-searchbox.min.js"
         "ext-settings_menu.min.js"
         "ext-split.min.js"
         "ext-whitespace.min.js")))))

(define-configuration ace-mode
  ((epilogue
    (str:concat
     (ps:ps
       (flet ((req (ext)
                (ps:chain ace (require ext)))
	      (bind (key command)
		(ps:chain editor commands (bind-key key command))))
         (req "ace/ext/searchbox")
         (req "ace/ext/split")
         (req "ace/ext/language_tools")
         (req "ace/ext/code_lens")
         (req "ace/ext/whitespace")
         (req "ace/worker/base")
         (req "ace/worker/coffee")
         (req "ace/worker/css")
         (req "ace/worker/html")
         (req "ace/worker/javascript")
         (req "ace/worker/json")
         (req "ace/worker/lua")
         (req "ace/worker/php")
         (req "ace/worker/xml")
         (req "ace/worker/xquery")
         (req "ace/worker/yaml")
         (ps:chain (req "ace/ext/settings_menu") (init editor))
         (ps:chain (req "ace/ext/keybinding_menu") (init editor))
	 (bind "Ctrl-h m" (lambda (editor) (ps:chain editor (show-keyboard-shortcuts))))
         (ps:chain editor (set-option "cursorStyle" "smooth slim"))
         (ps:chain editor (set-option "readOnly" nil))
         (ps:chain editor (set-option "showLineNumbers" nil))
         (ps:chain editor (set-option "showPrintMargin" nil))
         (ps:chain editor (set-option "displayIndentGuides" nil))
         (ps:chain editor (set-option "hScrollBarAlwaysVisible" nil))
         (ps:chain editor (set-option "vScrollBarAlwaysVisible" nil))
         (ps:chain editor (set-option "useSoftTabs" t))
         (ps:chain editor (set-option "enableSnippets" t))
         (ps:chain editor (set-option "highlightActiveLine" t))
         (ps:chain editor (set-option "enableBasicAutocompletion" t))
         (ps:chain editor session
                   (set-mode (ps:chain (req "ace/ext/modelist")
                                       (get-mode-for-path (ps:@ window location href)) mode)))
         (ps:chain editor commands
                   (add-command (ps:chain ace (require "ace/ext/beautify") commands 0)))))))))

(define-configuration ace-mode
  ((style (str:concat
           %slot-value%
           (theme:themed-css (theme *browser*)
             ("#kbshortcutmenu"
              :background-color theme:background
              :color theme:on-background))))
   (:theme "ace/theme/xcode")
   (:keybindings "ace/keyboard/vim")))

(define-configuration nyxt/editor-mode::editor-buffer
  ((default-modes `(ace-mode ,@%slot-value%))))
