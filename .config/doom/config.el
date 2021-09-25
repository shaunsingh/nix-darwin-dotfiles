;; [[file:config.org::*Basic Configuration][Basic Configuration:2]]
(when 'native-comp-compiler-options
                 (setq native-comp-speed 3
                       native-comp-compiler-options '("-O3" "-march=native" "-mtune=native")))
;; Basic Configuration:2 ends here

;; [[file:config.org::*Always compile][Always compile:1]]
(setq vterm-always-compile-module t)
;; Always compile:1 ends here

;; [[file:config.org::*Kill buffer][Kill buffer:1]]
(setq vterm-kill-buffer-on-exit t)
;; Kill buffer:1 ends here

;; [[file:config.org::*Functions][Functions:1]]
(after! vterm
  (setf (alist-get "magit-status" vterm-eval-cmds nil nil #'equal)
        '((lambda (path)
            (magit-status path)))))
;; Functions:1 ends here

;; [[file:config.org::*Fonts][Fonts:5]]
;; No missing fonts detected
;; Fonts:5 ends here

;; [[file:config.org::*Company][Company:4]]
(defun +yas/org-src-header-p ()
  "Determine whether `point' is within a src-block header or header-args."
  (pcase (org-element-type (org-element-context))
    ('src-block (< (point) ; before code part of the src-block
                   (save-excursion (goto-char (org-element-property :begin (org-element-context)))
                                   (forward-line 1)
                                   (point))))
    ('inline-src-block (< (point) ; before code part of the inline-src-block
                          (save-excursion (goto-char (org-element-property :begin (org-element-context)))
                                          (search-forward "]{")
                                          (point))))
    ('keyword (string-match-p "^header-args" (org-element-property :value (org-element-context))))))
;; Company:4 ends here

;; [[file:config.org::*Company][Company:5]]
(defun +yas/org-prompt-header-arg (arg question values)
  "Prompt the user to set ARG header property to one of VALUES with QUESTION.
The default value is identified and indicated. If either default is selected,
or no selection is made: nil is returned."
  (let* ((src-block-p (not (looking-back "^#\\+property:[ \t]+header-args:.*" (line-beginning-position))))
         (default
           (or
            (cdr (assoc arg
                        (if src-block-p
                            (nth 2 (org-babel-get-src-block-info t))
                          (org-babel-merge-params
                           org-babel-default-header-args
                           (let ((lang-headers
                                  (intern (concat "org-babel-default-header-args:"
                                                  (+yas/org-src-lang)))))
                             (when (boundp lang-headers) (eval lang-headers t)))))))
            ""))
         default-value)
    (setq values (mapcar
                  (lambda (value)
                    (if (string-match-p (regexp-quote value) default)
                        (setq default-value
                              (concat value " "
                                      (propertize "(default)" 'face 'font-lock-doc-face)))
                      value))
                  values))
    (let ((selection (consult--read question values :default default-value)))
      (unless (or (string-match-p "(default)$" selection)
                  (string= "" selection))
        selection))))
;; Company:5 ends here

;; [[file:config.org::*Company][Company:6]]
(defun +yas/org-src-lang ()
  "Try to find the current language of the src/header at `point'.
Return nil otherwise."
  (let ((context (org-element-context)))
    (pcase (org-element-type context)
      ('src-block (org-element-property :language context))
      ('inline-src-block (org-element-property :language context))
      ('keyword (when (string-match "^header-args:\\([^ ]+\\)" (org-element-property :value context))
                  (match-string 1 (org-element-property :value context)))))))

(defun +yas/org-last-src-lang ()
  "Return the language of the last src-block, if it exists."
  (save-excursion
    (beginning-of-line)
    (when (re-search-backward "^[ \t]*#\\+begin_src" nil t)
      (org-element-property :language (org-element-context)))))

(defun +yas/org-most-common-no-property-lang ()
  "Find the lang with the most source blocks that has no global header-args, else nil."
  (let (src-langs header-langs)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*#\\+begin_src" nil t)
        (push (+yas/org-src-lang) src-langs))
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*#\\+property: +header-args" nil t)
        (push (+yas/org-src-lang) header-langs)))

    (setq src-langs
          (mapcar #'car
                  ;; sort alist by frequency (desc.)
                  (sort
                   ;; generate alist with form (value . frequency)
                   (cl-loop for (n . m) in (seq-group-by #'identity src-langs)
                            collect (cons n (length m)))
                   (lambda (a b) (> (cdr a) (cdr b))))))

    (car (cl-set-difference src-langs header-langs :test #'string=))))
;; Company:6 ends here

;; [[file:config.org::*Company][Company:7]]
(sp-local-pair
 '(org-mode)
 "<<" ">>"
 :actions '(insert))
;; Company:7 ends here

;; [[file:config.org::*Better Defaults][Better Defaults:2]]
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))
;; Better Defaults:2 ends here

;; [[file:config.org::*Better Defaults][Better Defaults:3]]
(setq doom-scratch-initial-major-mode 'lisp-interaction-mode)
;; Better Defaults:3 ends here

;; [[file:config.org::*Splash screen][Splash screen:1]]
(defvar fancy-splash-image-template
  (expand-file-name "misc/splash-images/emacs-e-template.svg" doom-private-dir)
  "Default template svg used for the splash image, with substitutions from ")

(defvar fancy-splash-sizes
  `((:height 300 :min-height 50 :padding (0 . 2))
    (:height 250 :min-height 42 :padding (2 . 4))
    (:height 200 :min-height 35 :padding (3 . 3))
    (:height 150 :min-height 28 :padding (3 . 3))
    (:height 100 :min-height 20 :padding (2 . 2))
    (:height 75  :min-height 15 :padding (2 . 1))
    (:height 50  :min-height 10 :padding (1 . 0))
    (:height 1   :min-height 0  :padding (0 . 0)))
  "list of plists with the following properties
  :height the height of the image
  :min-height minimum `frame-height' for image
  :padding `+doom-dashboard-banner-padding' (top . bottom) to apply
  :template non-default template file
  :file file to use instead of template")

(defvar fancy-splash-template-colours
  '(("$colour1" . keywords) ("$colour2" . type) ("$colour3" . base5) ("$colour4" . base8))
  "list of colour-replacement alists of the form (\"$placeholder\" . 'theme-colour) which applied the template")

(unless (file-exists-p (expand-file-name "theme-splashes" doom-cache-dir))
  (make-directory (expand-file-name "theme-splashes" doom-cache-dir) t))

(defun fancy-splash-filename (theme-name height)
  (expand-file-name (concat (file-name-as-directory "theme-splashes")
                            theme-name
                            "-" (number-to-string height) ".svg")
                    doom-cache-dir))

(defun fancy-splash-clear-cache ()
  "Delete all cached fancy splash images"
  (interactive)
  (delete-directory (expand-file-name "theme-splashes" doom-cache-dir) t)
  (message "Cache cleared!"))

(defun fancy-splash-generate-image (template height)
  "Read TEMPLATE and create an image if HEIGHT with colour substitutions as
   described by `fancy-splash-template-colours' for the current theme"
  (with-temp-buffer
    (insert-file-contents template)
    (re-search-forward "$height" nil t)
    (replace-match (number-to-string height) nil nil)
    (dolist (substitution fancy-splash-template-colours)
      (goto-char (point-min))
      (while (re-search-forward (car substitution) nil t)
        (replace-match (doom-color (cdr substitution)) nil nil)))
    (write-region nil nil
                  (fancy-splash-filename (symbol-name doom-theme) height) nil nil)))

(defun fancy-splash-generate-images ()
  "Perform `fancy-splash-generate-image' in bulk"
  (dolist (size fancy-splash-sizes)
    (unless (plist-get size :file)
      (fancy-splash-generate-image (or (plist-get size :template)
                                       fancy-splash-image-template)
                                   (plist-get size :height)))))

(defun ensure-theme-splash-images-exist (&optional height)
  (unless (file-exists-p (fancy-splash-filename
                          (symbol-name doom-theme)
                          (or height
                              (plist-get (car fancy-splash-sizes) :height))))
    (fancy-splash-generate-images)))

(defun get-appropriate-splash ()
  (let ((height (frame-height)))
    (cl-some (lambda (size) (when (>= height (plist-get size :min-height)) size))
             fancy-splash-sizes)))

(setq fancy-splash-last-size nil)
(setq fancy-splash-last-theme nil)
(defun set-appropriate-splash (&rest _)
  (let ((appropriate-image (get-appropriate-splash)))
    (unless (and (equal appropriate-image fancy-splash-last-size)
                 (equal doom-theme fancy-splash-last-theme)))
    (unless (plist-get appropriate-image :file)
      (ensure-theme-splash-images-exist (plist-get appropriate-image :height)))
    (setq fancy-splash-image
          (or (plist-get appropriate-image :file)
              (fancy-splash-filename (symbol-name doom-theme) (plist-get appropriate-image :height))))
    (setq +doom-dashboard-banner-padding (plist-get appropriate-image :padding))
    (setq fancy-splash-last-size appropriate-image)
    (setq fancy-splash-last-theme doom-theme)
    (+doom-dashboard-reload)))

(add-hook 'window-size-change-functions #'set-appropriate-splash)
(add-hook 'doom-load-theme-hook #'set-appropriate-splash)
;; Splash screen:1 ends here

;; [[file:config.org::*Splash screen][Splash screen:3]]
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)
(add-hook! '+doom-dashboard-mode-hook (hide-mode-line-mode 1) (hl-line-mode -1))
(setq-hook! '+doom-dashboard-mode-hook evil-normal-state-cursor (list nil))
;; Splash screen:3 ends here

;; [[file:config.org::*Writeroom][Writeroom:1]]
(setq +zen-text-scale 0.8)
;; Writeroom:1 ends here

;; [[file:config.org::*Font Display][Font Display:1]]
(add-hook 'org-mode-hook #'+org-pretty-mode)
;; Font Display:1 ends here

;; [[file:config.org::*Font Display][Font Display:2]]
(setq org-pretty-entities-include-sub-superscripts nil)
;; Font Display:2 ends here

;; [[file:config.org::*Font Display][Font Display:3]]
(custom-set-faces!
  '(org-document-title :height 1.2)
  '(outline-1 :weight extra-bold :height 1.25)
  '(outline-2 :weight bold :height 1.15)
  '(outline-3 :weight bold :height 1.12)
  '(outline-4 :weight semi-bold :height 1.09)
  '(outline-5 :weight semi-bold :height 1.06)
  '(outline-6 :weight semi-bold :height 1.03)
  '(outline-8 :weight semi-bold)
  '(outline-9 :weight semi-bold))
;; Font Display:3 ends here

;; [[file:config.org::*Font Display][Font Display:4]]
(setq org-agenda-deadline-faces
      '((1.0 . error)
        (1.0 . org-warning)
        (0.5 . org-upcoming-deadline)
        (0.0 . org-upcoming-distant-deadline)))
;; Font Display:4 ends here

;; [[file:config.org::*Font Display][Font Display:5]]
(setq org-fontify-quote-and-verse-blocks t)
;; Font Display:5 ends here

;; [[file:config.org::*Font Display][Font Display:6]]
(use-package! org-appear
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoemphasis t
        org-appear-autosubmarkers t
        org-appear-autolinks nil)
  (run-at-time nil nil #'org-appear--set-elements))
;; Font Display:6 ends here

;; [[file:config.org::*Font Display][Font Display:7]]
(defun locally-defer-font-lock ()
  "Set jit-lock defer and stealth, when buffer is over a certain size."
  (when (> (buffer-size) 50000)
    (setq-local jit-lock-defer-time 0.05
                jit-lock-stealth-time 1)))

(add-hook 'org-mode-hook #'locally-defer-font-lock)


(custom-set-faces!
  `(org-block-end-line :background ,(doom-color 'base2))
  `(org-block-begin-line :background ,(doom-color 'base2)))
;; Font Display:7 ends here

;; [[file:config.org::*Fontifying inline src blocks][Fontifying inline src blocks:1]]
(defvar org-prettify-inline-results t
  "Whether to use (ab)use prettify-symbols-mode on {{{results(...)}}}.
Either t or a cons cell of strings which are used as substitutions
for the start and end of inline results, respectively.")

(defvar org-fontify-inline-src-blocks-max-length 200
  "Maximum content length of an inline src block that will be fontified.")

(defun org-fontify-inline-src-blocks (limit)
  "Try to apply `org-fontify-inline-src-blocks-1'."
  (condition-case nil
      (org-fontify-inline-src-blocks-1 limit)
    (error (message "Org mode fontification error in %S at %d"
                    (current-buffer)
                    (line-number-at-pos)))))

(defun org-fontify-inline-src-blocks-1 (limit)
  "Fontify inline src_LANG blocks, from `point' up to LIMIT."
  (let ((case-fold-search t)
        (initial-point (point)))
    (while (re-search-forward "\\_<src_\\([^ \t\n[{]+\\)[{[]?" limit t) ; stolen from `org-element-inline-src-block-parser'
      (let ((beg (match-beginning 0))
            pt
            (lang-beg (match-beginning 1))
            (lang-end (match-end 1)))
        (remove-text-properties beg lang-end '(face nil))
        (font-lock-append-text-property lang-beg lang-end 'face 'org-meta-line)
        (font-lock-append-text-property beg lang-beg 'face 'shadow)
        (font-lock-append-text-property beg lang-end 'face 'org-block)
        (setq pt (goto-char lang-end))
        ;; `org-element--parse-paired-brackets' doesn't take a limit, so to
        ;; prevent it searching the entire rest of the buffer we temporarily
        ;; narrow the active region.
        (save-restriction
          (narrow-to-region beg (min (point-max) limit (+ lang-end org-fontify-inline-src-blocks-max-length)))
          (when (ignore-errors (org-element--parse-paired-brackets ?\[))
            (remove-text-properties pt (point) '(face nil))
            (font-lock-append-text-property pt (point) 'face 'org-block)
            (setq pt (point)))
          (when (ignore-errors (org-element--parse-paired-brackets ?\{))
            (remove-text-properties pt (point) '(face nil))
            (font-lock-append-text-property pt (1+ pt) 'face '(org-block shadow))
            (unless (= (1+ pt) (1- (point)))
              (if org-src-fontify-natively
                  (org-src-font-lock-fontify-block (buffer-substring-no-properties lang-beg lang-end) (1+ pt) (1- (point)))
                (font-lock-append-text-property (1+ pt) (1- (point)) 'face 'org-block)))
            (font-lock-append-text-property (1- (point)) (point) 'face '(org-block shadow))
            (setq pt (point))))
        (when (and org-prettify-inline-results (re-search-forward "\\= {{{results(" limit t))
          (font-lock-append-text-property pt (1+ pt) 'face 'org-block)
          (goto-char pt))))
    (when org-prettify-inline-results
      (goto-char initial-point)
      (org-fontify-inline-src-results limit))))

(defun org-fontify-inline-src-results (limit)
  (while (re-search-forward "{{{results(\\(.+?\\))}}}" limit t)
    (remove-list-of-text-properties (match-beginning 0) (point)
                                    '(composition
                                      prettify-symbols-start
                                      prettify-symbols-end))
    (font-lock-append-text-property (match-beginning 0) (match-end 0) 'face 'org-block)
    (let ((start (match-beginning 0)) (end (match-beginning 1)))
      (with-silent-modifications
        (compose-region start end (if (eq org-prettify-inline-results t) "⟨" (car org-prettify-inline-results)))
        (add-text-properties start end `(prettify-symbols-start ,start prettify-symbols-end ,end))))
    (let ((start (match-end 1)) (end (point)))
      (with-silent-modifications
        (compose-region start end (if (eq org-prettify-inline-results t) "⟩" (cdr org-prettify-inline-results)))
        (add-text-properties start end `(prettify-symbols-start ,start prettify-symbols-end ,end))))))

(defun org-fontify-inline-src-blocks-enable ()
  "Add inline src fontification to font-lock in Org.
Must be run as part of `org-font-lock-set-keywords-hook'."
  (setq org-font-lock-extra-keywords
        (append org-font-lock-extra-keywords '((org-fontify-inline-src-blocks)))))

(add-hook 'org-font-lock-set-keywords-hook #'org-fontify-inline-src-blocks-enable)
;; Fontifying inline src blocks:1 ends here

;; [[file:config.org::*Symbols][Symbols:1]]
;;make bullets look better
(after! org-superstar
  (setq org-superstar-headline-bullets-list '("◉" "○" "✸" "✿" "✤" "✜" "◆" "▶")
        org-superstar-prettify-item-bullets t ))
;; Symbols:1 ends here

;; [[file:config.org::*Org-Mode][Org-Mode:5]]
(use-package! org-pandoc-import
  :after org)
;; Org-Mode:5 ends here

;; [[file:config.org::*HTML][HTML:3]]
(after! ox-html
  
)
;; HTML:3 ends here

;; [[file:config.org::Example, fixed width, and property blocks][Example, fixed width, and property blocks]]
(defun org-html-block-collapsable (orig-fn block contents info)
  "Wrap the usual block in a <details>"
  (if (or (not org-fancy-html-export-mode) (bound-and-true-p org-msg-export-in-progress))
      (funcall orig-fn block contents info)
    (let ((ref (org-export-get-reference block info))
          (type (pcase (car block)
                  ('property-drawer "Properties")))
          (collapsed-default (pcase (car block)
                               ('property-drawer t)
                               (_ nil)))
          (collapsed-value (org-export-read-attribute :attr_html block :collapsed))
          (collapsed-p (or (member (org-export-read-attribute :attr_html block :collapsed)
                                   '("y" "yes" "t" t "true"))
                           (member (plist-get info :collapsed) '("all")))))
      (format
       "<details id='%s' class='code'%s>
<summary%s>%s</summary>
<div class='gutter'>\
<a href='#%s'>#</a>
<button title='Copy to clipboard' onclick='copyPreToClipbord(this)'>⎘</button>\
</div>
%s\n
</details>"
       ref
       (if (or collapsed-p collapsed-default) "" " open")
       (if type " class='named'" "")
       (if type (format "<span class='type'>%s</span>" type) "")
       ref
       (funcall orig-fn block contents info)))))

(advice-add 'org-html-example-block   :around #'org-html-block-collapsable)
(advice-add 'org-html-fixed-width     :around #'org-html-block-collapsable)
(advice-add 'org-html-property-drawer :around #'org-html-block-collapsable)
;; Example, fixed width, and property blocks ends here

;; [[file:config.org::*Org-Capture][Org-Capture:1]]
(use-package! doct
  :commands (doct))
;; Org-Capture:1 ends here

;; [[file:config.org::*Prettify][Prettify:1]]
(defun org-capture-select-template-prettier (&optional keys)
  "Select a capture template, in a prettier way than default
Lisp programs can force the template by setting KEYS to a string."
  (let ((org-capture-templates
         (or (org-contextualize-keys
              (org-capture-upgrade-templates org-capture-templates)
              org-capture-templates-contexts)
             '(("t" "Task" entry (file+headline "" "Tasks")
                "* TODO %?\n  %u\n  %a")))))
    (if keys
        (or (assoc keys org-capture-templates)
            (error "No capture template referred to by \"%s\" keys" keys))
      (org-mks org-capture-templates
               "Select a capture template\n━━━━━━━━━━━━━━━━━━━━━━━━━"
               "Template key: "
               `(("q" ,(concat (all-the-icons-octicon "stop" :face 'all-the-icons-red :v-adjust 0.01) "\tAbort")))))))
(advice-add 'org-capture-select-template :override #'org-capture-select-template-prettier)

(defun org-mks-pretty (table title &optional prompt specials)
  "Select a member of an alist with multiple keys. Prettified.

TABLE is the alist which should contain entries where the car is a string.
There should be two types of entries.

1. prefix descriptions like (\"a\" \"Description\")
   This indicates that `a' is a prefix key for multi-letter selection, and
   that there are entries following with keys like \"ab\", \"ax\"…

2. Select-able members must have more than two elements, with the first
   being the string of keys that lead to selecting it, and the second a
   short description string of the item.

The command will then make a temporary buffer listing all entries
that can be selected with a single key, and all the single key
prefixes.  When you press the key for a single-letter entry, it is selected.
When you press a prefix key, the commands (and maybe further prefixes)
under this key will be shown and offered for selection.

TITLE will be placed over the selection in the temporary buffer,
PROMPT will be used when prompting for a key.  SPECIALS is an
alist with (\"key\" \"description\") entries.  When one of these
is selected, only the bare key is returned."
  (save-window-excursion
    (let ((inhibit-quit t)
          (buffer (org-switch-to-buffer-other-window "*Org Select*"))
          (prompt (or prompt "Select: "))
          case-fold-search
          current)
      (unwind-protect
          (catch 'exit
            (while t
              (setq-local evil-normal-state-cursor (list nil))
              (erase-buffer)
              (insert title "\n\n")
              (let ((des-keys nil)
                    (allowed-keys '("\C-g"))
                    (tab-alternatives '("\s" "\t" "\r"))
                    (cursor-type nil))
                ;; Populate allowed keys and descriptions keys
                ;; available with CURRENT selector.
                (let ((re (format "\\`%s\\(.\\)\\'"
                                  (if current (regexp-quote current) "")))
                      (prefix (if current (concat current " ") "")))
                  (dolist (entry table)
                    (pcase entry
                      ;; Description.
                      (`(,(and key (pred (string-match re))) ,desc)
                       (let ((k (match-string 1 key)))
                         (push k des-keys)
                         ;; Keys ending in tab, space or RET are equivalent.
                         (if (member k tab-alternatives)
                             (push "\t" allowed-keys)
                           (push k allowed-keys))
                         (insert (propertize prefix 'face 'font-lock-comment-face) (propertize k 'face 'bold) (propertize "›" 'face 'font-lock-comment-face) "  " desc "…" "\n")))
                      ;; Usable entry.
                      (`(,(and key (pred (string-match re))) ,desc . ,_)
                       (let ((k (match-string 1 key)))
                         (insert (propertize prefix 'face 'font-lock-comment-face) (propertize k 'face 'bold) "   " desc "\n")
                         (push k allowed-keys)))
                      (_ nil))))
                ;; Insert special entries, if any.
                (when specials
                  (insert "─────────────────────────\n")
                  (pcase-dolist (`(,key ,description) specials)
                    (insert (format "%s   %s\n" (propertize key 'face '(bold all-the-icons-red)) description))
                    (push key allowed-keys)))
                ;; Display UI and let user select an entry or
                ;; a sub-level prefix.
                (goto-char (point-min))
                (unless (pos-visible-in-window-p (point-max))
                  (org-fit-window-to-buffer))
                (let ((pressed (org--mks-read-key allowed-keys prompt nil)))
                  (setq current (concat current pressed))
                  (cond
                   ((equal pressed "\C-g") (user-error "Abort"))
                   ((equal pressed "ESC") (user-error "Abort"))
                   ;; Selection is a prefix: open a new menu.
                   ((member pressed des-keys))
                   ;; Selection matches an association: return it.
                   ((let ((entry (assoc current table)))
                      (and entry (throw 'exit entry))))
                   ;; Selection matches a special entry: return the
                   ;; selection prefix.
                   ((assoc current specials) (throw 'exit current))
                   (t (error "No entry available")))))))
        (when buffer (kill-buffer buffer))))))
(advice-add 'org-mks :override #'org-mks-pretty)
;; Prettify:1 ends here

;; [[file:config.org::*Prettify][Prettify:2]]
(setf (alist-get 'height +org-capture-frame-parameters) 15)
;; (alist-get 'name +org-capture-frame-parameters) "❖ Capture") ;; ATM hardcoded in other places, so changing breaks stuff
(setq +org-capture-fn
      (lambda ()
        (interactive)
        (set-window-parameter nil 'mode-line-format 'none)
        (org-capture)))
;; Prettify:2 ends here

;; [[file:config.org::*Prettify][Prettify:3]]
(defun +doct-icon-declaration-to-icon (declaration)
  "Convert :icon declaration to icon"
  (let ((name (pop declaration))
        (set  (intern (concat "all-the-icons-" (plist-get declaration :set))))
        (face (intern (concat "all-the-icons-" (plist-get declaration :color))))
        (v-adjust (or (plist-get declaration :v-adjust) 0.01)))
    (apply set `(,name :face ,face :v-adjust ,v-adjust))))

(defun +doct-iconify-capture-templates (groups)
  "Add declaration's :icon to each template group in GROUPS."
  (let ((templates (doct-flatten-lists-in groups)))
    (setq doct-templates (mapcar (lambda (template)
                                   (when-let* ((props (nthcdr (if (= (length template) 4) 2 5) template))
                                               (spec (plist-get (plist-get props :doct) :icon)))
                                     (setf (nth 1 template) (concat (+doct-icon-declaration-to-icon spec)
                                                                    "\t"
                                                                    (nth 1 template))))
                                   template)
                                 templates))))

(setq doct-after-conversion-functions '(+doct-iconify-capture-templates))
;; Prettify:3 ends here

;; [[file:config.org::*Templates][Templates:1]]
(setq org-capture-templates
      (doct `(("Home" :keys "h"
               :icon ("home" :set "octicon" :color "cyan")
               :file "Home.org"
               :prepend t
               :headline "Inbox"
               :template ("* TODO %?"
                          "%i %a"))
              ("Work" :keys "w"
               :icon ("business" :set "material" :color "yellow")
               :file "Work.org"
               :prepend t
               :headline "Inbox"
               :template ("* TODO %?"
                          "SCHEDULED: %^{Schedule:}t"
                          "DEADLINE: %^{Deadline:}t"
                          "%i %a"))
              ("Note" :keys "n"
               :icon ("sticky-note" :set "faicon" :color "yellow")
               :file "Notes.org"
               :template ("* *?"
                          "%i %a"))
              ("Project" :keys "p"
               :icon ("repo" :set "octicon" :color "silver")
               :prepend t
               :type entry
               :headline "Inbox"
               :template ("* %{keyword} %?"
                          "%i"
                          "%a")
               :file ""
               :custom (:keyword "")
               :children (("Task" :keys "t"
                           :icon ("checklist" :set "octicon" :color "green")
                           :keyword "TODO"
                           :file +org-capture-project-todo-file)
                          ("Note" :keys "n"
                           :icon ("sticky-note" :set "faicon" :color "yellow")
                           :keyword "%U"
                           :file +org-capture-project-notes-file)))
              )))
;; Templates:1 ends here

;; [[file:config.org::*Conditional features][Conditional features:1]]
(defvar org-latex-italic-quotes t
  "Make \"quote\" environments italic.")
(defvar org-latex-par-sep t
  "Vertically seperate paragraphs, and remove indentation.")

(defvar org-latex-conditional-features
  '(("\\[\\[\\(?:file\\|https?\\):\\(?:[^]]\\|\\\\\\]\\)+?\\.\\(?:eps\\|pdf\\|png\\|jpeg\\|jpg\\|jbig2\\)\\]\\]" . image)
    ("\\[\\[\\(?:file\\|https?\\):\\(?:[^]]+?\\|\\\\\\]\\)\\.svg\\]\\]" . svg)
    ("^[ \t]*|" . table)
    ("cref:\\|\\cref{\\|\\[\\[[^\\]]+\\]\\]" . cleveref)
    ("[;\\\\]?\\b[A-Z][A-Z]+s?[^A-Za-z]" . acronym)
    ("\\+[^ ].*[^ ]\\+\\|_[^ ].*[^ ]_\\|\\\\uu?line\\|\\\\uwave\\|\\\\sout\\|\\\\xout\\|\\\\dashuline\\|\\dotuline\\|\\markoverwith" . underline)
    (":float wrap" . float-wrap)
    (":float sideways" . rotate)
    ("^[ \t]*#\\+caption:\\|\\\\caption" . caption)
    ("\\[\\[xkcd:" . (image caption))
    ((and org-latex-italic-quotes "^[ \t]*#\\+begin_quote\\|\\\\begin{quote}") . italic-quotes)
    (org-latex-par-sep . par-sep)
    ("^[ \t]*\\(?:[-+*]\\|[0-9]+[.)]\\|[A-Za-z]+[.)]\\) \\[[ -X]\\]" . checkbox)
    ("^[ \t]*#\\+begin_warning\\|\\\\begin{warning}" . box-warning)
    ("^[ \t]*#\\+begin_info\\|\\\\begin{info}"       . box-info)
    ("^[ \t]*#\\+begin_success\\|\\\\begin{success}" . box-success)
    ("^[ \t]*#\\+begin_error\\|\\\\begin{error}"     . box-error))
  "Org feature tests and associated LaTeX feature flags.

Alist where the car is a test for the presense of the feature,
and the cdr is either a single feature symbol or list of feature symbols.

When a string, it is used as a regex search in the buffer.
The feature is registered as present when there is a match.

The car can also be a
- symbol, the value of which is fetched
- function, which is called with info as an argument
- list, which is `eval'uated

If the symbol, function, or list produces a string: that is used as a regex
search in the buffer. Otherwise any non-nil return value will indicate the
existance of the feature.")
;; Conditional features:1 ends here

;; [[file:config.org::*Conditional features][Conditional features:2]]
(defvar org-latex-caption-preamble "
\\usepackage{subcaption}
\\usepackage[hypcap=true]{caption}
\\setkomafont{caption}{\\sffamily\\small}
\\setkomafont{captionlabel}{\\upshape\\bfseries}
\\captionsetup{justification=raggedright,singlelinecheck=true}
\\usepackage{capt-of} % required by Org
"
  "Preamble that improves captions.")

(defvar org-latex-checkbox-preamble "
\\newcommand{\\checkboxUnchecked}{$\\square$}
\\newcommand{\\checkboxTransitive}{\\rlap{\\raisebox{-0.1ex}{\\hspace{0.35ex}\\Large\\textbf -}}$\\square$}
\\newcommand{\\checkboxChecked}{\\rlap{\\raisebox{0.2ex}{\\hspace{0.35ex}\\scriptsize \\ding{52}}}$\\square$}
"
  "Preamble that improves checkboxes.")

(defvar org-latex-box-preamble "
% args = #1 Name, #2 Colour, #3 Ding, #4 Label
\\newcommand{\\defsimplebox}[4]{%
  \\definecolor{#1}{HTML}{#2}
  \\newenvironment{#1}[1][]
  {%
    \\par\\vspace{-0.7\\baselineskip}%
    \\textcolor{#1}{#3} \\textcolor{#1}{\\textbf{\\def\\temp{##1}\\ifx\\temp\\empty#4\\else##1\\fi}}%
    \\vspace{-0.8\\baselineskip}
    \\begin{addmargin}[1em]{1em}
  }{%
    \\end{addmargin}
    \\vspace{-0.5\\baselineskip}
  }%
}
"
  "Preamble that provides a macro for custom boxes.")
;; Conditional features:2 ends here

;; [[file:config.org::*Conditional features][Conditional features:3]]
(defvar org-latex-feature-implementations
  '((image         :snippet "\\usepackage{graphicx}" :order 2)
    (svg           :snippet "\\usepackage{svg}" :order 2)
    (table         :snippet "\\usepackage{longtable}\n\\usepackage{booktabs}" :order 2)
    (cleveref      :snippet "\\usepackage[capitalize]{cleveref}" :order 1)
    (underline     :snippet "\\usepackage[normalem]{ulem}" :order 0.5)
    (float-wrap    :snippet "\\usepackage{wrapfig}" :order 2)
    (rotate        :snippet "\\usepackage{rotating}" :order 2)
    (caption       :snippet org-latex-caption-preamble :order 2.1)
    (acronym       :snippet "\\newcommand{\\acr}[1]{\\protect\\textls*[110]{\\scshape #1}}\n\\newcommand{\\acrs}{\\protect\\scalebox{.91}[.84]\\hspace{0.15ex}s}" :order 0.4)
    (italic-quotes :snippet "\\renewcommand{\\quote}{\\list{}{\\rightmargin\\leftmargin}\\item\\relax\\em}\n" :order 0.5)
    (par-sep       :snippet "\\setlength{\\parskip}{\\baselineskip}\n\\setlength{\\parindent}{0pt}\n" :order 0.5)
    (.pifont       :snippet "\\usepackage{pifont}")
    (checkbox      :requires .pifont :order 3
                   :snippet (concat (unless (memq 'maths features)
                                      "\\usepackage{amssymb} % provides \\square")
                                    org-latex-checkbox-preamble))
    (.fancy-box    :requires .pifont    :snippet org-latex-box-preamble :order 3.9)
    (box-warning   :requires .fancy-box :snippet "\\defsimplebox{warning}{e66100}{\\ding{68}}{Warning}" :order 4)
    (box-info      :requires .fancy-box :snippet "\\defsimplebox{info}{3584e4}{\\ding{68}}{Information}" :order 4)
    (box-success   :requires .fancy-box :snippet "\\defsimplebox{success}{26a269}{\\ding{68}}{\\vspace{-\\baselineskip}}" :order 4)
    (box-error     :requires .fancy-box :snippet "\\defsimplebox{error}{c01c28}{\\ding{68}}{Important}" :order 4))
  "LaTeX features and details required to implement them.

List where the car is the feature symbol, and the rest forms a plist with the
following keys:
- :snippet, which may be either
  - a string which should be included in the preamble
  - a symbol, the value of which is included in the preamble
  - a function, which is evaluated with the list of feature flags as its
    single argument. The result of which is included in the preamble
  - a list, which is passed to `eval', with a list of feature flags available
    as \"features\"

- :requires, a feature or list of features that must be available
- :when, a feature or list of features that when all available should cause this
    to be automatically enabled.
- :prevents, a feature or list of features that should be masked
- :order, for when ordering is important. Lower values appear first.
    The default is 0.

Features that start with ! will be eagerly loaded, i.e. without being detected.")
;; Conditional features:3 ends here

;; [[file:config.org::*Conditional features][Conditional features:4]]
(defun org-latex-detect-features (&optional buffer info)
  "List features from `org-latex-conditional-features' detected in BUFFER."
  (let ((case-fold-search nil))
    (with-current-buffer (or buffer (current-buffer))
      (delete-dups
       (mapcan (lambda (construct-feature)
                 (when (let ((out (pcase (car construct-feature)
                                    ((pred stringp) (car construct-feature))
                                    ((pred functionp) (funcall (car construct-feature) info))
                                    ((pred listp) (eval (car construct-feature)))
                                    ((pred symbolp) (symbol-value (car construct-feature)))
                                    (_ (user-error "org-latex-conditional-features key %s unable to be used" (car construct-feature))))))
                         (if (stringp out)
                             (save-excursion
                               (goto-char (point-min))
                               (re-search-forward out nil t))
                           out))
                   (if (listp (cdr construct-feature)) (cdr construct-feature) (list (cdr construct-feature)))))
               org-latex-conditional-features)))))
;; Conditional features:4 ends here

;; [[file:config.org::*Conditional features][Conditional features:5]]
(defun org-latex-expand-features (features)
  "For each feature in FEATURES process :requires, :when, and :prevents keywords and sort according to :order."
  (dolist (feature features)
    (unless (assoc feature org-latex-feature-implementations)
      (error "Feature %s not provided in org-latex-feature-implementations" feature)))
  (setq current features)
  (while current
    (when-let ((requirements (plist-get (cdr (assq (car current) org-latex-feature-implementations)) :requires)))
      (setcdr current (if (listp requirements)
                          (append requirements (cdr current))
                        (cons requirements (cdr current)))))
    (setq current (cdr current)))
  (dolist (potential-feature
           (append features (delq nil (mapcar (lambda (feat)
                                                (when (plist-get (cdr feat) :eager)
                                                  (car feat)))
                                              org-latex-feature-implementations))))
    (when-let ((prerequisites (plist-get (cdr (assoc potential-feature org-latex-feature-implementations)) :when)))
      (setf features (if (if (listp prerequisites)
                             (cl-every (lambda (preq) (memq preq features)) prerequisites)
                           (memq prerequisites features))
                         (append (list potential-feature) features)
                       (delq potential-feature features)))))
  (dolist (feature features)
    (when-let ((prevents (plist-get (cdr (assoc feature org-latex-feature-implementations)) :prevents)))
      (setf features (cl-set-difference features (if (listp prevents) prevents (list prevents))))))
  (sort (delete-dups features)
        (lambda (feat1 feat2)
          (if (< (or (plist-get (cdr (assoc feat1 org-latex-feature-implementations)) :order) 1)
                 (or (plist-get (cdr (assoc feat2 org-latex-feature-implementations)) :order) 1))
              t nil))))
;; Conditional features:5 ends here

;; [[file:config.org::*Conditional features][Conditional features:6]]
(defun org-latex-generate-features-preamble (features)
  "Generate the LaTeX preamble content required to provide FEATURES.
This is done according to `org-latex-feature-implementations'"
  (let ((expanded-features (org-latex-expand-features features)))
    (concat
     (format "\n%% features: %s\n" expanded-features)
     (mapconcat (lambda (feature)
                  (when-let ((snippet (plist-get (cdr (assoc feature org-latex-feature-implementations)) :snippet)))
                    (concat
                     (pcase snippet
                       ((pred stringp) snippet)
                       ((pred functionp) (funcall snippet features))
                       ((pred listp) (eval `(let ((features ',features)) (,@snippet))))
                       ((pred symbolp) (symbol-value snippet))
                       (_ (user-error "org-latex-feature-implementations :snippet value %s unable to be used" snippet)))
                     "\n")))
                expanded-features
                "")
     "% end features\n")))
;; Conditional features:6 ends here

;; [[file:config.org::*Conditional features][Conditional features:7]]
(defvar info--tmp nil)

(defadvice! org-latex-save-info (info &optional t_ s_)
  :before #'org-latex-make-preamble
  (setq info--tmp info))

(defadvice! org-splice-latex-header-and-generated-preamble-a (orig-fn tpl def-pkg pkg snippets-p &optional extra)
  "Dynamically insert preamble content based on `org-latex-conditional-preambles'."
  :around #'org-splice-latex-header
  (let ((header (funcall orig-fn tpl def-pkg pkg snippets-p extra)))
    (if snippets-p header
      (concat header
              (org-latex-generate-features-preamble (org-latex-detect-features nil info--tmp))
              "\n"))))
;; Conditional features:7 ends here

;; [[file:config.org::*Tectonic][Tectonic:1]]
(setq org-latex-pdf-process '("tectonic -X compile --print --outdir=%o -Z shell-escape %f"))
;; Tectonic:1 ends here

;; [[file:config.org::*Tectonic][Tectonic:2]]
(setq org-preview-latex-process-alist
'((dvipng :programs
                  ("tectonic" "dvipng")
                  :description "dvi > png" :message "you need to install the programs: tectonic and dvipng." :image-input-type "dvi" :image-output-type "png" :image-size-adjust
                  (1.0 . 1.0)
                  :latex-compiler
                  ;; tectonic doesn't have a non interactive mode
                  ("tectonic --outdir %o %f")
                  :image-converter
                  ("dvipng -D %D -T tight -bg Transparent -o %O %f"))
          (dvisvgm :programs
                   ("tectonic" "dvisvgm")
                   :description "dvi > svg" :message "you need to install the programs: tectonic and dvisvgm." :image-input-type "dvi" :image-output-type "svg" :image-size-adjust
                   (1.7 . 1.5)
                   :latex-compiler
                   ("tectonic --outdir %o %f")
                   :image-converter
                   ("dvisvgm %f -n -b min -c %S -o %O"))
          (imagemagick :programs
                       ("latex" "convert")
                       :description "pdf > png" :message "you need to install the programs: latex and imagemagick." :image-input-type "pdf" :image-output-type "png" :image-size-adjust
                       (1.0 . 1.0)
                       :latex-compiler
                       ("tectonic --outdir %o %f")
                       :image-converter
                       ("convert -density %D -trim -antialias %f -quality 100 %O"))))
;; Tectonic:2 ends here

;; [[file:config.org::*Classes][Classes:1]]
(after! ox-latex
  (add-to-list 'org-latex-classes
               '("cb-doc" "\\documentclass{scrartcl}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))
;; Classes:1 ends here

;; [[file:config.org::*Classes][Classes:2]]
(after! ox-latex
  (setq org-latex-default-class "cb-doc"
        org-latex-tables-booktabs t
        org-latex-hyperref-template "\\colorlet{greenyblue}{blue!70!green}
\\colorlet{blueygreen}{blue!40!green}
\\providecolor{link}{named}{greenyblue}
\\providecolor{cite}{named}{blueygreen}
\\hypersetup{
  pdfauthor={%a},
  pdftitle={%t},
  pdfkeywords={%k},
  pdfsubject={%d},
  pdfcreator={%c},
  pdflang={%L},
  breaklinks=true,
  colorlinks=true,
  linkcolor=,
  urlcolor=link,
  citecolor=cite\n}
\\urlstyle{same}
"
        org-latex-reference-command "\\cref{%s}"))
;; Classes:2 ends here

;; [[file:config.org::*Packages][Packages:1]]
(setq org-latex-default-packages-alist
      `(("AUTO" "inputenc" t
         ("pdflatex"))
        ("T1" "fontenc" t
         ("pdflatex"))
        ("" "fontspec" t)
        ("" "graphicx" t)
        ("" "grffile" t)
        ("" "longtable" nil)
        ("" "wrapfig" nil)
        ("" "rotating" nil)
        ("normalem" "ulem" t)
        ("" "amsmath" t)
        ("" "textcomp" t)
        ("" "amssymb" t)
        ("" "capt-of" nil)
        ("dvipsnames" "xcolor" nil)
        ("colorlinks=true, linkcolor=Blue, citecolor=BrickRed, urlcolor=PineGreen" "hyperref" nil)
    ("" "indentfirst" nil)
    "\\setmainfont[Ligatures=TeX]{Alegreya}"
    "\\setmonofont[Ligatures=TeX]{Liga SFMono Nerd Font}"))
;; Packages:1 ends here

;; [[file:config.org::*Pretty code blocks][Pretty code blocks:1]]
(use-package! engrave-faces-latex
  :after ox-latex
  :config
  (setq org-latex-listings 'engraved))
;; Pretty code blocks:1 ends here

;; [[file:config.org::*Pretty code blocks][Pretty code blocks:2]]
(defadvice! org-latex-src-block-engraved (orig-fn src-block contents info)
  "Like `org-latex-src-block', but supporting an engraved backend"
  :around #'org-latex-src-block
  (if (eq 'engraved (plist-get info :latex-listings))
      (org-latex-scr-block--engraved src-block contents info)
    (funcall orig-fn src-block contents info)))

(defadvice! org-latex-inline-src-block-engraved (orig-fn inline-src-block contents info)
  "Like `org-latex-inline-src-block', but supporting an engraved backend"
  :around #'org-latex-inline-src-block
  (if (eq 'engraved (plist-get info :latex-listings))
      (org-latex-inline-scr-block--engraved inline-src-block contents info)
    (funcall orig-fn src-block contents info)))

(defvar-local org-export-has-code-p nil)

(defadvice! org-export-expect-no-code (&rest _)
  :before #'org-export-as
  (setq org-export-has-code-p nil))

(defadvice! org-export-register-code (&rest _)
  :after #'org-latex-src-block-engraved
  :after #'org-latex-inline-src-block-engraved
  (setq org-export-has-code-p t))

(setq org-latex-engraved-code-preamble "
\\usepackage{fvextra}
\\fvset{
  commandchars=\\\\\\{\\},
  highlightcolor=white!95!black!80!blue,
  breaklines=true,
  breaksymbol=\\color{white!60!black}\\tiny\\ensuremath{\\hookrightarrow}}
\\renewcommand\\theFancyVerbLine{\\footnotesize\\color{black!40!white}\\arabic{FancyVerbLine}}

\\definecolor{codebackground}{HTML}{f7f7f7}
\\definecolor{codeborder}{HTML}{f0f0f0}

% TODO have code boxes keep line vertical alignment
\\usepackage[breakable,xparse]{tcolorbox}
\\DeclareTColorBox[]{Code}{o}%
{colback=codebackground, colframe=codeborder,
  fontupper=\\footnotesize,
  colupper=EFD,
  IfNoValueTF={#1}%
  {boxsep=2pt, arc=2.5pt, outer arc=2.5pt,
    boxrule=0.5pt, left=2pt}%
  {boxsep=2.5pt, arc=0pt, outer arc=0pt,
    boxrule=0pt, leftrule=1.5pt, left=0.5pt},
  right=2pt, top=1pt, bottom=0.5pt,
  breakable}
")

(add-to-list 'org-latex-conditional-features '((and org-export-has-code-p "^[ \t]*#\\+begin_src\\|^[ \t]*#\\+BEGIN_SRC\\|src_[A-Za-z]") . engraved-code) t)
(add-to-list 'org-latex-conditional-features '("^[ \t]*#\\+begin_example\\|^[ \t]*#\\+BEGIN_EXAMPLE" . engraved-code-setup) t)
(add-to-list 'org-latex-feature-implementations '(engraved-code :requires engraved-code-setup :snippet (engrave-faces-latex-gen-preamble) :order 99) t)
(add-to-list 'org-latex-feature-implementations '(engraved-code-setup :snippet org-latex-engraved-code-preamble :order 98) t)

(defun org-latex-scr-block--engraved (src-block contents info)
  (let* ((lang (org-element-property :language src-block))
         (attributes (org-export-read-attribute :attr_latex src-block))
         (float (plist-get attributes :float))
         (num-start (org-export-get-loc src-block info))
         (retain-labels (org-element-property :retain-labels src-block))
         (caption (org-element-property :caption src-block))
         (caption-above-p (org-latex--caption-above-p src-block info))
         (caption-str (org-latex--caption/label-string src-block info))
         (placement (or (org-unbracket-string "[" "]" (plist-get attributes :placement))
                        (plist-get info :latex-default-figure-position)))
         (float-env
          (cond
           ((string= "multicolumn" float)
            (format "\\begin{listing*}[%s]\n%s%%s\n%s\\end{listing*}"
                    placement
                    (if caption-above-p caption-str "")
                    (if caption-above-p "" caption-str)))
           (caption
            (format "\\begin{listing}[%s]\n%s%%s\n%s\\end{listing}"
                    placement
                    (if caption-above-p caption-str "")
                    (if caption-above-p "" caption-str)))
           ((string= "t" float)
            (concat (format "\\begin{listing}[%s]\n"
                            placement)
                    "%s\n\\end{listing}"))
           (t "%s")))
         (options (plist-get info :latex-minted-options))
         (content-buffer
          (with-temp-buffer
            (insert
             (let* ((code-info (org-export-unravel-code src-block))
                    (max-width
                     (apply 'max
                            (mapcar 'length
                                    (org-split-string (car code-info)
                                                      "\n")))))
               (org-export-format-code
                (car code-info)
                (lambda (loc _num ref)
                  (concat
                   loc
                   (when ref
                     ;; Ensure references are flushed to the right,
                     ;; separated with 6 spaces from the widest line
                     ;; of code.
                     (concat (make-string (+ (- max-width (length loc)) 6)
                                          ?\s)
                             (format "(%s)" ref)))))
                nil (and retain-labels (cdr code-info)))))
            (funcall (org-src-get-lang-mode lang))
            (engrave-faces-latex-buffer)))
         (content
          (with-current-buffer content-buffer
            (buffer-string)))
         (body
          (format
           "\\begin{Code}\n\\begin{Verbatim}[%s]\n%s\\end{Verbatim}\n\\end{Code}"
           ;; Options.
           (concat
            (org-latex--make-option-string
             (if (or (not num-start) (assoc "linenos" options))
                 options
               (append
                `(("linenos")
                  ("firstnumber" ,(number-to-string (1+ num-start))))
                options)))
            (let ((local-options (plist-get attributes :options)))
              (and local-options (concat "," local-options))))
           content)))
    (kill-buffer content-buffer)
    ;; Return value.
    (format float-env body)))

(defun org-latex-inline-scr-block--engraved (inline-src-block _contents info)
  (let ((options (org-latex--make-option-string
                  (plist-get info :latex-minted-options)))
        code-buffer code)
    (setq code-buffer
          (with-temp-buffer
            (insert (org-element-property :value inline-src-block))
            (funcall (org-src-get-lang-mode
                      (org-element-property :language inline-src-block)))
            (engrave-faces-latex-buffer)))
    (setq code (with-current-buffer code-buffer
                 (buffer-string)))
    (kill-buffer code-buffer)
    (format "\\Verb%s{%s}"
            (if (string= options "") ""
              (format "[%s]" options))
            code)))

(defadvice! org-latex-example-block-engraved (orig-fn example-block contents info)
  "Like `org-latex-example-block', but supporting an engraved backend"
  :around #'org-latex-example-block
  (let ((output-block (funcall orig-fn example-block contents info)))
    (if (eq 'engraved (plist-get info :latex-listings))
        (format "\\begin{Code}[alt]\n%s\n\\end{Code}" output-block)
      output-block)))
;; Pretty code blocks:2 ends here

;; [[file:config.org::*ox-chameleon][ox-chameleon:1]]
(use-package! ox-chameleon
  :after ox)
;; ox-chameleon:1 ends here

;; [[file:config.org::*Async][Async:1]]
(setq org-export-in-background t)
;; Async:1 ends here

;; [[file:config.org::*(sub|super)script characters][(sub|super)script characters:1]]
(setq org-export-with-sub-superscripts '{})
;; (sub|super)script characters:1 ends here

;; [[file:config.org::*Mu4e][Mu4e:1]]
(setq mu4e-update-interval 300)
;; Mu4e:1 ends here

;; [[file:config.org::*Mu4e][Mu4e:3]]
(after! mu4e
  (setq sendmail-program "~/.nix-profile/bin/msmtp"
        send-mail-function #'smtpmail-send-it
        message-sendmail-f-is-evil t
        message-sendmail-extra-arguments '("--read-envelope-from")
        message-send-mail-function #'message-send-mail-with-sendmail))
;; Mu4e:3 ends here

;; [[file:config.org::*Mu4e][Mu4e:4]]
;;(setq alert-default-style 'osx-notifier)
;; Mu4e:4 ends here

;; [[file:config.org::*Webkit][Webkit:1]]
;;(use-package org
;;  :demand t)

(use-package webkit
  :defer t
  :commands webkit
  :init
  (setq webkit-search-prefix "https://google.com/search?q="
        webkit-history-file nil
        webkit-cookie-file nil
        browse-url-browser-function 'webkit-browse-url
        webkit-browse-url-force-new t
        webkit-download-action-alist '(("\\.pdf\\'" . webkit-download-open)
                                       ("\\.png\\'" . webkit-download-save)
                                       (".*" . webkit-download-default)))

  (defun webkit--display-progress (progress)
    (setq webkit--progress-formatted
          (if (equal progress 100.0)
              ""
            (format "%s%.0f%%  " (all-the-icons-faicon "spinner") progress)))
    (force-mode-line-update)))
;; Webkit:1 ends here

;; [[file:config.org::*IRC][IRC:2]]
(after! circe
  (setq-default circe-use-tls t)
  (setq circe-notifications-alert-icon "/usr/share/icons/breeze/actions/24/network-connect.svg"
        lui-logging-directory "~/.emacs.d/.local/etc/irc"
        lui-logging-file-format "{buffer}/%Y/%m-%d.txt"
        circe-format-self-say "{nick:+13s} ┃ {body}")

  (custom-set-faces!
    '(circe-my-message-face :weight unspecified))

  (enable-lui-logging-globally)
  (enable-circe-display-images)

  (defun lui-org-to-irc ()
    "Examine a buffer with simple org-mode formatting, and converts the empasis:
  *bold*, /italic/, and _underline_ to IRC semi-standard escape codes.
  =code= is converted to inverse (highlighted) text."
    (goto-char (point-min))
    (while (re-search-forward "\\_<\\(?1:[*/_=]\\)\\(?2:[^[:space:]]\\(?:.*?[^[:space:]]\\)?\\)\\1\\_>" nil t)
      (replace-match
       (concat (pcase (match-string 1)
                 ("*" "")
                 ("/" "")
                 ("_" "")
                 ("=" ""))
               (match-string 2)
               "") nil nil)))
  
  (add-hook 'lui-pre-input-hook #'lui-org-to-irc)

  (defun lui-ascii-to-emoji ()
    (goto-char (point-min))
    (while (re-search-forward "\\( \\)?::?\\([^[:space:]:]+\\):\\( \\)?" nil t)
      (replace-match
       (concat
        (match-string 1)
        (or (cdr (assoc (match-string 2) lui-emojis-alist))
            (concat ":" (match-string 2) ":"))
        (match-string 3))
       nil nil)))
  
  (defun lui-emoticon-to-emoji ()
    (dolist (emoticon lui-emoticons-alist)
      (goto-char (point-min))
      (while (re-search-forward (concat " " (car emoticon) "\\( \\)?") nil t)
        (replace-match (concat " "
                               (cdr (assoc (cdr emoticon) lui-emojis-alist))
                               (match-string 1))))))
  
  (define-minor-mode lui-emojify
    "Replace :emojis: and ;) emoticons with unicode emoji chars."
    :global t
    :init-value t
    (if lui-emojify
        (add-hook! lui-pre-input #'lui-ascii-to-emoji #'lui-emoticon-to-emoji)
      (remove-hook! lui-pre-input #'lui-ascii-to-emoji #'lui-emoticon-to-emoji)))
  (defvar lui-emojis-alist
    '(("grinning"                      . "😀")
      ("smiley"                        . "😃")
      ("smile"                         . "😄")
      ("grin"                          . "😁")
      ("laughing"                      . "😆")
      ("sweat_smile"                   . "😅")
      ("joy"                           . "😂")
      ("rofl"                          . "🤣")
      ("relaxed"                       . "☺️")
      ("blush"                         . "😊")
      ("innocent"                      . "😇")
      ("slight_smile"                  . "🙂")
      ("upside_down"                   . "🙃")
      ("wink"                          . "😉")
      ("relieved"                      . "😌")
      ("heart_eyes"                    . "😍")
      ("yum"                           . "😋")
      ("stuck_out_tongue"              . "😛")
      ("stuck_out_tongue_closed_eyes"  . "😝")
      ("stuck_out_tongue_wink"         . "😜")
      ("zanzy"                         . "🤪")
      ("raised_eyebrow"                . "🤨")
      ("monocle"                       . "🧐")
      ("nerd"                          . "🤓")
      ("cool"                          . "😎")
      ("star_struck"                   . "🤩")
      ("party"                         . "🥳")
      ("smirk"                         . "😏")
      ("unamused"                      . "😒")
      ("disapointed"                   . "😞")
      ("pensive"                       . "😔")
      ("worried"                       . "😟")
      ("confused"                      . "😕")
      ("slight_frown"                  . "🙁")
      ("frown"                         . "☹️")
      ("persevere"                     . "😣")
      ("confounded"                    . "😖")
      ("tired"                         . "😫")
      ("weary"                         . "😩")
      ("pleading"                      . "🥺")
      ("tear"                          . "😢")
      ("cry"                           . "😢")
      ("sob"                           . "😭")
      ("triumph"                       . "😤")
      ("angry"                         . "😠")
      ("rage"                          . "😡")
      ("exploding_head"                . "🤯")
      ("flushed"                       . "😳")
      ("hot"                           . "🥵")
      ("cold"                          . "🥶")
      ("scream"                        . "😱")
      ("fearful"                       . "😨")
      ("disapointed"                   . "😰")
      ("relieved"                      . "😥")
      ("sweat"                         . "😓")
      ("thinking"                      . "🤔")
      ("shush"                         . "🤫")
      ("liar"                          . "🤥")
      ("blank_face"                    . "😶")
      ("neutral"                       . "😐")
      ("expressionless"                . "😑")
      ("grimace"                       . "😬")
      ("rolling_eyes"                  . "🙄")
      ("hushed"                        . "😯")
      ("frowning"                      . "😦")
      ("anguished"                     . "😧")
      ("wow"                           . "😮")
      ("astonished"                    . "😲")
      ("sleeping"                      . "😴")
      ("drooling"                      . "🤤")
      ("sleepy"                        . "😪")
      ("dizzy"                         . "😵")
      ("zipper_mouth"                  . "🤐")
      ("woozy"                         . "🥴")
      ("sick"                          . "🤢")
      ("vomiting"                      . "🤮")
      ("sneeze"                        . "🤧")
      ("mask"                          . "😷")
      ("bandaged_head"                 . "🤕")
      ("money_face"                    . "🤑")
      ("cowboy"                        . "🤠")
      ("imp"                           . "😈")
      ("ghost"                         . "👻")
      ("alien"                         . "👽")
      ("robot"                         . "🤖")
      ("clap"                          . "👏")
      ("thumpup"                       . "👍")
      ("+1"                            . "👍")
      ("thumbdown"                     . "👎")
      ("-1"                            . "👎")
      ("ok"                            . "👌")
      ("pinch"                         . "🤏")
      ("left"                          . "👈")
      ("right"                         . "👉")
      ("down"                          . "👇")
      ("wave"                          . "👋")
      ("pray"                          . "🙏")
      ("eyes"                          . "👀")
      ("brain"                         . "🧠")
      ("facepalm"                      . "🤦")
      ("tada"                          . "🎉")
      ("fire"                          . "🔥")
      ("flying_money"                  . "💸")
      ("lighbulb"                      . "💡")
      ("heart"                         . "❤️")
      ("sparkling_heart"               . "💖")
      ("heartbreak"                    . "💔")
      ("100"                           . "💯")))
  
  (defvar lui-emoticons-alist
    '((":)"   . "slight_smile")
      (";)"   . "wink")
      (":D"   . "smile")
      ("=D"   . "grin")
      ("xD"   . "laughing")
      (";("   . "joy")
      (":P"   . "stuck_out_tongue")
      (";D"   . "stuck_out_tongue_wink")
      ("xP"   . "stuck_out_tongue_closed_eyes")
      (":("   . "slight_frown")
      (";("   . "cry")
      (";'("  . "sob")
      (">:("  . "angry")
      (">>:(" . "rage")
      (":o"   . "wow")
      (":O"   . "astonished")
      (":/"   . "confused")
      (":-/"  . "thinking")
      (":|"   . "neutral")
      (":-|"  . "expressionless")))

  (defun named-circe-prompt ()
    (lui-set-prompt
     (concat (propertize (format "%13s > " (circe-nick))
                         'face 'circe-prompt-face)
             "")))
  (add-hook 'circe-chat-mode-hook #'named-circe-prompt)

  (appendq! all-the-icons-mode-icon-alist
            '((circe-channel-mode all-the-icons-material "message" :face all-the-icons-lblue)
              (circe-server-mode all-the-icons-material "chat_bubble_outline" :face all-the-icons-purple))))

(defun auth-server-pass (server)
  (if-let ((secret (plist-get (car (auth-source-search :host server)) :secret)))
      (if (functionp secret)
          (funcall secret) secret)
    (error "Could not fetch password for host %s" server)))

(defun register-irc-auths ()
  (require 'circe)
  (require 'dash)
  (let ((accounts (-filter (lambda (a) (string= "irc" (plist-get a :for)))
                           (auth-source-search :require '(:for) :max 10))))
    (appendq! circe-network-options
              (mapcar (lambda (entry)
                        (let* ((host (plist-get entry :host))
                               (label (or (plist-get entry :label) host))
                               (_ports (mapcar #'string-to-number
                                               (s-split "," (plist-get entry :port))))
                               (port (if (= 1 (length _ports)) (car _ports) _ports))
                               (user (plist-get entry :user))
                               (nick (or (plist-get entry :nick) user))
                               (channels (mapcar (lambda (c) (concat "#" c))
                                                 (s-split "," (plist-get entry :channels)))))
                          `(,label
                            :host ,host :port ,port :nick ,nick
                            :sasl-username ,user :sasl-password auth-server-pass
                            :channels ,channels)))
                      accounts))))

(add-transient-hook! #'=irc (register-irc-auths))
;; IRC:2 ends here

;; [[file:config.org::org-emph-to-irc][org-emph-to-irc]]
(defun lui-org-to-irc ()
  "Examine a buffer with simple org-mode formatting, and converts the empasis:
*bold*, /italic/, and _underline_ to IRC semi-standard escape codes.
=code= is converted to inverse (highlighted) text."
  (goto-char (point-min))
  (while (re-search-forward "\\_<\\(?1:[*/_=]\\)\\(?2:[^[:space:]]\\(?:.*?[^[:space:]]\\)?\\)\\1\\_>" nil t)
    (replace-match
     (concat (pcase (match-string 1)
               ("*" "")
               ("/" "")
               ("_" "")
               ("=" ""))
             (match-string 2)
             "") nil nil)))

(add-hook 'lui-pre-input-hook #'lui-org-to-irc)
;; org-emph-to-irc ends here

;; [[file:config.org::*Nyxt][Nyxt:1]]
(defcustom cl-ide 'sly
  "What IDE to use to evaluate Common Lisp.
Defaults to Sly because it has better integration with Nyxt."
  :options (list 'sly 'slime))

(defvar emacs-with-nyxt-delay
  0.1
  "Delay to wait for `cl-ide' commands to reach Nyxt.")

(setq slime-protocol-version 'ignore)

(defun emacs-with-nyxt-connected-p ()
  "Is `cl-ide' connected to nyxt."
  (cond
   ((eq cl-ide 'slime) (slime-connected-p))
   ((eq cl-ide 'sly) (sly-connected-p)))) ;; TODO this should check it
                                          ;; is connected to Nyxt and
                                          ;; not just to cl-ide
                                          ;; session

(defun emacs-with-nyxt--connect (host port)
  "Connect `cl-ide' to HOST and PORT."
  (cond
   ((eq cl-ide 'slime) (slime-connect host port))
   ((eq cl-ide 'sly) (sly-connect host port))))

(defun emacs-with-nyxt-connect (host port)
  "Connect `cl-ide' to HOST and PORT ignoring version mismatches."
  (emacs-with-nyxt--connect host port)
  (while (not (emacs-with-nyxt-connected-p))
    (message "Starting %s connection..." cl-ide)
    (sleep-for emacs-with-nyxt-delay)))

(defun emacs-with-nyxt-eval (string)
  "Send STRING to `cl-ide'."
  (cond
   ((eq cl-ide 'slime) (slime-repl-eval-string string))
   ((eq cl-ide 'sly) (sly-eval `(slynk:interactive-eval-region ,string)))))

(defun emacs-with-nyxt-send-sexps (&rest s-exps)
  "Evaluate S-EXPS with Nyxt `cl-ide' session."
  (let ((s-exps-string (s-join "" (--map (prin1-to-string it) s-exps))))
    (defun true (&rest args) 't)
    (if (emacs-with-nyxt-connected-p)
        (emacs-with-nyxt-eval s-exps-string)
      (error (format "%s is not connected to Nyxt. Run `emacs-with-nyxt-start-and-connect-to-nyxt' first" cl-ide)))))

(add-to-list
 'org-capture-templates
 `("wN" "Web link" entry (file+headline ,(car org-agenda-files) "Links to read later")
   "* TODO %?%a :readings: \nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"Fri\"))\n"
   :immediate-finish t :empty-lines 2))

(defun on/slug-string (title)  (let ((slug-trim-chars '(;; Combining Diacritical Marks https://www.unicode.org/charts/PDF/U0300.pdf
                                                        768 ; U+0300 COMBINING GRAVE ACCENT
                                                        769 ; U+0301 COMBINING ACUTE ACCENT
                                                        770 ; U+0302 COMBINING CIRCUMFLEX ACCENT
                                                        771 ; U+0303 COMBINING TILDE
                                                        772 ; U+0304 COMBINING MACRON
                                                        774 ; U+0306 COMBINING BREVE
                                                        775 ; U+0307 COMBINING DOT ABOVE
                                                        776 ; U+0308 COMBINING DIAERESIS
                                                        777 ; U+0309 COMBINING HOOK ABOVE
                                                        778 ; U+030A COMBINING RING ABOVE
                                                        780 ; U+030C COMBINING CARON
                                                        795 ; U+031B COMBINING HORN
                                                        803 ; U+0323 COMBINING DOT BELOW
                                                        804 ; U+0324 COMBINING DIAERESIS BELOW
                                                        805 ; U+0325 COMBINING RING BELOW
                                                        807 ; U+0327 COMBINING CEDILLA
                                                        813 ; U+032D COMBINING CIRCUMFLEX ACCENT BELOW
                                                        814 ; U+032E COMBINING BREVE BELOW
                                                        816 ; U+0330 COMBINING TILDE BELOW
                                                        817 ; U+0331 COMBINING MACRON BELOW
                                                        )))
                                 (cl-flet* ((nonspacing-mark-p (char)
                                                               (memq char slug-trim-chars))
                                            (strip-nonspacing-marks (s)
                                                                    (ucs-normalize-NFC-string
                                                                     (apply #'string (seq-remove #'nonspacing-mark-p
                                                                                                 (ucs-normalize-NFD-string s)))))
                                            (cl-replace (title pair)
                                                        (replace-regexp-in-string (car pair) (cdr pair) title)))
                                   (let* ((pairs `(("[^[:alnum:][:digit:]]" . "_") ;; convert anything not alphanumeric
                                                   ("__*" . "_") ;; remove sequential underscores
                                                   ("^_" . "") ;; remove starting underscore
                                                   ("_$" . ""))) ;; remove ending underscore
                                          (slug (-reduce-from #'cl-replace (strip-nonspacing-marks title) pairs)))
                                     (downcase slug)))))

(defun on/make-filepath (title now &optional zone)
  "Make filename from note TITLE and NOW time (assumed in the current time ZONE)."
  (concat
   org-roam-directory
   (format-time-string "%Y%m%d%H%M%S_" now (or zone (current-time-zone)))
   (s-truncate 70 (on/slug-string title) "")
   ".org"))

(defun on/insert-org-roam-file (file-path title &optional links sources text quote)
  "Insert org roam file in FILE-PATH with TITLE, LINKS, SOURCES, TEXT, QUOTE."
  (with-temp-file file-path
    (insert
     "* " title "\n"
     "\n"
     "- tags :: " (--reduce (concat acc ", " it) links) "\n"
     (if sources (concat "- source :: " (--reduce (concat acc ", " it) sources) "\n") "")
     "\n"
     (if text text "")
     "\n"
     "\n"
     (if quote
         (concat "#+begin_src text \n"
                 quote "\n"
                 "#+end_src")
       "")))
  (with-file file-path
             (org-id-get-create)
             (save-buffer)))

(defun emacs-with-nyxt-current-package ()
  "Return current package set for `cl-ide'."
  (cond
   ((eq cl-ide 'slime) (slime-current-package))
   ((eq cl-ide 'sly) (with-current-buffer (sly-mrepl--find-buffer) (sly-current-package)))))

(defun emacs-with-nyxt-start-and-connect-to-nyxt (&optional no-maximize)
  "Start Nyxt with swank capabilities. Optionally skip window maximization with NO-MAXIMIZE."
  (interactive)
  (async-shell-command (format "nyxt -e \"(nyxt-user::start-swank)\""))
  (while (not (ignore-errors (not (emacs-with-nyxt-connect "localhost" "4006"))))
    (message "Starting Swank connection...")
    (sleep-for emacs-with-nyxt-delay))
  (while (not (ignore-errors (string= "NYXT-USER" (upcase (emacs-with-nyxt-current-package)))))
    (progn (message "Setting %s package to NYXT-USER..." cl-ide)
           (sleep-for emacs-with-nyxt-delay)))
  (emacs-with-nyxt-send-sexps
   `(load "~/quicklisp/setup.lisp")
   `(defun replace-all (string part replacement &key (test #'char=))
      "Return a new string in which all the occurences of the part is replaced with replacement."
      (with-output-to-string (out)
                             (loop with part-length = (length part)
                                   for old-pos = 0 then (+ pos part-length)
                                   for pos = (search part string
                                                     :start2 old-pos
                                                     :test test)
                                   do (write-string string out
                                                    :start old-pos
                                                    :end (or pos (length string)))
                                   when pos do (write-string replacement out)
                                   while pos)))

   `(defun eval-in-emacs (&rest s-exps)
      "Evaluate S-EXPS with emacsclient."
      (let ((s-exps-string (replace-all
                            (write-to-string
                             `(progn ,@s-exps) :case :downcase)
                            ;; Discard the package prefix.
                            "nyxt::" "")))
        (format *error-output* "Sending to Emacs:~%~a~%" s-exps-string)
        (uiop:run-program
         (list "emacsclient" "--eval" s-exps-string))))
   `(ql:quickload "cl-qrencode")
   `(define-command-global my/make-current-url-qr-code () ; this is going to be redundant: https://nyxt.atlas.engineer/article/qr-url.org
      "Something else."
      (when (equal (mode-name (current-buffer)) 'web-buffer))
      (progn
        (cl-qrencode:encode-png (quri:render-uri (url (current-buffer))) :fpath "/tmp/qrcode.png")
        (uiop:run-program (list "nyxt" "/tmp/qrcode.png"))))
   '(define-command-global my/open-html-in-emacs ()
      "Open buffer html in Emacs."
      (when (equal (mode-name (current-buffer)) 'web-buffer))
      (with-open-file
       (file "/tmp/temp-nyxt.html" :direction :output
             :if-exists :supersede
             :if-does-not-exist :create)
       (write-string (ffi-buffer-get-document (current-buffer)) file))
      (eval-in-emacs
       `(progn (switch-to-buffer
                (get-buffer-create ,(render-url (url (current-buffer)))))
               (erase-buffer)
               (insert-file-contents-literally "/tmp/temp-nyxt.html")
               (html-mode)
               (indent-region (point-min) (point-max))))
      (delete-file "/tmp/temp-nyxt.html"))

   `(define-command-global eval-expression ()
      "Prompt for the expression and evaluate it, echoing result to the `message-area'."
      (let ((expression-string
             ;; Read an arbitrary expression. No error checking, though.
             (first (prompt :prompt "Expression to evaluate"
                            :sources (list (make-instance 'prompter:raw-source))))))
        ;; Message the thing to the message-area down below.
        (echo "~S" (eval (read-from-string expression-string)))))

   `(define-configuration nyxt/web-mode:web-mode
      ;; Bind eval-expression to M-:, but only in emacs-mode.
      ((keymap-scheme (let ((scheme %slot-default%))
                        (keymap:define-key (gethash scheme:emacs scheme)
                                           "M-:" 'eval-expression)
                        scheme))))
   `(define-command-global org-capture ()
      "Org-capture current page."
      (eval-in-emacs
       `(let ((org-link-parameters
               (list (list "nyxt"
                           :store
                           (lambda ()
                             (org-store-link-props
                              :type "nyxt"
                              :link ,(quri:render-uri (url (current-buffer)))
                              :description ,(title (current-buffer))))))))
          (org-capture nil "wN"))
       (echo "Note stored!")))
   `(define-command-global org-roam-capture ()
      "Org-capture current page."
      (let ((quote (%copy))
            (title (prompt
                    :input (title (current-buffer))
                    :prompt "Title of note:"
                    :sources (list (make-instance 'prompter:raw-source))))
            (text (prompt
                   :input ""
                   :prompt "Note to take:"
                   :sources (list (make-instance 'prompter:raw-source)))))
        (eval-in-emacs
         `(let ((file (on/make-filepath ,(car title) (current-time))))
            (on/insert-org-roam-file
             file
             ,(car title)
             nil
             (list ,(render-url (url (current-buffer))))
             ,(car text)
             ,quote)
            (find-file file)
            (org-id-get-create)))
        (echo "Org Roam Note stored!")))
   `(define-configuration nyxt/web-mode:web-mode
      ;; Bind org-capture to C-o-c, but only in emacs-mode.
      ((keymap-scheme (let ((scheme %slot-default%))
                        (keymap:define-key (gethash scheme:emacs scheme)
                                           "C-c o c" 'org-capture)
                        scheme))))
   `(define-configuration nyxt/web-mode:web-mode
      ;; Bind org-roam-capture to C-c n f, but only in emacs-mode.
      ((keymap-scheme (let ((scheme %slot-default%))
                        (keymap:define-key (gethash scheme:emacs scheme)
                                           "C-c n f" 'org-roam-capture)
                        scheme))))
   )
  (unless no-maximize
    (emacs-with-nyxt-send-sexps
     '(toggle-fullscreen))))

(defun emacs-with-nyxt-browse-url-nyxt (url &optional buffer-title)
  "Open URL with Nyxt and optionally define BUFFER-TITLE."
  (interactive "sURL: ")
  (emacs-with-nyxt-send-sexps
   (cl-concatenate
    'list
    (list
     'buffer-load
     url)
    (if buffer-title
        `(:buffer (make-buffer :title ,buffer-title))
      nil))))

(defun emacs-with-nyxt-close-nyxt-connection ()
  "Close Nyxt connection."
  (interactive)
  (emacs-with-nyxt-send-sexps '(quit)))

(defun browse-url-nyxt (url &optional new-window)
  "Browse URL with Nyxt. NEW-WINDOW is ignored."
  (interactive "sURL: ")
  (unless (emacs-with-nyxt-connected-p) (emacs-with-nyxt-start-and-connect-to-nyxt))
  (emacs-with-nyxt-browse-url-nyxt url url))

(defun emacs-with-nyxt-search-first-in-nyxt-current-buffer (string)
  "Search current Nyxt buffer for STRING."
  (interactive "sString to search: ")
  (unless (emacs-with-nyxt-connected-p) (emacs-with-nyxt-start-and-connect-to-nyxt))
  (emacs-with-nyxt-send-sexps
   `(nyxt/web-mode::highlight-selected-hint
     :link-hint
     (car (nyxt/web-mode::matches-from-json
           (nyxt/web-mode::query-buffer :query ,string)))
     :scroll 't)))

(defun emacs-with-nyxt-make-qr-code-of-current-url ()
  "Open QR code of current url."
  (interactive)
  (if (file-exists-p "~/quicklisp/setup.lisp")
      (progn
        (unless (emacs-with-nyxt-connected-p) (emacs-with-nyxt-start-and-connect-to-nyxt))
        (emacs-with-nyxt-send-sexps
         '(ql:quickload "cl-qrencode")
         '(cl-qrencode:encode-png (quri:render-uri (url (current-buffer))) :fpath "/tmp/qrcode.png"))
        (find-file "/tmp/qrcode.png")
        (auto-revert-mode))
    (error "You cannot use this until you have Quicklisp installed! Check how to do that at: https://www.quicklisp.org/beta/#installation")))

(defun emacs-with-nyxt-get-nyxt-buffers ()
  "Return nyxt buffers."
  (when (emacs-with-nyxt-connected-p)
    (read
     (emacs-with-nyxt-send-sexps
      '(map 'list (lambda (el) (slot-value el 'title)) (buffer-list))))))

(defun emacs-with-nyxt-nyxt-switch-buffer (&optional title)
  "Interactively switch nyxt buffers.  If argument is provided switch to buffer with TITLE."
  (interactive)
  (if (emacs-with-nyxt-connected-p)
      (let ((title (or title (completing-read "Title: " (emacs-with-nyxt-get-nyxt-buffers)))))
        (emacs-with-nyxt-send-sexps
         `(switch-buffer :id (slot-value (find-if #'(lambda (el) (equal (slot-value el 'title) ,title)) (buffer-list)) 'id))))
    (error (format "%s is not connected to Nyxt. Run `emacs-with-nyxt-start-and-connect-to-nyxt' first" cl-ide))))

(defun emacs-with-nyxt-get-nyxt-commands ()
  "Return nyxt commands."
  (when (emacs-with-nyxt-connected-p)
    (read
     (emacs-with-nyxt-send-sexps
      `(let ((commands (make-instance 'command-source)))
         (map 'list (lambda (el) (slot-value el 'name)) (funcall (slot-value commands 'prompter:CONSTRUCTOR) commands)))))))

(defun emacs-with-nyxt-nyxt-run-command (&optional command)
  "Interactively run nyxt COMMAND."
  (interactive)
  (if (emacs-with-nyxt-connected-p)
      (let ((command (or command (completing-read "Execute command: " (emacs-with-nyxt-get-nyxt-commands)))))
        (emacs-with-nyxt-send-sexps `(nyxt::run-async ',(read command))))
    (error (format "%s is not connected to Nyxt. Run `emacs-with-nyxt-start-and-connect-to-nyxt' first" cl-ide))))

(defun emacs-with-nyxt-nyxt-take-over-prompt ()
  "Take over the nyxt prompt and let Emacs handle completions."
  (interactive)
  (emacs-with-nyxt-send-sexps
   `(progn
      (defun flatten (structure)
        (cond ((null structure) nil)
              ((atom structure) (list structure))
              (t (mapcan #'flatten structure))))

      (defun prompt (&REST args)
        (flet ((ensure-sources (specifiers)
                               (mapcar (lambda (source-specifier)
                                         (cond
                                          ((and (symbolp source-specifier)
                                                (c2cl:subclassp source-specifier 'source))
                                           (make-instance source-specifier))
                                          (t source-specifier)))
                                       (uiop:ensure-list specifiers))))
              (sleep 0.1)
              (let* ((promptstring (list (getf args :prompt)))
                     (sources (ensure-sources (getf args :sources)))
                     (names (mapcar (lambda (ol) (slot-value ol 'prompter:attributes)) (flatten (mapcar (lambda (el) (slot-value el 'PROMPTER::INITIAL-SUGGESTIONS)) sources))))
                     (testing (progn
                                (setq my-names names)
                                (setq my-prompt promptstring)))
                     (completed (read-from-string (eval-in-emacs `(emacs-with-nyxt-nyxt-complete ',promptstring ',names))))
                     (suggestion
                      (find-if (lambda (el) (equal completed (slot-value el 'PROMPTER::ATTRIBUTES))) (flatten (mapcar (lambda (el) (slot-value el 'PROMPTER::INITIAL-SUGGESTIONS)) sources))))
                     (selected-class (find-if (lambda (el) (find suggestion (slot-value el 'PROMPTER::INITIAL-SUGGESTIONS))) sources)))
                (if selected-class
                    (funcall (car (slot-value selected-class 'PROMPTER::ACTIONS)) (list (slot-value suggestion 'PROMPTER:VALUE)))
                  (funcall (car (slot-value (car sources) 'PROMPTER::ACTIONS)) (list completed)))))))))

(defun emacs-with-nyxt-nyxt-complete (prompt names)
  "Completion function for nyxt completion."
  (let* ((completions (--map (s-join "\t" (--map (s-join ": " it) it)) names))
         (completed-string (completing-read (s-append ": " (car prompt)) completions))
         (completed-index (-elem-index  completed-string completions)))
    (if (numberp completed-index)
        (nth completed-index names)
      completed-string)))
;; Nyxt:1 ends here
