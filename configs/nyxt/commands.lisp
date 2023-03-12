(in-package #:nyxt-user)

;; global commands
(define-command-global open-in-firefox ()
   "Open current url in firefox"
   (uiop:run-program 
     (list "firefox" (render-url 
           (url (current-buffer))))))

;; bookmarklet commands
(nyxt/bookmarklets-mode:define-bookmarklet-command-global post-to-hn
  "Post the link you're currently on to Hacker News"
  "window.location=\"https://news.ycombinator.com/submitlink?u=\" + encodeURIComponent(document.location) + \"&t=\" + encodeURIComponent(document.title)")

;; context menu commands
(ffi-add-context-menu-command
 (lambda ()
   (let ((url (url-at-point (current-buffer))))
     (nyxt/bookmark-mode:bookmark-add
      url :title (fetch-url-title url))))
 "Bookmark this URL")

;; open stuff in nosave buffers
(define-command-global open-in-nosave-buffer ()
  "Make a new nosave buffer with URL at point."
  (let ((url (url-at-point (current-buffer))))
    (make-nosave-buffer :url url)))

(ffi-add-context-menu-command
 (lambda ()
   (when (url-at-point (current-buffer))
     (make-nosave-buffer :url (url-at-point (current-buffer)))))
 "Open Link in New Nosave Buffer")

;; translate
(define-panel-command-global search-translate-selection (&key (selection (ffi-buffer-copy (current-buffer))))
    (panel "*Translate panel*" :right)
  "Open the translation of the selected word in a panel buffer."
  (run-thread "search translation URL loader"
    (setf 
      (ffi-width panel) (round (/ (ffi-width (current-window)) 3)))
    (sleep 0.3)
    (buffer-load (quri:uri (format nil (nyxt::search-url (nyxt::default-search-engine))
                                   (str:concat "translate " (ffi-buffer-copy (current-buffer)) "to english")))
                 :buffer panel))
  "")

(ffi-add-context-menu-command
 'search-translate-selection
 "Translate Selection")

;; open markdown preview in a split
(defun prompt-for-markdown-file ()
  (uiop:native-namestring
   (pathname
    (prompt1
     :prompt "Open markdown file"
     :extra-modes 'nyxt/file-manager-mode:file-manager-mode
     :input (uiop:native-namestring (uiop:getcwd))
     :sources
     (list (make-instance 'nyxt/file-manager-mode:file-source
                          :name "Existing file"
                          :actions-on-return #'identity)
           (make-instance 'prompter:raw-source
                          :name "Create new file"))))))

(define-panel-command open-preview ()
    (panel "*markdown preview*" :right)
  "Open a file to preview using grip on the right buffer"
  (run-thread "grip loader"
    (setf 
      (ffi-width panel) (round (/ (ffi-width (current-window)) 2)))
    (sleep 0.3)
    (buffer-load (quri:uri "http://localhost:6419")
                 :buffer panel))
  "")

(define-command-global open-markdown (&key (file (prompt-for-markdown-file)))
  "Open a markdown file with a grip-powered preview."
  (flet ((launch-grip (file-path)
           (uiop:launch-program (format nil "grip ~a" file-path))))
    (let ((buffer (make-instance 'nyxt/editor-mode::editor-buffer
                                 :url (quri:make-uri :scheme "editor" :path file))))
      (set-current-buffer buffer)
      (launch-grip file)
      (open-preview))))

(define-command-global close-preview ()
  "Close grip preview window"
  (delete-all-panel-buffers)
  (uiop:launch-program "pkill grip"))

;; terminal stuff
(define-command-global open-terminal ()
  "Open a terminal in a new buffer"
  (let ((term-buffer (make-buffer :title "*term*"
                                  :url "http://localhost:7681/"
                                  :modes 'nyxt/passthrough-mode:passthrough-mode)))
    (set-current-buffer term-buffer)))

(define-panel-command-global open-terminal-split ()
    (panel "*term split*" :right)
  "Open a terminal on the right buffer"
  (run-thread "term loader"
    (setf 
      (ffi-width panel) (round (/ (ffi-width (current-window)) 2)))
    (sleep 0.3)
    (buffer-load (quri:uri "http://localhost:7681/")
                 :buffer panel))
  "")
