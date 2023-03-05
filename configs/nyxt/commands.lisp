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
