(in-package #:nyxt-user)

#+nyxt-3
(define-panel-command hsplit-internal (&key (url (quri:render-uri (url (current-buffer)))))
    (panel "*Duplicate panel*" :right)
  "Duplicate the current buffer URL in the panel buffer on the right.
A poor man's hsplit :)"
  (setf (ffi-window-panel-buffer-width (current-window) panel) 550)
  (run-thread "URL loader"
    (sleep 0.3)
    (buffer-load (quri:uri url) :buffer panel))
  "")

#+nyxt-3
(define-command-global close-all-panels ()
  "Close all the panel buffers there are."
  (when (panel-buffers-right (current-window))
    (delete-panel-buffer :window (current-window) :panels (panel-buffers-right (current-window))))
  (when (panel-buffers-left (current-window))
    (delete-panel-buffer :window (current-window) :panels (panel-buffers-left (current-window)))))

#+nyxt-3
(define-command-global hsplit ()
  "Based on `hsplit-panel' above."
  (if (panel-buffers-right (current-window))
      (close-all-panels)
      (hsplit-internal)))

#+nyxt-3-pre-release-2
(nyxt/bookmarklets-mode:define-bookmarklet-command-global post-to-hn
  "Post the link you're currently on to Hacker News"
  "window.location=\"https://news.ycombinator.com/submitlink?u=\" + encodeURIComponent(document.location) + \"&t=\" + encodeURIComponent(document.title)")

(define-command-global open-in-nosave-buffer ()
  "Make a new nosave buffer with URL at point."
  (let ((url (url-at-point (current-buffer))))
    (make-nosave-buffer :url url)))

#+nyxt-3-pre-release-2
(ffi-add-context-menu-command
 'open-in-nosave-buffer
 "Open Link in New Nosave Buffer")

#+(and nyxt-gtk nyxt-3)
(define-command-global make-new-buffer-with-url-and-context ()
  "Make a new buffer with a user-chosen context and a URL under pointer."
  (make-buffer-with-context :url (url-at-point (current-buffer))))

#+nyxt-3-pre-release-2
(ffi-add-context-menu-command
 'make-new-buffer-with-url-and-context
 "Open Link in New Buffer with Context")

#+nyxt-3
(define-panel-command-global search-translate-selection (&key (selection (ffi-buffer-copy (current-buffer))))
    (panel "*Translate panel*" :right)
  "Open the translation of the selected word in a panel buffer."
  (setf (ffi-window-panel-buffer-width (current-window) panel) 550)
  (run-thread "search translation URL loader"
    (sleep 0.3)
    (buffer-load (quri:uri (format nil (nyxt::search-url (nyxt::default-search-engine))
                                   (str:concat "translate " (ffi-buffer-copy (current-buffer)))))
                 :buffer panel))
  "")

#+nyxt-3-pre-release-2
(ffi-add-context-menu-command
 'search-translate-selection
 "Translate Selection")

#+nyxt-3
(define-command-global add-autofill ()
  "Add an autofill with the selected text to the list of `autofill-mode' autofills."
  (push (make-instance 'nyxt/autofill-mode:autofill
                       :name (prompt1 :prompt "Autofill key" :sources 'prompter:raw-source)
                       :fill (ffi-buffer-copy (current-buffer)))
        (nyxt/autofill-mode::autofills (current-mode :autofill))))

#+nyxt-3-pre-release-2
(ffi-add-context-menu-command
 'add-autofill
 "Add Temporary Autofill")
