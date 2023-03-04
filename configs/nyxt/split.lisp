(in-package #:nyxt-user)

(define-panel-command vsplit-internal (&key (url (quri:render-uri (url (current-buffer)))))
    (panel "*Duplicate panel*" :right)
  "Duplicate the current buffer URL in the panel buffer on the right.

A poor man's vsplit :("
  (setf 
    (ffi-width panel) (round (/ (ffi-width (current-window)) 2)))
  (run-thread "URL loader"
    (sleep 0.3)
    (buffer-load (quri:uri url) :buffer panel))
  "")

(define-command-global vsplit ()
  "Based on `vsplit-internal' above."
  (if (nyxt/renderer/gtk:panel-buffers-right
       (current-window))
      (delete-all-panel-buffers (current-window))
      (vsplit-internal)))
