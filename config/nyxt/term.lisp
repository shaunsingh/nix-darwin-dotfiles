(in-package #:nyxt-user)

;;; provide interaction with xterm.js

(define-command-global open-terminal ()
  "Open a terminal in a new buffer"
  (let ((term-buffer (make-buffer :title "*xterm*"
                                  :url "http://localhost:3000/wetty"
                                  :modes 'nyxt/mode/passthrough:passthrough-mode)))
    (set-current-buffer term-buffer)))
