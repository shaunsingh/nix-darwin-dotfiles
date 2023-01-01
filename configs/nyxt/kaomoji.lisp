(in-package #:nyxt-user)

;;; Add a single keybinding for the extension-provided `kaomoji-fill' command.
#+nyxt-2
(define-configuration nyxt/web-mode:web-mode
  ((keymap-scheme (let ((scheme %slot-default%))
                    (keymap:define-key (gethash scheme:emacs scheme)
                      "C-c K" 'nx-kaomoji:kaomoji-fill)
                    scheme))))
