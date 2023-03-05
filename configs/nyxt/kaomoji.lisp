(in-package #:nyxt-user)

(define-configuration buffer
  ((override-map (let ((map (make-keymap "override-map")))
                              (define-key map
                                "C-c k" 'nx-kaomoji:kaomoji-fill)
                   map))))
