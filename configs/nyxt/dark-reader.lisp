(in-package #:nyxt-user)

(define-configuration nx-dark-reader:dark-reader-mode
  ((nxdr:selection-color "#ff7eb6")
   (nxdr:background-color "#161616")
   (nxdr:text-color "#f2f4f8")
   ;; (nxdr:grayscale 21)
   (nxdr:contrast 93)
   (nxdr:brightness 81)))

(define-configuration web-buffer
  ((default-modes `(nxdr:dark-reader-mode ,@%slot-value%))))

