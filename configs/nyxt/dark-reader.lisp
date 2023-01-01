(in-package #:nyxt-user)

(define-configuration nx-dark-reader:dark-reader-mode
  ((nxdr:selection-color "#CD5C5C")
   (nxdr:background-color "black")
   (nxdr:text-color "white")
   (nxdr:grayscale 50)
   (nxdr:contrast 100)
   (nxdr:brightness 100)))

(define-configuration web-buffer
  ((default-modes `(nx-dark-reader:dark-reader-mode ,@%slot-value%))))
