(in-package #:nyxt-user)

(let ((*default-pathname-defaults* (uiop:pathname-directory-pathname (files:expand *config-file*))))
  (load "init"))
