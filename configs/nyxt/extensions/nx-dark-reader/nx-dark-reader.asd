;;;; nx-dark-reader.asd

(asdf:defsystem #:nx-dark-reader
  :description "DarkReader integration for Nyxt 3.0."
  :author "Artyom Bologov"
  :license "BSD 2-Clause"
  :version "0.0.1"
  :serial t
  :depends-on (#:nyxt)
  :components ((:file "package")
               (:file "nx-dark-reader")))
