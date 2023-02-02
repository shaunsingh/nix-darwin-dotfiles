;;;; nx-search-engines.asd

(asdf:defsystem #:nx-search-engines
  :description "A collection of easy-to-setup search-engines for Nyxt browser."
  :author "Artyom Bologov"
  :license "BSD 2-Clause"
  :version "1.1.0"
  :serial t
  :depends-on (#:nyxt)
  :components ((:file "package")
               (:file "search-engines")))
