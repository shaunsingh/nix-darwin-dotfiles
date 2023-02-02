;;;; package.lisp

(defpackage #:nx-search-engines
  (:nicknames #:search-engines #:engines)
  (:use #:cl)
  (:import-from #:nyxt
                #:define-class
                #:define-mode
                #:define-command
                #:current-mode
                #:search-engine
                #:new-url-query
                #:default-search-engine
                #:make-search-completion-function)
  (:import-from #:serapeum
                #:->
                #:export-always)
  (:documentation "A collection of search engines for Nyxt browser."))
