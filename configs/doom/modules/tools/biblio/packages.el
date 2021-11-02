;; -*- no-byte-compile: t; -*-
;;; tools/biblio/packages.el

(package! bibtex-completion :pin "bb47f355b0da8518aa3fb516019120c14c8747c9")
(when (featurep! :completion ivy)
  (package! ivy-bibtex :pin "bb47f355b0da8518aa3fb516019120c14c8747c9"))
(when (featurep! :completion helm)
  (package! helm-bibtex :pin "bb47f355b0da8518aa3fb516019120c14c8747c9"))
(when (featurep! :completion vertico)
  (package! citar :pin "6e30af6f739ed01437d58d7b3e54a5757e8eb55a"))

(package! citeproc :pin "c8ff95862823cdff067e8cc9bb7f5ef537e8f1d9")
