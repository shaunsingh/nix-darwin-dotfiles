;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; live life on the edge
(unpin! t)

;;org
(package! doct)
(package! websocket)
(package! org-appear)
(package! org-roam-ui)
(package! org-preview-html)

;;latex
(package! aas)
(package! laas)
(package! engrave-faces)

;;looks
(package! focus)
(package! dimmer)
(package! info-colors)
(package! svg-tag-mode)
(package! solaire-mode :disable t)
;; (package! ox-chameleon :recipe (:host github :repo "tecosaur/ox-chameleon") :pin "5a1928b9c33cbeb0463cf794afe8cff4ab512ce7")

;;nano
(package! nano-theme)
(package! nano-modeline

;;emacs additions
;; (package! meow)
(package! lexic)

;;fun
(package! nov)
(package! xkcd)
(package! md4rd)
(package! elcord)
(package! monkeytype)
;; (package! selectric-mode :recipe (:local-repo "lisp/selectric-mode"))
