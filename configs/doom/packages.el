;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;;org
(package! org-appear)
(package! doct)
(package! org-roam-ui)
(package! org-pandoc-import ;https://github.com/melpa/melpa/pull/7326
  :recipe (:host github
           :repo "tecosaur/org-pandoc-import"
           :files ("*.el" "filters" "preprocessors")))
;; org cite additions
(package! citar)
(package! citeproc)
(package! org-cite-csl-activate :recipe (:host github :repo "andras-simonyi/org-cite-csl-activate"))

;;latex
(package! org-fragtog)
(package! aas)
(package! laas)
(package! engrave-faces)

;;markdown and html
(package! ox-gfm)
(package! websocket)
;;(package! webkit
;;          :recipe (:host github
;;                   :repo "akirakyle/emacs-webkit"
;;                   :branch "main"
;;                   :files (:defaults "*.js" "*.css" "*.so" "*.nix")
;;                   :pre-build (("nix-shell" "shell.nix" "--command make"))))

;;looks
(unpin! doom-themes)
(unpin! doom-modeline)
(package! modus-themes)
(package! solaire-mode :disable t)
(package! ox-chameleon :recipe (:host github :repo "tecosaur/ox-chameleon")) ;soon :tm:

;;emacs additions
(package! lexic)
(package! magit-delta)
(package! pdf-tools)
(package! screenshot :recipe (:host github :repo "Jimmysit0/screenshot")) ;https://github.com/melpa/melpa/pull/7327

;;lsp
(unpin! lsp-ui)
(unpin! lsp-mode)

;;fun
(package! nov)
(package! xkcd)
(package! keycast)
(package! selectric-mode :recipe (:local-repo "lisp/selectric-mode"))
