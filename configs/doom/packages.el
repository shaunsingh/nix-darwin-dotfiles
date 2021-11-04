;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;;org
(package! org-appear)
(package! doct)
(package! org-roam-ui :recipe (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out")))
(package! org-pandoc-import
  :recipe (:host github
           :repo "tecosaur/org-pandoc-import"
           :files ("*.el" "filters" "preprocessors")))

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
(package! ox-chameleon :recipe (:host github :repo "tecosaur/ox-chameleon"))

;;emacs additions
(package! lexic)
(package! magit-delta)
(package! pdf-tools)

;;lsp
(unpin! lsp-ui)
(unpin! lsp-mode)

;;fun
(package! nov)
(package! xkcd)
(package! keycast)
(package! selectric-mode)
