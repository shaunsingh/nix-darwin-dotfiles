;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;;org
(package! org-appear)
(package! doct :recipe (:host github :repo "progfolio/doct"))
(package! org-padding :recipe (:host github :repo "TonCherAmi/org-padding"))
(package! org-ol-tree :recipe (:host github :repo "Townk/org-ol-tree"))
(package! org-pretty-table :recipe (:host github :repo "Fuco1/org-pretty-table"))
(package! org-roam-ui :recipe (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out")))
(package! org-pandoc-import
  :recipe (:host github
           :repo "tecosaur/org-pandoc-import"
           :files ("*.el" "filters" "preprocessors")))

;;latex
;;(package! org-fragtog)
(package! aas :recipe (:host github :repo "ymarco/auto-activating-snippets"))
(package! laas :recipe (:host github :repo "tecosaur/LaTeX-auto-activating-snippets"))
(package! engrave-faces :recipe (:host github :repo "tecosaur/engrave-faces"))
(package! org-fragtog)

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
(package! solaire-mode :disable t)
(package! ox-chameleon :recipe (:host github :repo "tecosaur/ox-chameleon"))

;;emacs additions
;;(package! vlf :recipe (:host github :repo "m00natic/vlfi" :files ("*.el")))
(package! screenshot :recipe (:host github :repo "tecosaur/screenshot"))
(package! lexic :recipe (:host github :repo "tecosaur/lexic"))
(package! magit-delta :recipe (:host github :repo "dandavison/magit-delta"))

;;lsp
(unpin! lsp-ui)
(unpin! lsp-mode)

;;fun
(package! xkcd)
(package! keycast)
(package! selectric-mode)
