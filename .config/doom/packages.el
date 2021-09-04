;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;;org
(package! org-appear)
(package! org-super-agenda)
(package! doct
  :recipe (:host github :repo "progfolio/doct"))
(package! org-pandoc-import
  :recipe (:host github
           :repo "tecosaur/org-pandoc-import"
           :files ("*.el" "filters" "preprocessors")))
(package! org-pretty-table
  :recipe (:host github :repo "Fuco1/org-pretty-table"))
(package! org-pretty-tags)
(package! org-padding :recipe (:host github :repo "TonCherAmi/org-padding" ))
(package! org-ol-tree :recipe (:host github :repo "Townk/org-ol-tree"))
(package! websocket)
(package! org-roam-ui :recipe (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out")))

;;latex
(package! aas :recipe (:host github :repo "ymarco/auto-activating-snippets"))
(package! laas :recipe (:host github :repo "tecosaur/LaTeX-auto-activating-snippets"))
(package! calctex :recipe (:host github :repo "johnbcoughlin/calctex" :files ("*.el" "calctex/*.el" "calctex-contrib/*.el" "org-calctex/*.el" "vendor")))
(package! org-fragtog)
(package! engrave-recipes
  :recipe (:host github :repo "tecosaur/engrave-faces"))

;;markdown and html
(package! ox-gfm)
;; (package! webkit
;;           :recipe (:host github
;;                    :repo "akirakyle/emacs-webkit"
;;                    :branch "main"
;;                    :files (:defaults "*.js" "*.css" "*.so" "*.nix")
;;                    :pre-build
;;                    (("nix-shell" "shell.nix") ("make"))))

;;looks
(package! solaire-mode :disable t)
(unpin! doom-themes)

;;other
(package! keycast)
(package! selectric-mode)
(package! screenshot :recipe (:host github :repo "tecosaur/screenshot"))
(package! screenshot :recipe (:host github :repo "tecosaur/lexic"))
