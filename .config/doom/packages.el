;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;;org
(package! org-fragtog)
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
(package! focus)
(package! websocket)
(package! org-roam-ui :recipe (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out")))
(unpin! org)

;;latex
(package! aas :recipe (:host github :repo "ymarco/auto-activating-snippets"))
(package! laas :recipe (:host github :repo "tecosaur/LaTeX-auto-activating-snippets"))
(package! calctex :recipe (:host github :repo "johnbcoughlin/calctex" :files ("*.el" "calctex/*.el" "calctex-contrib/*.el" "org-calctex/*.el" "vendor")))

;;looks
(package! solaire-mode :disable t)
(unpin! doom-themes)

;;markdown
(package! ox-gfm)

;;other
(package! keycast)
(package! selectric-mode)
(package! evil-better-visual-line)
