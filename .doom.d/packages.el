;; -*- no-byte-compile: t; -*-

;;toggle between latex and preview
(package! org-fragtog)
;;references for research documents
(package! org-ref)
(package! org-appear)
;;add some padding in org mode
(package! org-padding :recipe (:host github :repo "TonCherAmi/org-padding" ))
;;use latex in the emacs calaculator
(package! calctex :recipe (:host github :repo "johnbcoughlin/calctex" :files ("*.el" "calctex/*.el" "calctex-contrib/*.el" "org-calctex/*.el" "vendor")))
;;focus mode in writeroom mode
(package! focus)
;;always get the latest doom themes
(unpin! doom-themes)
;;scroll by visual lines, not lines
(package! evil-better-visual-line)
;;markdown/org preview
(package! ox-gfm)
(package! grip-mode)
;;jupyter notebooks
(package! ein)
;;tabnine completion
(package! company-tabnine)
