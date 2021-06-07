;; -*- no-byte-compile: t; -*-

;;toggle between latex and preview
(package! org-fragtog)

;;typewritter sounds built in
(package! selectric-mode)

;;references for research documents
(package! org-ref)
(package! org-appear)

;;break very large files into smaller ones automatically
(package! vlf :recipe (:host github :repo "m00natic/vlfi" :files ("*.el")))

;;use latex in the emacs calaculator
(package! calctex :recipe (:host github :repo "johnbcoughlin/calctex"
                           :files ("*.el" "calctex/*.el" "calctex-contrib/*.el" "org-calctex/*.el" "vendor")))

;;always get the latest doom themes
(package! doom-themes :pin nil)
