;; -*- no-byte-compile: t; -*-

(package! org-padding :recipe (:host github :repo "TonCherAmi/org-padding" ))

(unpin! doom-modeline)

(package! org-pretty-table :recipe (:host github :repo "Fuco1/org-pretty-table"))

(package! screenshot :recipe (:host github :repo "tecosaur/screenshot"))

(package! org-pandoc-import
  :recipe (:host github
           :repo "tecosaur/org-pandoc-import"
           :files ("*.el" "filters" "preprocessors")))

(package! org-ol-tree :recipe (:host github :repo "Townk/org-ol-tree"))

(package! ox-gfm)

(package! org-roam-ui :recipe (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out")))
(package! websocket)

(package! org-super-agenda)

(package! doct :recipe (:host github :repo "progfolio/doct"))

(package! org-fragtog)

(package! calctex :recipe (:host github :repo "johnbcoughlin/calctex" :files ("*.el" "calctex/*.el" "calctex-contrib/*.el" "org-calctex/*.el" "vendor")))

(package! engrave-faces :recipe (:host github :repo "tecosaur/engrave-faces"))

(package! ox-chameleon :recipe (:host github :repo "tecosaur/ox-chameleon"))

;;(package! webkit
;;          :recipe (:host github
;;                   :repo "akirakyle/emacs-webkit"
;;                   :branch "main"
;;                   :files (:defaults "*.js" "*.css" "*.so" "*.nix")
;;                   :pre-build (("nix-shell" "shell.nix" "--command make"))))

;; [[file:config.org::*Themes][Themes:1]]
(unpin! doom-themes)
;; Themes:1 ends here

;; [[file:config.org::*Themes][Themes:2]]
(package! solaire-mode :disable t)
;; Themes:2 ends here

;; [[file:config.org::*Very large files][Very large files:1]]
;;(package! vlf :recipe (:host github :repo "m00natic/vlfi" :files ("*.el")))
;; Very large files:1 ends here

;; [[file:config.org::*Company][Company:2]]
(package! aas :recipe (:host github :repo "ymarco/auto-activating-snippets"))
(package! laas :recipe (:host github :repo "tecosaur/LaTeX-auto-activating-snippets"))
;; Company:2 ends here

;; [[file:config.org::*LSP][LSP:1]]
(unpin! lsp-ui)
(unpin! lsp-mode)
;; LSP:1 ends here

;; [[file:config.org::*Selectric mode][Selectric mode:1]]
(package! selectric-mode)
;; Selectric mode:1 ends here

;; [[file:config.org::*Font Display][Font Display:6]]
(package! org-appear)
;; Font Display:6 ends here

;; [[file:config.org::*Keycast][Keycast:1]]
(package! keycast)
;; Keycast:1 ends here

;; [[file:config.org::*Dictionaries][Dictionaries:1]]
(package! lexic :recipe (:host github :repo "tecosaur/lexic"))
;; Dictionaries:1 ends here
