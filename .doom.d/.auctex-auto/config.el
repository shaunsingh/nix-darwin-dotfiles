(TeX-add-style-hook
 "config"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "11pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8") ("fontenc" "T1") ("ulem" "normalem") ("xcolor" "usenames")))
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "href")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art11"
    "inputenc"
    "fontenc"
    "graphicx"
    "grffile"
    "longtable"
    "wrapfig"
    "rotating"
    "ulem"
    "amsmath"
    "textcomp"
    "amssymb"
    "capt-of"
    "hyperref"
    "art10"
    "xcolor"
    "booktabs"
    "soul"
    "adjustbox"
    "siunitx"
    "cancel"
    "mathtools"
    "mathalpha"
    "xparse"
    "arevmath")
   (LaTeX-add-labels
    "sec:org28d84fa"
    "sec:orgb21a723"
    "sec:orgc875187"
    "sec:orge3a2f45"
    "sec:orgc66a026"
    "sec:org1dfb24f"
    "sec:orgdf50124"
    "sec:orgb27e915"
    "sec:org360f29e"
    "sec:org48d45cc"
    "sec:orgb0334f4"
    "sec:orgb7292bf"
    "sec:orge6ccd14"
    "sec:org399a9ec"
    "sec:org762f479"
    "sec:orge0e59ad"
    "sec:org4b64010"
    "sec:org308f9f2"
    "sec:orgc8dc77e"
    "sec:org1ee293d"))
 :latex)

