;;; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load in.
;; Press 'K' on a module to view its documentation, and 'gd' to browse its directory.

(doom! :completion
       (company                     ; the ultimate code completion backend
        +childframe)                ; ... when your children are better than you
       (vertico +icons)             ; the search engine of the future

       :ui
       doom-dashboard               ; a nifty splash screen for Emacs
       (ligatures                   ; ligatures and symbols to make your code pnoretty again
        +extra)                     ; for those who dislike letters
       minimap                      ; show a map of the code on the side
       ophints                      ; highlight the region an operation acts on
       (popup                       ; tame sudden yet inevitable temporary windows
        +all                        ; catch all popups that start with an asterix
        +defaults)                  ; default popup rules
       vc-gutter                    ; vcs diff in the fringe
       workspaces                   ; tab emulation, persistence & separate workspaces
       zen                          ; distraction-free coding or writing

       :editor
       (evil +everywhere)           ; come to the dark side, we have cookies
       file-templates               ; auto-snippets for empty files
       fold                         ; (nigh) universal code folding
       (format +onsave)             ; automated prettiness
       parinfer                     ; turn lisp into python, sort of
       snippets                     ; my elves. They type so I don't have to

       :emacs
       (dired +icons)               ; making dired pretty [functional]
       electric                     ; smarter, keyword-based electric-indent
       (ibuffer +icons)             ; interactive buffer management
       undo                         ; persistent, smarter undo for your inevitable mistakes
       vc                           ; version-control and Emacs, sitting in a tree

       :term
       eshell                       ; the elisp shell that works everywhere
       vterm                        ; the best terminal emulation in Emacs

       :checkers
       syntax                                         ; tasing you for every semicolon you forget
       (:if (executable-find "aspell") spell)         ; tasing you for misspelling mispelling
       (:if (executable-find "languagetool") grammar) ; tasing grammar mistake every you make

       :tools
       biblio                       ; Writes a PhD for you (citation needed)
       (debugger +lsp)              ; FIXME stepping through code, to help you add bugs
       (eval +overlay)              ; run code, run (also, repls)
       (lookup                      ; helps you navigate your code and documentation
        +dictionary                 ; dictionary/thesaurus is nice
        +docsets)                   ; ...or in Dash docsets locally
       (lsp +peek)                  ; Language Server Protocol
       pdf                          ; pdf enhancements
       rgb                          ; creating color strings
       tree-sitter                  ; Syntax and Parsing sitting in a tree

       :os
       (:if IS-MAC macos)           ; improve compatibility with macOS

       :lang
       ;;agda                       ; types of types of types of types...
       ;;beancount                  ; mind the GAAP
       ;;(cc +lsp +tree-sitter)       ; C/C++/Obj-C madness
       (clojure +lsp)               ; java with a lisp
       ;;common-lisp                ; if you've seen one lisp, you've seen them all
       ;;coq                        ; proofs-as-programs
       ;;crystal                    ; ruby at the speed of c
       ;;csharp                     ; unity, .NET, and mono shenanigans
       ;;data                       ; config/data formats
       ;;(dart +flutter)            ; paint ui and not much else
       ;;dhall                      ; JSON with FP sprinkles
       ;;elixir                     ; erlang done right
       ;;elm                        ; care for a cup of TEA?
       emacs-lisp                   ; drown in parentheses
       ;;erlang                     ; an elegant language for a more civilized age
       ;;ess                        ; emacs speaks statistics
       ;;faust                      ; dsp, but you get to keep your soul
       ;;fsharp                     ; ML stands for Microsoft's Language
       ;;fstar                      ; (dependent) types and (monadic) effects and Z3
       ;;gdscript                   ; the language you waited for
       ;;(go +lsp)                  ; the hipster dialect
       ;;(haskell +lsp)             ; a language that's lazier than I am
       ;;hy                         ; readability of scheme w/ speed of python
       ;;idris                      ;
       ;;json                       ; At least it ain't XML
       ;;(java +lsp                 ; the poster child for carpal tunnel syndrome
       ;;(javascript +lsp)          ; all(hope(abandon(ye(who(enter(here))))))
       ;;(julia +lsp)               ; Python, R, and MATLAB in a blender
       ;;(kotlin +lsp)              ; a better, slicker Java(Script)
       (latex                       ; writing papers in Emacs has never been so fun
        ;;+fold                     ; fold the clutter away nicities
        +latexmk                    ; modern latex plz
        ;;+cdlatex                  ; quick maths symbols
        +lsp)
       ;;lean                       ; proof that mathematicians need help
       ;;factor                     ; for when scripts are stacked against you
       ;;ledger                     ; an accounting system in Emacs
       (lua +lsp +fennel)           ; one-based indices? one-based indices
       ;;(markdown +grip)           ; writing docs for people to ignore
       ;;nim                        ; python + lisp at the speed of c
       nix                          ; I hereby declare "nix geht mehr!"
       ;;ocaml                      ; an objective camel
       (org                         ; organize your plain life in plain text
        ;;+pretty                   ; yessss my pretties! (nice unicode symbols)
        +dragndrop                  ; drag & drop files/images into org buffers
        ;;+hugo                     ; use Emacs for hugo blogging
        ;;+noter                    ; enhanced PDF notetaking
        +jupyter                    ; ipython/jupyter support for babel
        +pandoc                     ; export-with-pandoc support
        +gnuplot                    ; who doesn't like pretty pictures
        +pomodoro                   ; be fruitful with the tomato technique
        ;;+present                  ; using org-mode for presentations
        +roam2)                     ; wander around notes
       ;;php                        ; perl's insecure younger brother
       ;;plantuml                   ; diagrams for confusing people more
       ;;purescript                 ; javascript, but functional
       (python                      ; beautiful is better than ugly
        +lsp
        +pyright
        +tree-sitter
        +cython)
       ;;qt                         ; the 'cutest' gui framework ever
       ;;racket                     ; a DSL for DSLs
       ;;raku                       ; the artist formerly known as perl6
       ;;rest                       ; Emacs as a REST client
       ;;rst                        ; ReST in peace
       ;;(ruby +rails)              ; 1.step {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
       (rust +lsp +tree-sitter)     ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       ;;scala                      ; java, but good
       ;;scheme                     ; a fully conniving family of lisps
       ;;(sh +lsp +fish +tree-sitter) ; she sells {ba,z,fi}sh shells on the C xor
       ;;sml                        ; no, the /other/ ML
       ;;solidity                   ; do you need a blockchain? No.
       ;;swift                      ; who asked for emoji variables?
       ;;terra                      ; Earth and Moon in alignment for performance.
       ;;(web +lsp)                 ; the tubes
       ;;yaml                       ; JSON, but readable
       ;;zig                        ; C, but simpler

       :email
       ;;(:if (executable-find "mu") (mu4e +org +gmail))

       :app
       ;;calendar                   ; A dated approach to timetabling
       ;;emms                       ; Multimedia in Emacs is music to my ears
       ;;everywhere                   ; *leave* Emacs!? You must be joking.
       ;;(rss +org)                   ; emacs as an RSS reader

       :config
       literate
       (default +bindings +smartparens))
