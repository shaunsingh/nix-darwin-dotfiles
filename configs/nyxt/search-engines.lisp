(in-package #:nyxt-user)

(defvar *duckduckgo-keywords*
  '(:theme :dark
    :help-improve-duckduckgo nil
    :homepage-privacy-tips nil
    :privacy-newsletter nil
    :newsletter-reminders nil
    :install-reminders nil
    :install-duckduckgo nil
    :units-of-measure :metric
    :keyboard-shortcuts t
    :advertisements nil
    :open-in-new-tab nil
    :infinite-scroll t
    :safe-search :off
    :font-size :larger
    :header-behavior :on-fixed
    :font "Liga SFMono Nerd Font"
    :background-color "161616"))

(define-configuration context-buffer
  ((search-engines (list
                    ;; code search
                    (engines:github)
                    (engines:gitea)
                    (engines:sourcehut)

                    ;; research
                    (engines:scihub)
                    (engines:wikipedia)
                    (engines:google-scholar)

                    ;; social
                    (engines:teddit)
                    (engines:libgen)
                    (engines:lobsters)
                    (engines:invidious)
                    (engines:hacker-news)

                    ;; package management
                    (engines:arch)
                    (engines:pkgs)
                    (engines:debian)
                    (engines:arch-aur)
                    (make-instance 'search-engine
                                   :shortcut "nix"
                                   :search-url "https://search.nixos.org/packages?channel=unstable&from=0&size=50&sort=relevance&type=packages&query=~a"
                                   :fallback-url (quri:uri "https://search.nixos.org/"))
                    (make-instance 'search-engine
                                   :shortcut "golang"
                                   :search-url "https://golang.org/pkg/~a/"
                                   :fallback-url (quri:uri "https://golang.org/pkg/")
                                   :completion-function
                                   (let ((installed-packages
                                           (str:split nyxt::+newline+
                                                      (ignore-errors
                                                       (uiop:run-program
                                                        "go list all"
                                                        :output '(:string :stripped t))))))
                                     (lambda (input)
                                       (sort
                                        (serapeum:filter (alexandria:curry #'str:containsp input)
                                                         installed-packages)
                                        #'> :key (alexandria:curry
                                                  #'prompter::score-suggestion-string input)))))

                    ;; maps
                    (engines:google :shortcut "gmaps"
                                    :object :maps)
                    (make-instance 'search-engine
                                   :shortcut "osm"
                                   :search-url "https://www.openstreetmap.org/search?query=~a"
                                   :fallback-url (quri:uri "https://www.openstreetmap.org/"))

                    ;; general search engines
                    (engines:google)
                    (engines:duckduckgo-html-only)
                    (apply #'engines:duckduckgo-images
                           *duckduckgo-keywords*)
                    (apply #'engines:duckduckgo
                           :shortcut "d" *duckduckgo-keywords*)
                    (make-instance 'search-engine
                                   :shortcut "kagi"
                                   :search-url "https://kagi.com/search?q=~a"
                                   :fallback-url (quri:uri "https://kagi.com/"))

                    ;; default search
                    (engines:searx :base-search-url "https://xo.wtf/search?q=~a")))))
