;; List of auto-rules (meant to be human-readable and human-writable)

;; Rules start on a new line and consist of one or more of the following:

;; - Conditions for rule activation. One of match-domain, match-host,
;;   match-regex or match-port (please refer to help system); a user-defined
;;   condition; or a URL between string quotes.

;; - :included (optional) -- List of the modes to enable on condition.

;; - :excluded (optional) -- List of the modes to disable on condition.

;; - :exact-p  (optional) -- Whether to enable only :included modes and disable
;;    everything else (if :exact-p is t), or just add :included and exclude
;;    :excluded modes from the current modes list (if :exact-p is nil or not
;;    present).

;; Included modes is a list of mode symbols, or a list of lists in the form of
;; (MODE-SYMBOL INIT-ARGS), where init-args are passed to the mode when
;; instantiated.

;; Conditions work this way:
;; - match-domain matches the URL domains only.
;;   Example: (match-domain "reddit.com") will work for all of Reddit.

;; - match-host is more specific -- it matches only on certain subdomains of
;;   the website.
;;   Example: (match-host "old.reddit.com") will work on old Reddit only.

;; - match-regex works for any address that matches a given regex. You can add
;;   these manually, but remember: with great regex comes great responsibility!
;;   Example: (match-regex "https://github\.com/.*/.*") will match only in
;;   repos on GitHub.

;; - match-port matches the port number(s) only.
;;   Example: (match-port 8000 8080) will work for e.g. localhost:8000,
;;   127.0.0.1:8080, etc.

;; - String format matches the exact URL and nothing else
;;   Example: "https://lispcookbook.github.io/cl-cookbook/pattern_matching.html"
;;   will work on the Pattern Matching article of CL Cookbook, and nowhere else.

;; - Any other Lisp form is evaluated and the result of it is called with the
;;   URL as an argument. This means you can write arbitrary Lisp code to
;;   activate auto-rules.
;;   Note: The URL is passed as quri:uri object.
;;   Example: (lambda (url) (string= "/my/path" (quri:uri-path url)))

;; You can write additional URLs in the parenthesized conditions, to reuse the
;; rule for other URL.
;; Example: (match-host "reddit.com" "old.reddit.com" "www6.reddit.com")

;;; Rules:

(
((match-domain "discord.com" "github.com" "www.reddit.com" "duckduckgo.com")  :excluded (nx-dark-reader:dark-reader-mode))
((match-domain "discord.com" "reddit.com" "youtube.com" "duckduckgo.com")  :excluded (nyxt/history-mode:history-mode))
((match-domain "9front.org")  :excluded (nyxt/reduce-tracking-mode:reduce-tracking-mode))
((match-regex "^https?://([a-z0-9.-]+.)?[a-z2-7]{56}.onion")  :included (nyxt-user:tor-proxy-mode) :excluded (nyxt/force-https-mode:force-https-mode))
)
