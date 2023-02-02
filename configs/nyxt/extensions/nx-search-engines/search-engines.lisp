;;;; search-engines.lisp

(in-package #:nx-search-engines)

(export-always 'define-search-engine)
(defmacro define-search-engine (name (&key shortcut fallback-url base-search-url
                                        force-supply-p manual-delims-p completion-function
                                        documentation)
                                &body keywords)
  "Defines a new `nyxt:search-engine' called NAME and having FALLBACK-URL.
`nyxt:search-url' of the new engine is built from BASE-SEARCH-URL and KEYWORDS.

FALLBACK-URL, SHORTCUT, AND COMPLETION-FUNCTION are documented in
`nyxt:search-engine'.

DOCUMENTATION is the documentation string for the function that
define-search-engine generates.

BASE-SEARCH-URL is a control string with one placeholder (e.g.,
\"example.com/?q=~a\").

FORCE-SUPPLY-P specifies if all of the provided KEYWORDS are to be supplied
by force to BASE-SEARCH-URL for those search engines that need this behavior.

MANUAL-DELIMS-P indicates we'll manually pass the query parameters delimiters in
the KEYWORDS URI-PARAM, useful if a search engine doesn't use a traditional
search URL structure.

Each keyword in KEYWORDS is a list like (KEYWORD URI-PARAM VALUES),
where VALUES is either
- an association list of possible values and their URI representations, or
- a (:function FUNC) list where FUNC is function to process the value
  provided by the user.

Examples (documentation and completion functions omitted for brevity):
Simple search engine for Wikipedia:

\(define-search-engine wikipedia
    (:shortcut \"wikipedia\"
     :base-search-url \"https://en.wikipedia.org/w/index.php?search=~a\"
     :fallback-url (quri:uri \"https://en.wikipedia.org/\")))

A more involved example with keywords:

\(define-search-engine google
    (:shortcut \"google\"
     :fallback-url \"https://google.com\"
     :base-search-url \"https://google.com/search?q=~a\")
  (safe-search \"safe\" ((t   \"strict\")
                         (nil \"images\")))
  (object \"tbm\" ((:all      \"\")
                   (:image    \"isch\")
                   (:video    \"vid\")
                   (:news     \"nws\")
                   (:shopping \"shop\")
                   (:books    \"bks\")
                   (:finance  \"fin\"))))"
  (flet ((supplied-p (symbol)
           (intern (format nil "~s-SUPPLIED-P" symbol)
                   (symbol-package symbol)))
         (make-cond (arg-name values)
           `(cond
              ,@(loop :for value :in values
                      :collect
                      `((equal ,arg-name ,(first value))
                        ,(second value))
                        :into clauses
                      :finally (return (append clauses (list `(t ,arg-name))))))))
    `(progn
       (export-always (quote ,name))
       (defun ,name (&key
                       (fallback-url ,fallback-url)
                       (shortcut ,shortcut)
                       (completion-function ,completion-function)
                       (base-search-url ,base-search-url)
                       (manual-delims-p ,manual-delims-p)
                       (force-supply-p ,force-supply-p)
                       ,@(mapcar #'(lambda (k)
                                     (list (first k)                 ; name
                                           (if (eq (first (third k)) :function)
                                               nil
                                               (first (first (third k)))) ; default value
                                           (supplied-p (first k))))  ; supplied-p
                                 keywords))
         (declare (ignorable force-supply-p manual-delims-p
                             ,@(mapcar #'first keywords)
                             ,@(mapcar (alexandria:compose #'supplied-p #'first) keywords)))
         (make-instance
          'search-engine
          :shortcut shortcut
          :fallback-url fallback-url
          :completion-function completion-function
          :search-url (format nil "~a~{~a~}"
                              base-search-url
                              (delete
                               nil
                               (list
                                ,@(loop :for (arg-name uri-parameter values)
                                          :in keywords
                                        :collect
                                        `(when ,(or force-supply-p (supplied-p arg-name))
                                           (format nil (if manual-delims-p
                                                           "~a~a"
                                                           "&~a=~a")
                                                   ,uri-parameter
                                                   ,(if (eq (first values) :function)
                                                        `(funcall ,(second values) ,arg-name)
                                                        (make-cond arg-name values))))))))))
       ,@(when documentation
           `((setf (documentation (quote ,name) 'function) ,documentation))))))

(export-always 'define-derived-search-engine)
(defmacro define-derived-search-engine (name (parent-engine &rest arguments) &optional documentation)
  ;; TODO: Use `mopu:function-arglist' to reproduce original arglist?
  `(progn
     (export-always (quote ,name))
     (defun ,name (&rest args)
       ,documentation
       (apply (function ,parent-engine) (append args (list ,@arguments))))))

(export-always 'make-brave-completion)
(defun make-brave-completion (&key request-args)
  "Helper that generates Brave search completion functions. The only
thing that's left to pass to it is REQUEST-ARGS to slightly modify the
request."
  (make-search-completion-function
   ;; TODO: Maybe set rich=true and process the JSON for 3.0+ search suggestions format?
   :base-url "https://search.brave.com/api/suggest?q=~a&rich=false&source=web"
   :processing-function
   #'(lambda (results)
       (when results
         (second (json:decode-json-from-string results))))
   :request-args request-args))

(define-search-engine brave
    (:shortcut "brave"
     :fallback-url (quri:uri "https://search.brave.com/")
     :base-search-url "https://search.brave.com/search?q=~a"
     :completion-function (make-brave-completion)
     :documentation "Brave Search is built on top of a completely independent index, and doesn’t track users, their searches, or their clicks.")
  (timeframe "tf" ((:any "")
                   (:day "pd")
                   (:past-day "pd")
                   (:week "pw")
                   (:past-week "pw")
                   (:month "pm")
                   (:past-month "pm")
                   (:year "py")
                   (:past-year "py"))))

(export-always 'make-duckduckgo-completion)
(defun make-duckduckgo-completion (&key request-args)
  "Helper that generates DuckDuckGo search completion functions. The only
thing that's left to pass to it is REQUEST-ARGS to slightly modify the
request."
  (make-search-completion-function
   :base-url "https://duckduckgo.com/ac/?q=~a"
   :processing-function
   #'(lambda (results)
       (when results
         (mapcar #'cdar
                 (json:decode-json-from-string results))))
   :request-args request-args))

(define-search-engine duckduckgo
    (:shortcut "duckduckgo"
     :fallback-url (quri:uri "https://duckduckgo.com/")
     :base-search-url "https://duckduckgo.com/?q=~a"
     :completion-function (make-duckduckgo-completion)
     :documentation "DuckDuckGo `nyxt:search-engine' with the configuration as capable as the built-in settings pane.
See DuckDuckGo settings for the names of the necessary setting and use
the matching kebab-case keywords for this helper.")
  ;; DuckDuckGo uses two (four?) object-related keywords.
  (object "ia" ((:all "web")
                (:images "images")
                (:videos "videos")
                (:news "news")
                (:meanings "meanings")))
  (object2 "iax" ((:all "web")
                  (:images "images")
                  (:videos "videos")
                  (:news "news")
                  (:meanings "meanings")))
  ;;; Theming
  (theme "kae" ((:default  "")
                (:basic    "b")
                (:contrast "c")
                (:dark     "d")
                (:gray     "g")
                (:terminal "t")))
  ;;; Privacy
  (get-requests "kg" ((t "")
                      (nil "p"))) ;; Use POST requests.
  (video-playback "k5" ((:prompt-me "")
                        (:always-ddg "1")
                        (:always-third-party "2")))
  ;; See https://help.duckduckgo.com/results/rduckduckgocom/
  (redirect "kd" ((t "")
                  (nil "-1")))
  ;;; General
  ;; abbr. means the abbreviation I came up with based on what DDG
  ;; provides. It's mostly just removing the -en suffix from the
  ;; region name, like :india-en -> :india
  (region "kl" ((:all "")
                (:argentina   "ar-es")
                (:australia   "au-en")
                (:austria     "at-de")
                (:belgium-fr  "be-fr")
                (:belgium-nl  "be-nl")
                (:brazil      "br-pt")
                (:bulgaria    "bg-bg")
                (:canada-en   "ca-en")
                (:canada-fr   "ca-fr")
                (:catalonia   "ct-ca")
                (:chile       "cl-es")
                (:china       "cn-zh")
                (:colombia    "co-es")
                (:croatia     "hr-hr")
                (:czech-republic "cz-cs")
                (:denmark     "dk-da")
                (:estonia     "ee-et")
                (:finland     "fi-fi")
                (:france      "fr-fr")
                (:germany     "de-de")
                (:greece      "gr-el")
                (:hong-kong   "hk-tzh")
                (:hungary     "hu-hu")
                (:india-en    "in-en")
                (:india       "in-en")   ; abbr.
                (:indonesia-en "id-en")
                (:indonesia   "id-en")   ; abbr.
                (:ireland     "ie-en")
                (:israel-en   "il-en")
                (:israel      "il-en")   ; abbr.
                (:italy       "it-it")
                (:japan       "jp-jp")
                (:korea       "kr-kr")
                (:latvia      "lv-lv")
                (:lithuania   "lt-lt")
                (:malaysia-en "my-en")
                (:malaysia    "my-en")   ; abbr.
                (:mexico      "mx-es")
                (:netherlands "nl-nl")
                (:new-zealand "nz-en")
                (:norway      "no-no")
                (:pakistan-en "pk-en")
                (:peru        "pe-es")
                (:philippines-en "ph-en")
                (:philippines "ph-en")   ; abbr.
                (:poland      "pl-pl")
                (:portugal    "pt-pt")
                (:romania     "ro-ro")
                (:russia      "ru-ru")
                (:russian-federation "ru-ru") ; abbr.
                (:saudi-arabia "xa-ar")
                (:singapore   "sg-en")
                (:slovakia    "sk-sk")
                (:slovenia    "sl-sl")
                (:south-africa "za-en")
                (:spain-ca    "es-ca")
                (:spain-es    "es-es")
                (:spain       "es-es")   ; abbr.
                (:sweden      "se-sv")
                (:switzerland-de "ch-de")
                (:switzerland-fr "ch-fr")
                (:taiwan "tw-tzh")
                (:thailand-en "th-en")
                (:thailand    "th-en")   ; abbr.
                (:turkey      "tr-tr")
                (:us-english  "us-en")
                (:us-en       "us-en")   ; abbr.
                (:us          "us-en")   ; abbr.
                (:us-spanish  "us-es")
                (:us-es       "us-es")   ; abbr.
                (:ukraine     "ua-uk")
                (:united-kingdom "uk-en")
                (:uk "uk-en")            ; abbr.
                (:vietnam-en  "vn-en")
                (:vietnam     "vn-en"))) ; abbr.
  ;; TODO: Write it.
  (language "kad" ((:default "")))
  (safe-search "kp" ((:moderate "")
                     (:strict   "1")
                     (:off      "-2")))
  (instant-answers "kz" ((t "")
                         (nil "-1")))
  (infinite-scroll-for-media "kav" ((t "")
                                    (nil "-1")))
  (infinite-scroll "kav" ((nil "")
                          (t "1")))
  (autocomplete-suggestions "kac" ((t "")
                                   (nil "-1")))
  (open-in-new-tab "kn" ((nil "")
                         (t   "1")))
  (advertisements "k1"  ((t "")
                         (nil "-1")))
  (keyboard-shortcuts "kk" ((t "")
                            (nil "-1")))
  (units-of-measure "kaj" ((:no-preference "")
                           (:metric "m")
                           (:us-based "u")))
  (map-rendering "kay" ((:not-set "")
                        (:best-available "b")
                        (:image-tiles "i")))
  (page-break-numbers "kv" ((:on "")
                            (:off "-1")
                            (:lines "l")))
  (install-duckduckgo "kak" ((t "")
                             (nil "-1")))
  (install-reminders "kax" ((t "")
                            (nil "-1")))
  (privacy-newsletter "kaq" ((t "")
                             (nil "-1")))
  (newsletter-reminders "kap" ((t "")
                               (nil "-1")))
  (homepage-privacy-tips "kao" ((t "")
                                (nil "-1")))
  (help-improve-duckduckgo "kau" ((t "")
                                  (nil "-1")))
  ;;; Appearance
  (font "kt" (("Proxima Nova" "")
              (:proxima-nova  "")
              ("Arial" "a")
              (:arial "a")
              ("Century Gothic" "c")
              (:century-gothic "c")
              ("Georgia" "g")
              (:georgia "g")
              ("Helvetica" "h")
              (:helvetica "h")
              ("Helvetica Neue" "u")
              (:helvetica-neue "u")
              ("Sans Serif" "n")
              (:sans-serif "n")
              ("Segoe UI" "e")
              (:segoe-ui "e")
              ("Serif" "s")
              (:serif "s")
              ("Times" "t")
              (:times "t")
              ("Tahoma" "o")
              (:tahoma "o")
              ("Trebuchet MS" "b")
              (:trebuchet-ms "b")
              ("Verdana" "v")
              (:verdana "v")))
  (font-size "ks" ((:large "")
                   (:small "s")
                   (:medium "m")
                   (:larger "l")
                   (:largest "t")))
  (page-width "kw" ((:normal "")
                    (:wide "w")
                    (:super-wide "s")))
  (center-alignment "km" ((nil "")
                          (t "m")))
  (background-color "k7" ((:default "ffffff")))
  (header-behavior "ko" ((:on-dynamic "")
                         (:on-fixed "1")
                         (:off "-1")
                         (:on-scrolling "s")))
  (header-color "kj" ((:default "ffffff")))
  (result-title-font "ka" (("Proxima Nova" "")
                           (:proxima-nova  "")
                           ("Arial" "a")
                           (:arial "a")
                           ("Century Gothic" "c")
                           (:century-gothic "c")
                           ("Georgia" "g")
                           (:georgia "g")
                           ("Helvetica" "h")
                           (:helvetica "h")
                           ("Helvetica Neue" "u")
                           (:helvetica-neue "u")
                           ("Sans Serif" "n")
                           (:sans-serif "n")
                           ("Segoe UI" "e")
                           (:segoe-ui "e")
                           ("Serif" "s")
                           (:serif "s")
                           ("Times" "t")
                           (:times "t")
                           ("Tahoma" "o")
                           (:tahoma "o")
                           ("Trebuchet MS" "b")
                           (:trebuchet-ms "b")
                           ("Verdana" "v")
                           (:verdana "v")))
  (result-title-color "k9" ((:default "084999")))
  (result-visited-title-color "kaa" ((:default "6c00a2")))
  (result-title-underline "ku" ((nil "")
                                (t "1")))
  (result-description-color "k8" ((:default "494949")))
  (result-url-color "kx" ((:default "3f6e35")))
  (result-module-color "k21" ((:default "ffffff")))
  (result-full-urls "kaf" ((t "")
                           (nil "-1")))
  (result-urls-above-snippet "kaf" ((t "")
                                    (nil "-1")))
  (result-visible-checkmark "k18" ((nil "")
                                   (t "1")))
  (site-icons "kf" ((t "")
                    (nil "-1"))))

(define-derived-search-engine duckduckgo-images
    (duckduckgo :object :images :object2 :images))

(define-derived-search-engine duckduckgo-html-only
    (duckduckgo :base-search-url "https://html.duckduckgo.com/html/?q=~a"
                :fallback-url (quri:uri "https://html.duckduckgo.com/html/")))

(define-search-engine frogfind
    (:shortcut "frogfind"
     :fallback-url (quri:uri "http://frogfind.com/")
     :base-search-url "http://frogfind.com/?q=~a"
     :documentation "A search engine targeting vintage computers.
No completion, no HTTPS, no search customization, but simple and easy to parse search results.

Don't forget to add ((match-host \"frogfind.com\")  :excluded (force-https-mode))
to your auto-mode-rules.lisp file, because FrogFind is HTTP-only."))

(define-search-engine searchmysite
    (:shortcut "searchmysite"
     :fallback-url (quri:uri "https://searchmysite.net/")
     :base-search-url "https://searchmysite.net/search/?q=~a"
     :documentation
     "Open source search engine and search as a service for personal and independent websites."))

(export-always 'make-google-completion)
(defun make-google-completion (&key request-args)
  "Helper that generates Google search completion functions. The only
thing that's left to pass to it is REQUEST-ARGS to slightly modify the
request."
  (make-search-completion-function
   :base-url "https://www.google.com/complete/search?q=~a&client=gws-wiz"
   :processing-function
   #'(lambda (results)
       (mapcar (alexandria:compose (alexandria:curry #'str:replace-using '("<b>" "" "</b>" ""))
                                   #'first)
               (first (json:decode-json-from-string
                       (str:replace-first "window.google.ac.h(" "" results)))))
   :request-args request-args))

(defvar *google-countries*
  '((:default "")
    (:afghanistan "AF")
    (:albania "AL")
    (:algeria "DZ")
    (:american-samoa "AS")
    (:andorra "AD")
    (:anguilla "AI")
    (:antartica "AQ")
    (:antigua-and-barbuda "AG")
    (:argentina "AR")
    (:armenia "AM")
    (:aruba "AW")
    (:australia "AU")
    (:austria "AT")
    (:azerbaijan "AZ")
    (:bahamas "BS")
    (:bahrain "BH")
    (:bangladesh "BD")
    (:barbados "BB")
    (:belarus "BY")
    (:belgium "BE")
    (:belize "BZ")
    (:benin "BJ")
    (:bermuda "BM")
    (:bhutan "BT")
    (:bolivia "BO")
    (:bosnia "BA")
    (:botswana "BW")
    (:bouvet-island "BV")
    (:brazil "BR")
    (:british-indian-ocean "IO")
    (:brunei "BN")
    (:bulgaria "BG")
    (:burkina-faso "BF")
    (:burundi "BI")
    (:cambodia "KH")
    (:cameroon "CM")
    (:canada "CA")
    (:cape-verde "CV")
    (:cayman-islands "KY")
    (:central-african-republic "CF")
    (:chad "TD")
    (:chile "CL")
    (:china "CN")
    (:christmas-island "CX")
    (:cocos-islands "CC")
    (:colombia "CO")
    (:comoros "KM")
    (:congo "CG")
    (:democratic-replublic-of-congo "CD")
    (:cook-islands "CK")
    (:costa-rica "CR")
    (:cote-divoire "CI")
    (:croatia "HR")
    (:cuba "CU")
    (:cyprus "CY")
    (:czech-republic "CZ")
    (:denmark "DK")
    (:djibouti "DJ")
    (:dominica "DM")
    (:dominican-republic "DO")
    (:east-timor "TP")
    (:ecuador "EC")
    (:egypt "EG")
    (:el-salvador "SV")
    (:equatorial-guinea "GQ")
    (:eritrea "ER")
    (:estonia "EE")
    (:ethiopia "ET")
    (:european-union "EU")
    (:falkland-islands "FK")
    (:faroe-islands "FO")
    (:fiji "FJ")
    (:finland "FI")
    (:france "FR")
    (:france-metropolitan "FX")
    (:french-guiana "GF")
    (:french-polynesia "PF")
    (:french-southern-territories "TF")
    (:gabon "GA")
    (:gambia "GM")
    (:georgia "GE")
    (:germany "DE")
    (:ghana "GH")
    (:gibraltar "GI")
    (:greece "GR")
    (:greenland "GL")
    (:grenada "GD")
    (:guadeloupe "GP")
    (:guam "GU")
    (:guatemala "GT")
    (:guinea "GN")
    (:guinea-bissau "GW")
    (:guyana "GY")
    (:haiti "HT")
    (:heard-island-mcdonald-islands "HM")
    (:vatican-city "VA")
    (:honduras "HN")
    (:hong-kong "HK")
    (:hungary "HU")
    (:iceland "IS")
    (:india "IN")
    (:indonesia "ID")
    (:iran "IR")
    (:iraq "IQ")
    (:ireland "IE")
    (:israel "IL")
    (:italy "IT")
    (:jamaica "JM")
    (:japan "JP")
    (:jordan "JO")
    (:kazakhstan "KZ")
    (:kenya "KE")
    (:kiribati "KI")
    (:democratic-peoples-republic-of-korea "KP")
    (:republic-of-korea "KR")
    (:kuwait "KW")
    (:kyrgyzstan "KG")
    (:lao "LA")
    (:latvia "LV")
    (:lebanon "LB")
    (:lesotho "LS")
    (:liberia "LR")
    (:libyan-arab-jamahiriya "LY")
    (:liechtenstein "LI")
    (:lithuania "LT")
    (:luxembourg "LU")
    (:macao "MO")
    (:macedonia "MK")
    (:madagascar "MG")
    (:malawi "MW")
    (:malaysia "MY")
    (:maldives "MV")
    (:mali "ML")
    (:malta "MT")
    (:marshall-islands "MH")
    (:martinique "MQ")
    (:mauritania "MR")
    (:mauritius "MU")
    (:mayotte "YT")
    (:mexico "MX")
    (:micronesia "FM")
    (:moldova "MD")
    (:monaco "MC")
    (:mongolia "MN")
    (:montserrat "MS")
    (:morocco "MA")
    (:mozambique "MZ")
    (:myanmar "MM")
    (:namibia "NA")
    (:nauru "NR")
    (:nepal "NP")
    (:netherlands "NL")
    (:netherlands-antilles "AN")
    (:new-caledonia "NC")
    (:new-zealand "NZ")
    (:nicaragua "NI")
    (:niger "NE")
    (:nigeria "NG")
    (:niue "NU")
    (:norkfolk-island "NF")
    (:northern-mariana-islands "MP")
    (:norway "NO")
    (:oman "OM")
    (:pakistan "PK")
    (:palau "PW")
    (:palestinian-territory "PS")
    (:panama "PA")
    (:papua-new-guinea "PG")
    (:paraguay "PY")
    (:peru "PE")
    (:philippines "PH")
    (:pitcairn "PN")
    (:poland "PL")
    (:portugal "PT")
    (:puerto-rico "PR")
    (:qatar "QA")
    (:reunion "RE")
    (:romania "RO")
    (:russian-federation "RU")
    (:rwanda "RW")
    (:saint-helena "SH")
    (:saint-kitts-and-nevis "KN")
    (:saint-lucia "LC")
    (:saint-pierre-and-miquelon "PM")
    (:saint-vincent-and-the-grenadines "VC")
    (:samoa "WS")
    (:san-marino "SM")
    (:sao-tome-and-principe "ST")
    (:saudi-arabia "SA")
    (:senagal "SN")
    (:serbia-and-montenegro "CS")
    (:seychelles "SC")
    (:sierra-leone "SL")
    (:singapore "SG")
    (:slovakia "SK")
    (:slovenia "SI")
    (:solomon-islands "SB")
    (:somalia "SO")
    (:south-africa "ZA")
    (:south-georgia "GS")
    (:spain "ES")
    (:sri-lanka "LK")
    (:sudan "SD")
    (:suriname "SR")
    (:svalbard-and-jan-mayen "SJ")
    (:swaziland "SZ")
    (:sweden "SE")
    (:switzerland "CH")
    (:syrian-arab-republic "SY")
    (:taiwan "TW")
    (:tajikistan "TJ")
    (:tanzania "TZ")
    (:thailand "TH")
    (:togo "TG")
    (:tokelau "TK")
    (:tonga "TO")
    (:trinidad-and-tobago "TT")
    (:tunisia "TN")
    (:turkey "TR")
    (:turkmenistan "TM")
    (:turks-and-caicos-islands "TC")
    (:tuvalu "TV")
    (:uganda "UG")
    (:ukraine "UA")
    (:united-arab-emirates "AE")
    (:united-kingdom "UK")
    (:united-states "US")
    (:united-states-minor-outlying-islands "UM")
    (:uruguay "UY")
    (:uzbekistan "UZ")
    (:vanuatu "VU")
    (:venezuela "VE")
    (:vietnam "VN")
    (:british-virgin-islands "VG")
    (:us-virgin-islands "VI")
    (:wallis-and-futuna "WF")
    (:western-sahara "EH")
    (:yemen "YE")
    (:yugoslavia "YU")
    (:zambia "ZM")
    (:zimbabwe "ZW")))

(defvar *google-languages*
  '((:default "")
    (:english "en")
    (:afrikaans "af")
    (:arabic "ar")
    (:armenian "hy")
    (:belarusian "be")
    (:catalan "ca")
    (:chinese-simplified "zh-CN")
    (:chinese-traditional "zh-TW")
    (:croatian "hr")
    (:czech "cs")
    (:danish "da")
    (:dutch "nl")
    (:esperanto "eo")
    (:estonian "et")
    (:filipino "tl")
    (:finnish "fi")
    (:french "fr")
    (:german "de")
    (:greek "el")
    (:hebrew "iw")
    (:hindi "hi")
    (:hungarian "hu")
    (:icelandic "is")
    (:indonesian "id")
    (:italian "it")
    (:japanese "ja")
    (:korean "ko")
    (:latvian "lv")
    (:lithuanian "lt")
    (:norwegian "no")
    (:persian "fa")
    (:polish "pl")
    (:portuguese "pt")
    (:romanian "ro")
    (:russian "ru")
    (:serbian "sr")
    (:sinhala "si")
    (:slovak "sk")
    (:slovenian "sl")
    (:spanish "es")
    (:swahili "sw")
    (:swedish "sv")
    (:thai "th")
    (:turkish "tr")
    (:ukranian "uk")
    (:vietnamese "vi")
    (:xhosa "xh")
    (:zulu "zu")))

(defun compute-google-lang (code)
  "Returns the corresponding language value from CODE."
  (car (alexandria:assoc-value *google-languages* code :test #'equal)))

(defun compute-edit-google-lang (code)
  "Returns the corresponding language value from CODE and tweaks it."
  (concatenate 'string "lang_" (compute-google-lang code)))

(defun compute-google-country (code)
  "Returns the corresponding country value from CODE."
  (car (alexandria:assoc-value *google-countries* code :test #'equal)))

(defun compute-edit-google-country (code)
  "Returns the corresponding country value from CODE and tweaks it."
  (concatenate 'string "country" (compute-google-country code)))

(define-search-engine google
    (:shortcut "google"
     :fallback-url (quri:uri "https://google.com")
     :base-search-url "https://google.com/search?q=~a"
     :completion-function (make-google-completion)
     :documentation "Google `nyxt:search-engine'.
Does not support advanced results sorting as of now.
Arguments:
SAFE-SEARCH -- Whether results will be filtered. Boolean. t to enable,
nil to disable.
OBJECT -- One of :all :image, :video, :news, :shopping, :books,
:finance.
EXTRA-FILTERS -- Additional search filters.
RESULTS-START -- Displays the search results starting from the given position.
RESULTS-NUMBER -- Number of results to display on a single page.
NEAR-CITY -- Display results near a provided city.
PERSONALIZED-SEARCH -- Whether to show personalized results.
FILETYPE -- Narrow down results to a given file type.
FILETYPE-RULE -- Whether to include or exclude the provided FILETYPE from the results.
SITE -- Narrow down results to a given site.
SITE-RULE - Whether to include or exclude the provided SITE from the results.
EXCLUDE-TERMS -- Removes unwanted whitespace-separated words from the search results.
ACCESS-RIGHTS -- Show results with given license.
NEW-WINDOW -- Open links in a new tab.
FILTER -- Removes the omitted results or similar results filter and allows all
results to be shown.
LANG-RESULTS -- Search results language.
LANG-UI -- Interface language.
COUNTRY-RESULTS -- Use the given country for the search results.
COUNTRY-UI -- Use the given country for the search interface.
COORDINATES -- Search for results near the given coordinates.
DATE-RESULTS -- Filter results by a specified date range.")
  (safe-search "safe" ((t   "strict")
                       (nil "images")))
  (object "tbm" ((:all      "")
                 (:image    "isch")
                 (:video    "vid")
                 (:news     "nws")
                 (:shopping "shop")
                 (:books    "bks")
                 (:finance  "fin")))
  (extra-filters "tbs" ((:sort-by-relevance "")
                        (:sort-by-date "sbd:1")
                        (:archived "ar:1")
                        (:show-duplicates "nsd:1")
                        (:verbatim "li:1")))
  (results-start "start" ((:default 0)))
  (results-number "num" ((:default 10)))
  (near-city "near" ((:default "")))
  (personalized-search "pws" ((t "")
                              (nil "0")))
  (filetype "as_filetype" ((:default "")))
  (filetype-rule "as_ft" ((:include "i")
                          (:exclude "e")))
  (site "as_sitesearch" ((:default "")))
  (site-rule "as_dt" ((:include "i")
                      (:exclude "e")))
  (exclude-terms "as_eq" ((:default "")))
  (access-rights "as_rights" ((:all "")
                              (:cc0 "cc_publicdomain")
                              (:by "cc_attribute")
                              (:by-sa "cc_sharealike")
                              (:by-nc "cc_noncommercial")
                              (:by-nd "cc_nonderived")))
  (new-window "newwindow" ((nil "")
                           (t "1")))
  (filter "filter" ((t "")
                    (nil "0")))
  (lang-results "lr" (:function #'compute-edit-google-lang))
  (lang-ui "hl" (:function #'compute-google-lang))
  (country-results "cr"  (:function #'compute-edit-google-country))
  (country-ui "gl" (:function #'compute-google-country))
  (coordinates "gll" ((:default "")))
  (date-results "as_qdr" ((:default "")
                          (:past-hour "h")
                          (:past-day "d")
                          (:past-week "w")
                          (:past-month "m")
                          (:past-year "y"))))

(define-derived-search-engine google-images
    (google :object :image))

(export-always 'make-google-scholar-completion)
(defun make-google-scholar-completion (&key request-args)
  "Helper that generates Google Scholar completion functions. The only
thing that's left to pass to it is REQUEST-ARGS to slightly modify the
request."
  (make-search-completion-function
   :base-url "https://scholar.google.com/scholar_complete?q=~a"
   :processing-function
   #'(lambda (results)
       (mapcar (lambda (completion) (remove #\| completion))
               (alexandria:assoc-value (json:decode-json-from-string results) :l)))
   :request-args request-args))

(define-search-engine google-scholar
    (:shortcut "google-scholar"
     :fallback-url (quri:uri "https://scholar.google.com")
     :base-search-url "https://scholar.google.com/scholar?q=~a"
     :completion-function (make-google-scholar-completion)
     :documentation "Google Scholar `nyxt:search-engine'.
Arguments:
STARTING-TIME -- the year since which to search publications.
ENDING-TIME -- the year until which the found publications should span.
SORT-BY -- how to sort the results. Possible values are :RELEVANCE (default) and :DATE.
SEARCH-TYPE -- :ANY for all the papers, :REVIEW to only list review papers.")
  (starting-time "as_ylo" ((:any "")))
  (ending-time "as_yhi" ((:any "")))
  (sort-by "scisbd" ((:relevance "")
                     (:date "1")))
  (search-type "as_rr" ((:any "")
                        (:review "1"))))

(define-search-engine whoogle
    (:shortcut "whoogle"
     :fallback-url (quri:uri "https://gowogle.voring.me")
     :base-search-url "https://gowogle.voring.me/search?q=~a"
     :completion-function (make-google-completion)
     :documentation "`nyxt:search-engine' for Whoogle, a self-hosted, ad-free,
privacy-respecting metasearch engine which takes results from Google. Most Whoogle instances
disable user configuration settings, so setting the URL parameters is the only way to customize them.
Most of `google' engine's parameters are supported, and some have a Whoogle counterpart:
- lr -> lang_search
- hl -> lang_interface
- gl -> country
- newwindow -> new_tab")
  (object "tbm" ((:all "")
                 (:image "isch")
                 (:video "vid")
                 (:news "nws")
                 (:shopping "shop")
                 (:books "bks")
                 (:finance "fin")))
  (extra-filters "tbs" ((:sort-by-relevance "")
                        (:sort-by-date "sbd:1")
                        (:archived "ar:1")
                        (:show-duplicates "nsd:1")
                        (:verbatim "li:1")))
  ;; Results starting value (10 per page)
  (results-start "start" ((:default "")))
  (near-city "near" ((:default "")))
  ;; Exclude returned results from auto-corrected queries
  (exclude-autocorrect "nfpr" ((nil "")
                               (t "1")))
  (lang-results "lang_search" (:function #'compute-edit-google-lang))
  (lang-ui "lang_interface" (:function #'compute-edit-google-lang))
  (country "country" (:function #'compute-google-country))
  (theme "theme" ((:system "system")
                  (:dark "dark")
                  (:light "light")))
  ;; Whether to use alternative front-ends
  (alternatives "alts" ((nil "")
                        (t "1")))
  (new-tab "new_tab" ((nil "")
                      (t "1")))
  ;; Add a "View Image" button in the Images tab
  (view-image "view_image" ((nil "")
                            (t "1")))
  ;; Comma-separated list of blocked sites
  (blocked-sites "block" ((:default "")))
  (safe-search "safe" ((t "strict")
                       (nil "images")))
  (no-javascript "nojs" ((nil "")
                         (t "1")))
  (anonymous-view "anon_view" ((nil "")
                               (t "1")))
  (cookies-disabled "cookies_disabled" ((nil "")
                                        (t "1")))
  (date-results "as_qdr" ((:default "")
                          (:past-hour "h")
                          (:past-day "d")
                          (:past-week "w")
                          (:past-month "m")
                          (:past-year "y")))
  ;; Search chips in Images tab
  (chips "chips" ((:default ""))))

(export-always 'bing-date)
(-> bing-date (local-time:timestamp local-time:timestamp) string)
(defun bing-date (start-date end-date)
  "Helper function generating Bing-acceptable dates in the form \"ez5_START-DATE_END-DATE\".
Use it for the value of :date argument to `bing'"
  (format nil "\"ez5_~d_~d\""
          (local-time:day-of
           (local-time:timestamp-
            start-date (local-time:day-of (local-time:unix-to-timestamp 0)) :day))
          (local-time:day-of
           (local-time:timestamp-
            end-date (local-time:day-of (local-time:unix-to-timestamp 0)) :day))))

(define-search-engine bing
    (:shortcut "bing"
     :fallback-url (quri:uri "https://www.bing.com/")
     :base-search-url "https://www.bing.com/search?q=~a"
     :documentation "`nyxt:search-engine' for Bing.
MY-LANGUAGE-ONLY and MY-COUNTRY-ONLY are pre-defined by Bing based on
your location. Both are booleans.
DATE is either :all, :past-24-hours, :past-week, :past-month,
:past-year or an arbitrary bing-acceptable time string that you can
generate with `bing-date'.")
  (my-language-only "lf" ((nil "")
                          (t "1")))
  (my-countly-only "rf" ((nil "")
                         (t "1")))
  (date "filters" ((:all "")
                   (:past-24-hours "\"ez1\"")
                   (:past-week "\"ez2\"")
                   (:past-month "\"ez3\"")
                   (:past-year (bing-date (local-time:timestamp- (local-time:now) 1 :year)
                                          (local-time:now))))))

(define-search-engine bing-images
    (:shortcut "bing-images"
     :fallback-url (quri:uri "https://www.bing.com/")
     :base-search-url "https://www.bing.com/images/search?q=~a"
     :documentation "`nyxt:search-engine' for Bing Images."))

(define-search-engine bing-videos
    (:shortcut "bing-videos"
     :fallback-url (quri:uri "https://www.bing.com/")
     :base-search-url "https://www.bing.com/videos/search?q=~a"
     :documentation "`nyxt:search-engine' for Bing Videos."))

(define-search-engine bing-maps
    (:shortcut "bing-maps"
     :fallback-url (quri:uri "https://www.bing.com/")
     :base-search-url "https://www.bing.com/maps/search?q=~a"
     :documentation "`nyxt:search-engine' for Bing Maps."))

(define-search-engine bing-news
    (:shortcut "bing-news"
     :fallback-url (quri:uri "https://www.bing.com/")
     :base-search-url "https://www.bing.com/news/search?q=~a"
     :documentation "`nyxt:search-engine' for Bing News.

INTERVAL is the time since news publishing. It can be one of :all,
:past-5-minutes, :past-15-minutes, :past-30-minutes, :past-hour,
:past-4-hours, :past-6-hours, :past-24-hours, :past-day, :past-7-days,
:past-week, :past-30-days, :past-month. Yes, some are duplicated.")
  (interval "qft" ((:all "")
                   (:past-5-minutes "interval=\"1\"")
                   (:past-15-minutes "interval=\"2\"")
                   (:past-30-minutes "interval=\"3\"")
                   (:past-hour "interval=\"4\"")
                   (:past-4-hours "interval=\"5\"")
                   (:past-6-hours "interval=\"6\"")
                   (:past-24-hours "interval=\"7\"")
                   (:past-day "interval=\"7\"")
                   (:past-7-days "interval=\"8\"")
                   (:past-week "interval=\"8\"")
                   (:past-30-days "interval=\"9\"")
                   (:past-month "interval=\"9\""))))

(define-search-engine bing-shopping
    (:shortcut "bing-shopping"
     :fallback-url (quri:uri "https://www.bing.com/")
     :base-search-url "https://www.bing.com/shopping/search?q=~a"
     :documentation "`nyxt:search-engine' for Bing Shopping."))

(define-search-engine wordnet (:shortcut "wordnet"
                               :fallback-url (quri:uri "http://wordnetweb.princeton.edu/perl/webwn")
                               :base-search-url "http://wordnetweb.princeton.edu/perl/webwn?s=~a"
                               :documentation "`nyxt:search-engine' for WordNet.

To use it, disable force-https-mode for wordnetweb.princeton.edu or
add auto-mode rule that will manage that for you:

((match-host \"wordnetweb.princeton.edu\") :excluded (nyxt/force-https-mode:force-https-mode))

Arguments mean:
SHORTCUT -- the shortcut you need to input to use this search engine. Set to \"wordnet\" by default.
FALLBACK-URL -- what URL to follow if there was a search error.
SHOW-EXAMPLES -- Show example sentences. T by default.
SHOW-GLOSSES -- Show definitions. T by default.
SHOW-WORD-FREQUENCIES -- Show word frequency counts. NIL by default.
SHOW-DB-LOCATIONS -- Show WordNet database locations for this word. NIL by default.
SHOW-LEXICAL-FILE-INFO -- Show lexical file word belongs to. NIL by default.
SHOW-LEXICAL-FILE-NUMBERS -- Show number of the word in the lexical file. NIL by default.
SHOW-SENSE-KEYS -- Show symbols for senses of the word. NIL by default.
SHOW-SENSE-NUMBERS -- Show sense numbers. NIL by default.

A sensible non-default example:
\(wordnet :shortcut \"wn\"
         :show-examples nil
         :show-word-frequencies t
         :show-sense-numbers t)

This search engine, invokable with \"wn\", will show:
- NO example sentences,
- glosses,
- frequency counts,
- sense-numbers.")
  (show-examples             "o0" ((t "1")  (nil "")))
  (show-glosses              "o1" ((t "1")  (nil "")))
  (show-word-frequencies     "o2" ((nil "") (t "1")))
  (show-db-locations         "o3" ((nil "") (t "1")))
  (show-lexical-file-info    "o4" ((nil "") (t "1")))
  (show-lexical-file-numbers "o5" ((nil "") (t "1")))
  (show-sense-keys           "o6" ((nil "") (t "1")))
  (show-sense-numbers        "o7" ((nil "") (t "1"))))

(export-always 'make-wikipedia-completion)
(-> make-wikipedia-completion (&key (:suggestion-limit fixnum)
                                    (:request-args list)
                                    (:namespace (member :general
                                                        :talk
                                                        :user
                                                        :user-talk
                                                        :wikipedia
                                                        :wikipedia-talk
                                                        :file
                                                        :file-talk
                                                        :media-wiki
                                                        :media-wiki-talk
                                                        :template
                                                        :template-talk
                                                        :help
                                                        :help-talk
                                                        :category
                                                        :category-talk)))
    function)
(defun make-wikipedia-completion (&key (suggestion-limit 10) (namespace :general) request-args)
  "Helper completion function for Wikipedia.
SUGGESTION-LIMIT is how much suggestions you want to get.
NAMESPACE is the Wikipedia-namespace to search in. Acceptable values
are: :general, :talk, :user, :user-talk, :wikipedia, :wikipedia-talk,
:file, :file-talk, :media-wiki, :media-wiki-talk, :template,
:template-talk, :help, :help-talk, :category, and :category-talk.

REQUEST-ARGS are additional request function arguments,
for example '(proxy \"socks5://localhost:9050\") for proxying."
  (make-search-completion-function
   :base-url (str:concat "https://en.wikipedia.org/w/api.php?action=opensearch&format=json&search=~a"
                         (format nil "&limit=~d&namespace=~d"
                                 suggestion-limit
                                 (position namespace (list :general :talk
                                                           :user :user-talk
                                                           :wikipedia :wikipedia-talk
                                                           :file :file-talk
                                                           :media-wiki :media-wiki-talk
                                                           :template :template-talk
                                                           :help :help-talk
                                                           :category :category-talk))))
   :processing-function
   #'(lambda (results)
       (when results
         (second (json:decode-json-from-string results))))
   :request-args request-args))

(define-search-engine wikipedia
    (:shortcut "wikipedia"
     :base-search-url "https://en.wikipedia.org/w/index.php?search=~a"
     :fallback-url (quri:uri "https://en.wikipedia.org/")
     :completion-function (make-wikipedia-completion)))

(export-always 'make-yahoo-completion)
(defun make-yahoo-completion (&key request-args (suggestion-limit 10))
  "Completion helper for Yahoo! search engine.
SUGGESTION-LIMIT is how much suggestions you want to see.
REQUEST-ARGS is a list of args to pass to request function."
  (make-search-completion-function
   :base-url (str:concat "https://search.yahoo.com/sugg/gossip/gossip-us-ura/?command=~a&output=sd1"
                         (format nil "&nresults=~d" suggestion-limit))
   :processing-function
   #'(lambda (results)
       (when results
         (mapcar #'cdar
                 (alexandria:assoc-value
                  (json:decode-json-from-string
                   (ppcre:regex-replace "YAHOO.*\\(" results ""))
                  :r))))
   :request-args request-args))

(define-search-engine yahoo
    (:shortcut "yahoo"
     :fallback-url (quri:uri "https://search.yahoo.com/")
     :base-search-url "https://search.yahoo.com/search?p=~a"
     :completion-function (make-yahoo-completion)
     :documentation "Yahoo! `nyxt:search-engine'.")
  (number-of-results "n" ((:default "10")))
  (encoding "ei" ((:utf "UTF-8")))
  (domain "vs" ((:any "")
                (:dot-com ".com")
                (:dot-edu ".edu")
                (:dot-gov ".gov")
                (:dot-org ".org")))
  (date "btf" ((:past-day "d")
               (:past-week "d")
               (:past-month "m"))))

(define-search-engine scihub
    (:shortcut "scihub"
     :fallback-url (quri:uri "https://sci-hub.hkvisa.net")
     :base-search-url "https://sci-hub.hkvisa.net/~a"
     :documentation "Sci-Hub `nyxt:search-engine' for research papers"))

(define-search-engine searx
    (:shortcut "searx"
     :fallback-url (quri:uri "https://searx.bar")
     :base-search-url "https://searx.bar/search?q=~a"
     :documentation "SearX `nyxt:search-engine'.")
  (categories "categories" ((:general "general")
                            (:images "images")
                            (:files "files")
                            (:it "it")
                            (:map "map")
                            (:music "music")
                            (:news "news")
                            (:science "science")
                            (:social-media "social media")
                            (:videos "videos")))
  (language "language" ((:default "")))
  (time-range "time_range" ((:default "")
                            (:day "day")
                            (:week "week")
                            (:month "month")
                            (:year "year"))))

(export-always 'startpage-image-size)
(-> startpage-image-size (integer) (or integer string))
(defun startpage-image-size (input)
  (if (plusp input)
      input
      (progn (log:warn "The value specified for IMAGES-SIZE-EXACT-WIDTH or IMAGES-SIZE-EXACT-HEIGHT
 is not a positive integer. Defaulting to empty value")
             "")))

(-> startpage-settings-string (string) string)
(defun startpage-settings-string (input)
  (if (ppcre:scan "[0-9a-fA-F]{150,165}" input)
      input
      (progn (log:warn "The value specified for SETTINGS-STRING is not valid.
 Defaulting to empty value")
             "")))

(defun make-startpage-completion (&key request-args)
  "Helper that generates Startpage search completion functions.
REQUEST-ARGS is a list of args to pass to request function."
  (make-search-completion-function
   :base-url "https://startpage.com/suggestions?q=~a"
   :processing-function
   #'(lambda (results)
       (when results
         (mapcar #'cdar
                 (cddadr (json:decode-json-from-string results)))))
   :request-args request-args))

(define-search-engine startpage
    (:shortcut "startpage"
     :fallback-url (quri:uri "https://startpage.com/")
     :base-search-url "https://startpage.com/do/search?query=~a"
     :completion-function (make-startpage-completion)
     :documentation "Startpage `nyxt:search-engine' which can configure the settings accessible from the search page.
In order to specify settings from Startpage's \"Settings\" page, set `settings-string' to the hexadecimal number situated
after \"prfe=\" in the URL displayed in the \"Save without cookie\" section.")
  (object "cat" ((:web "web")
                 (:images "pics")
                 (:videos "video")
                 (:news "news")))
  (language-ui "lui" ((:english "english")
                      (:dansk "dansk")
                      (:deutsch "deutsch")
                      (:espanol "espanol")
                      (:francais "francais")
                      (:nederlands "nederlands")
                      (:norsk "norsk")
                      (:polski "polski")
                      (:portugues "portugues")
                      (:svenska "svenska")))
  (language-results "language" ((:english "english")
                                (:afrikaans "afrikaans")
                                (:albanian "albanian")
                                (:amharic "amharic")
                                (:arabic "arabic")
                                (:azerbaijani "azerbaijani")
                                (:basque "basque")
                                (:belarusian "belarusian")
                                (:bengali "bengali")
                                (:bihari "bihari")
                                (:bosnian "bosnian")
                                (:bulgarian "bulgarian")
                                (:catalan "catalan")
                                (:croatian "croatian")
                                (:czech "czech")
                                (:dansk "dansk")
                                (:deutsch "deutsch")
                                (:english-uk "english_uk")
                                (:espanol "espanol")
                                (:esperanto "esperanto")
                                (:estonian "estonian")
                                (:fantizhengwen "fantizhengwen")
                                (:faroese "faroese")
                                (:francais "francais")
                                (:frisian "frisian")
                                (:gaelic "gaelic")
                                (:galician "galician")
                                (:georgian "georgian")
                                (:greek "greek")
                                (:gujarati "gujarati")
                                (:hangul "hangul")
                                (:hebrew "hebrew")
                                (:hindi "hindi")
                                (:hungarian "hungarian")
                                (:icelandic "icelandic")
                                (:indonesian "indonesian")
                                (:interlingua "interlingua")
                                (:irish "irish")
                                (:italiano "italiano")
                                (:javanese "javanese")
                                (:jiantizhongwen "jiantizhongwen")
                                (:kannada "kannada")
                                (:latin "latin")
                                (:latvian "latvian")
                                (:lithuanian "lithuanian")
                                (:macedonian "macedonian")
                                (:malay "malay")
                                (:malayalam "malayalam")
                                (:maltese "maltese")
                                (:marathi "marathi")
                                (:nederlands "nederlands")
                                (:nepali "nepali")
                                (:nihongo "nihongo")
                                (:norsk "norsk")
                                (:occitan "occitan")
                                (:persian "persian")
                                (:polski "polski")
                                (:portugues "portugues")
                                (:punjabi "punjabi")
                                (:romanian "romanian")
                                (:russian "russian")
                                (:serbian "serbian")
                                (:sinhalese "sinhalese")
                                (:slovak "slovak")
                                (:slovenian "slovenian")
                                (:sudanese "sudanese")
                                (:suomi "suomi")
                                (:svenska "svenska")
                                (:swahili "swahili")
                                (:tagalog "tagalog")
                                (:tamil "tamil")
                                (:telugu "telugu")
                                (:thai "thai")
                                (:tigrinya "tigrinya")
                                (:turkce "turkce")
                                (:ukrainian "ukrainian")
                                (:urdu "urdu")
                                (:uzbek "uzbek")
                                (:vietnamese "vietnamese")
                                (:welsh "welsh")
                                (:xhosa "xhosa")
                                (:zulu "zulu")))
  (family-filter "qadf" ((t "heavy")
                         (nil "none")))
  (web-date "with_date" ((:any "")
                         (:day "d")
                         (:week "w")
                         (:month "m")
                         (:year "y")))
  (web-region "qsr" ((:all "all")
                     (:australia "en_AU")
                     (:austria "de_AT")
                     (:belarus "ru_BY")
                     (:belgium-fr "fr_BE")
                     (:belgium-nl "nl_BE")
                     (:brazil "pt-BR_BR")
                     (:bulgaria "bg_BG")
                     (:canada "en_CA")
                     (:canada-fr "en_FR")
                     (:chile "es_CL")
                     (:china "zh-CN_CN")
                     (:denmark "da_DK")
                     (:egypt "ar_EG")
                     (:finland "fi_FI")
                     (:france "fr_FR")
                     (:germany "de_DE")
                     (:greece "el_GR")
                     (:honk-kong "zh-TW_HK")
                     (:india "hi_IN")
                     (:japan "ja_JP")
                     (:korean "ko_KR")
                     (:malaysia "ms_MY")
                     (:malaysia-en "en_MY")
                     (:netherlands "nl_NL")
                     (:norway "no_NO")
                     (:poland "pl_PL")
                     (:portugal "pt_PT")
                     (:romania "ro_RO")
                     (:russia "ru_RU")
                     (:south-africa "en_ZA")
                     (:spain "es_ES")
                     (:spain-ca "ca_ES")
                     (:sweden "sv_SE")
                     (:switzerland-de "de_CH")
                     (:switzerland-fr "fr_CH")
                     (:switzerland-it "it_CH")
                     (:taiwan "zh-TW_TW")
                     (:turkey "tr_TR")
                     (:united-kingdom "en-GB_GB")
                     (:united-states-en "en_US")
                     (:united-states-es "es_US")))
  (images-size "flimgsize" ((:any "")
                            (:large "isz:l")
                            (:medium "isz:m")
                            (:large "isz:l")
                            (:icon "isz:i")))
  (images-size-predefined "image-size-select" ((:any "")
                                               (:400x300 "isz:lt,islt:qsvgs")
                                               (:640x480 "isz:lt,islt:vga")
                                               (:800x600 "isz:lt,islt:svga")
                                               (:1024x768 "isz:lt,islt:xga")
                                               (:1600x1200 "isz:lt,islt:2mp") ; 2MP
                                               (:2272x1704 "isz:lt,islt:4mp") ; 4MP
                                               (:2816x2112 "isz:lt,islt:6mp") ; 6MP
                                               (:3264x2448 "isz:lt,islt:8mp") ; 8MP
                                               (:3648x2736 "isz:lt,islt:10mp") ; 10MP
                                               (:4096x3072 "isz:lt,islt:12mp") ; 12MP
                                               (:4480x3360 "isz:lt,islt:15mp") ; 15MP
                                               (:5120x3840 "isz:lt,islt:20mp") ; 20MP
                                               (:7216x5412 "isz:lt,islt:40mp") ; 40MP
                                               (:9600x7200 "isz:lt,islt:70mp"))) ; 70MP
  (images-size-exact-width "flimgexwidth" (:function #'startpage-image-size))
  (images-size-exact-height "flimgexheight" (:function #'startpage-image-size))
  (images-color "flimgcolor" ((:any "ic:")
                              (:color-only "ic:color")
                              (:black-white "ic:gray")
                              (:transparent "ic:trans")
                              (:red "ic:specific,isc:red")
                              (:orange "ic:specific,isc:orange")
                              (:yellow "ic:specific,isc:yellow")
                              (:green "ic:specific,isc:green")
                              (:teal "ic:specific,isc:teal")
                              (:blue "ic:specific,isc:blue")
                              (:purple "ic:specific,isc:purple")
                              (:pink "ic:specific,isc:pink")
                              (:gray "ic:specific,isc:gray")
                              (:black "ic:specific,isc:black")
                              (:brown "ic:specific,isc:brown")))
  (images-type "flimgtype" ((:any "")
                            (:jpg "jpg")
                            (:png "png")
                            (:gif "gif")))
  (videos-filter "sort_by" ((:relevant "")
                            (:popular "popular")
                            (:recent "recent")))
  (videos-length "with_duration" ((:any "")
                                  (:short "short")
                                  (:medium "medium")
                                  (:long "long")))
  (news-date "with_date" ((:any "")
                          (:day "d")
                          (:week "w")
                          (:month "m")))
  (settings-string "prfe" (:function #'startpage-settings-string)))

(define-search-engine github
    (:shortcut "github"
     :fallback-url (quri:uri "https://github.com/")
     :base-search-url "https://github.com/search?q=~a"
     :documentation "GitHub search engine.
Has no completion, as GitHub doesn't seem to have one.
Use advanced search with

(github :object :advanced)

All the fancy github search params will be there for you.")
  (object "type" ((:repositories "repositories")
                  (:code "code")
                  (:commits "commits")
                  (:issues "issues")
                  (:discussions "discussions")
                  (:packages "registrypackages")
                  (:marketplace "marketplace")
                  (:topics "topics")
                  (:wikis "wikis")
                  (:users "users")
                  (:advanced "advsearch")))
  (language "l" ((:default "")))
  (sort-by "s" ((:best-match "")
                (:stars "stars")
                (:forks "forks")
                (:recently-indexed "indexed")
                (:recently-commited "commiter-date")
                (:recently-authored "author-date")
                (:recently-joined "joined ")
                (:recently-created "created")
                (:recently-updated "updated")
                (:most-commented "comments")
                (:most-downloads "downloads")
                (:most-followers "followers")
                (:most-repositories "repositories")))
  (sort-order "o" ((:descending "desc")
                   (:ascending "asc")))
  ;; Issue-specific
  (state state ((:any "")
                (:open "open")
                (:closed "closed")))
  ;; Package-specific
  (package-type "package_type" ((:any "")
                                (:npm "npm")
                                (:container "container")
                                (:maven "maven")
                                (:nuget "nuget")
                                (:docker "docker")
                                (:rubygems "rubygems"))))

(define-search-engine arch
    (:shortcut "arch"
     :fallback-url (quri:uri "https://archlinux.org/packages/")
     :base-search-url "https://archlinux.org/packages/?q=~a"
     :documentation "Arch package search.")
  (arch "arch" ((:any "")
                (:x86-64 "x86_64")))
  (repository "repo" ((:all "")
                      (:community "Community")
                      (:community-testing "Community-Testing")
                      (:core "Core")
                      (:extra "Extra")))
  (maintainer "maintainer" ((:default "")))
  (flagged "flagged" ((:all "")
                      (t "Flagged")
                      (nil "Not Flagged"))))

(export-always 'make-arch-aur-completion)
(defun make-arch-aur-completion (&rest request-args)
    "Helper that generates AUR search completion functions.
REQUEST-ARGS is a list of args to pass to request function."
  (make-search-completion-function
   :base-url "https://aur.archlinux.org/rpc?type=suggest&arg=~a"
   :processing-function
   #'(lambda (results)
       (when results
         (json:decode-json-from-string results)))
   :request-args request-args))

(define-search-engine arch-aur
    (:shortcut "aur"
     :fallback-url (quri:uri "https://aur.archlinux.org/")
     :base-search-url "https://aur.archlinux.org/packages/?O=0&K=~a"
     :completion-function (make-arch-aur-completion)
     :documentation "Arch AUR package search.")
  (search-by "SeB" ((:name-and-description "nd")
                    (:name "n")
                    (:package-base "b")
                    (:exact-name "N")
                    (:exact-package-base "B")
                    (:keywords "k")
                    (:maintainer "m")
                    (:co-maintainer "c")
                    (:maintainer-and-co-maintainer "M")
                    (:submitter "s")))
  (sort-order "SO" ((:ascending "a")
                    (:descending "d")))
  (outdated "outdated" ((:default "")
                        (t "on")
                        (nil "off")))
  (sort-by "SB" ((:name "n")
                 (:votes "v")
                 (:popularity "p")
                 (:voted "w")
                 (:notify "o")
                 (:maintainer "m")
                 (:last-modified "l")))
  (per-page "PP" ((:default ""))))

(define-search-engine debian
    (:shortcut "debian"
     :fallback-url (quri:uri "https://www.debian.org/distrib/packages#search_packages")
     :base-search-url "https://packages.debian.org/search?keywords=~a"
     :documentation "Debian package search.")
  (search-on "searchon" ((:default "")
                         (:names "names")
                         (:all "all")
                         (:source-names "sourcenames")))
  (suite "suite" ((:default "")
                  (:all "all")
                  (:experimental "experimental")
                  (:unstable "unstable")
                  (:testing "testing")
                  (:stable "stable")
                  (:oldstable "oldstable")))
  (section "section" ((:default "")
                      (:all "all")
                      (:main "main")
                      (:contrib "contrib")
                      (:non-free "non")))
  (exact "exact" ((nil "")
                  (t "1"))))

(export-always 'make-pkgs-completion)
(defun make-pkgs-completion (&rest request-args)
    "Helper that generates pkgs.org search completion functions.
REQUEST-ARGS is a list of args to pass to request function."
  (make-search-completion-function
   :base-url "https://api.pkgs.org/autocomplete/~a"
   :processing-function
   #'(lambda (results)
       (when results
         (json:decode-json-from-string results)))
   :request-args request-args))

(define-search-engine pkgs
    (:shortcut "pkgs"
     :fallback-url (quri:uri "https://pkgs.org/")
     :base-search-url "https://pkgs.org/search/?q=~a"
     :completion-function (make-pkgs-completion)
     :documentation "UNIX/GNU/Linux package search."))

(define-search-engine peertube
    (:shortcut "peertube"
     :fallback-url (quri:uri "search.joinpeertube.org")
     :base-search-url "https://search.joinpeertube.org/search?search=~a"
     :documentation "PeerTube `nyxt:search-engine' via its Sepia Search global search index.")
  (sort-by "sort" ((:best "-match")
                   (:newest "-publishedAt")
                   (:oldest "publishedAt")))
  (published-date "publishedDateRange" ((:any "any_published_date")
                                        (:today "today")
                                        (:month "last_month")
                                        (:week "last_7days")
                                        (:year "last_365days")))
  (is-live "isLive" ((nil "false")
                     (t "both")))
  (nsfw "nsfw" ((nil "false")
                (t "both")))
  (duration "durationRange" ((:any "any_duration")
                             (:short "short")
                             (:medium "medium")
                             (:long "long")))
  (category "categoryOneOf" ((:all "")
                             (:music "1")
                             (:films "2")
                             (:vehicles "3")
                             (:art "4")
                             (:sports "5")
                             (:travels "6")
                             (:gaming "7")
                             (:people "8")
                             (:comedy "9")
                             (:entertainment "10")
                             (:news-and-politics "11")
                             (:how-to "12")
                             (:education "13")
                             (:activism "14")
                             (:science-and-technology "15")
                             (:animals "16")
                             (:kids "17")
                             (:food "18")))
  (licence "licenceOneOf" ((:all "")
                           (:by "1")
                           (:by-sa "2")
                           (:by-nd "3")
                           (:by-nc "4")
                           (:by-nc-sa "5")
                           (:by-nc-nd "6")
                           (:cc0 "7")))
  (language "languageOneOf" ((:all "")
                             (:english "en")
                             (:francais "fr")
                             (:japanese "ja")
                             (:euskara "eu")
                             (:catala "ca")
                             (:czech "cs")
                             (:esperanto "eo")
                             (:bulgarian "el")
                             (:deutsch "de")
                             (:italiano "it")
                             (:nederlands "nl")
                             (:espanol "es")
                             (:occitan "oc")
                             (:gaelic "gd")
                             (:chinese "zh")
                             (:portugues "pt")
                             (:svenska "sv")
                             (:polski "pl")
                             (:suomi "fi")
                             (:russian "ru")))
  (host "host" ((:default "")))
  (tags "tagsAllOf" ((:default ""))))

(define-search-engine sourcehut
    (:shortcut "sourcehut"
     :fallback-url (quri:uri "https://sr.ht")
     :base-search-url "https://sr.ht/projects?search=~a"
     :documentation "Sourcehut project search `nyxt:search-engine'.")
  (sort-by "sort" ((:recent "recently-updated")
                   (:active "longest-active"))))

(define-search-engine libgen
    (:shortcut "libgen"
     :fallback-url (quri:uri "https://libgen.li")
     :base-search-url "https://libgen.li/index.php?req=~a"
     :documentation "Library Genesis `nyxt:search-engine'. Due to how this engine supplies
attribute filtering for fields, objects, or topics, it's not possible to filter per more than
one of these at once unlike on the web interface.")
  (results "res" ((25 "25")
                  (50 "50")
                  (100 "100")))
  (covers "covers" ((nil "")
                    (t "on")))
  (chapters "showch" ((nil "")
                      (t "on")))
  (google-mode "gmode" ((nil "")
                        (t "on")))
  (file-search "filesuns" ((:all "all")
                           (:sorted "sort")
                           (:unsorted "unsort")))
  (field "columns%5B%5D" ((:all "")
                          (:title "t")
                          (:author "a")
                          (:series "s")
                          (:year "y")
                          (:publisher "p")
                          (:isbn "i")))
  (object "objects%5B%5D" ((:all "")
                           (:files "f")
                           (:editions "e")
                           (:series "s")
                           (:authors "a")
                           (:publishers "p")
                           (:works "w")))
  (topic "topics%5B%5D" ((:all "")
                         (:libgen "l")
                         (:comics "c")
                         (:fiction "f")
                         (:articles "a")
                         (:magazines "m")
                         (:fiction-rus "r")
                         (:standards "s")))
  (tab "curtab" ((:files "f")
                 (:editions "e")
                 (:series "s")
                 (:authors "a")
                 (:publishers "p")
                 (:works "w"))))

(define-search-engine reddit
    (:shortcut "reddit"
     :fallback-url (quri:uri "https://reddit.com")
     :base-search-url "https://reddit.com/search/?q=~a"
     :documentation "Reddit `nyxt:search-engine'. Its `:nsfw' filter is only available when logged in.")
  (sort-by "sort" ((:relevance "relevance")
                   (:hot "hot")
                   (:top "top")
                   (:new "new")
                   (:comments "comments")))
  (only-from-subreddit "restrict_sr" ((nil "")
                                      (t "on")))
  (nsfw "nsfw" ((nil "")
                (t "on")))
  (date "t" ((:all "all")
             (:year "year")
             (:month "month")
             (:week "week")
             (:day "day")
             (:hour "hour")))
  (type-from "type" ((:link "link")
                     (:comments "comment")
                     (:communities "sr")
                     (:user "user"))))

(define-derived-search-engine teddit
    (reddit :shortcut "teddit"
            :fallback-url (quri:uri "https://teddit.net")
            :base-search-url "https://teddit.net/search/?q=~a"
            :nsfw t)
    "Teddit `nyxt:search-engine', a privacy-friendly Reddit front-end.")

(define-search-engine lemmy
    (:shortcut "lemmy"
     :fallback-url (quri:uri "https://lemmy.ml")
     :base-search-url "https://lemmy.ml/search/q/~a"
     :documentation "Lemmy `nyxt:search-engine'. Lemmy is a decentralized link-aggregator
for the Fediverse."
     :manual-delims-p t
     :force-supply-p t)
  (type-from "/type/" ((:all "All")
                       (:comments "Comments")
                       (:posts "Posts")
                       (:communities "Communities")
                       (:users "Users")
                       (:url "Url")))
  (sort-by "/sort/" ((:top-all "TopAll")
                     (:top-year "TopYear")
                     (:top-month "TopMonth")
                     (:top-week "TopWeek")
                     (:top-day "TopDay")
                     (:new "New")))
  (listing-type "/listing_type/" ((:all "All")
                                  (:local "Local")
                                  (:subscribed "Subscribed")))
  (community-id "/community_id/" ((:default 0)))
  (creator-id "/creator_id/" ((:default 0)))
  (page "/page/" ((:default 1))))

(export-always 'make-invidious-completion)
(defun make-invidious-completion (&key (instance "invidious.namazso.eu") request-args)
  "Helper that generates Invidious search completion suggestions with INSTANCE."
  (make-search-completion-function
   :base-url (format nil "https://~a/api/v1/search/suggestions?q=~~a" instance)
   :processing-function
   #'(lambda (result)
       (when result
         (alexandria:assoc-value (json:decode-json-from-string result) :suggestions)))
   :request-args request-args))

(define-search-engine invidious
    (:shortcut "invidious"
     :fallback-url (quri:uri "https://invidious.snopyta.org")
     :base-search-url "https://invidious.snopyta.org/search?q=~a"
     :completion-function (make-invidious-completion)
     :documentation "`nyxt:search-engine' for Invidious, an alternative YouTube front-end.")
  (upload-date "date" ((:none "none")
                       (:hour "hour")
                       (:today "today")
                       (:week "week")
                       (:month "month")
                       (:year "year")))
  (result-type "type" ((:all "all")
                       (:video "video")
                       (:channel "channel")
                       (:playlist "playlist")
                       (:movie "movie")
                       (:show "show")))
  (duration "duration" ((:none "none")
                        (:short "short")
                        (:long "long")
                        (:medium "medium")))
  (features "features" ((:none "none")
                        (:live "live")
                        (:4k "four_k")
                        (:hd "hd")
                        (:subtitles "subtitles")
                        (:cc "c_commons")
                        (:360-deg "three_sixty")
                        (:vr-180 "vr180")
                        (:3d "three_d")
                        (:hdr "hdr")
                        (:location "location")
                        (:purchased "purchased")))
  (sort-by "sort" ((:relevance "relevance")
                   (:rating "rating")
                   (:date "date")
                   (:views "views"))))

(define-search-engine discourse
    (:shortcut "discourse"
     :fallback-url (quri:uri "https://discourse.atlas.engineer")
     :base-search-url "https://discourse.atlas.engineer/search?q=~a"
     :documentation "`nyxt:search-engine' for Discourse-based instances. You can leverage this engine's
advanced search filters for more precise searches.")
  (search-type "search_type" ((:default "topics/posts")
                              (:categories "categories_tags")
                              (:users "users"))))

(define-search-engine meetup
    (:shortcut "meetup"
     :fallback-url (quri:uri "https://www.meetup.com")
     :base-search-url "https://www.meetup.com/find/?op=search&keywords=~a"
     :documentation "Meetup `nyxt:search-engine', a service for online communities. Its `:suggested'
search filter is only available when you're logged in and the `:location' filter takes the form
of `<country_code>--<city_name>', such as `gb--London'.")
  (distance "distance" ((:any "")
                        (2 "twoMiles")
                        (5 "fiveMiles")
                        (10 "tenMiles")
                        (25 "twentyFiveMiles")
                        (50 "fiftyMiles")
                        (100 "hundredMiles")))
  (sort-by "sortField" ((:default "default")
                        (:relevance "RELEVANCE")
                        (:date "DATETIME")))
  (date "dateRange" ((:any "")
                     (:soon "startingSoon")
                     (:today "today")
                     (:tomorrow "tomorrow")
                     (:this-week "this-week")
                     (:this-weekend "this-weekend")
                     (:next-week "next-week")))
  (start-date "customStartDate" ((:default "")))
  (end-date "customEndDate" ((:default "")))
  (event-type "eventType" ((:default "")
                           (:in-person "inPerson")
                           (:online "online")
                           (:indoor "indoor")
                           (:outdoor "outdoor")))
  (category "categoryId" ((:default "")
                          (:new-groups "-999")
                          (:art-culture "521")
                          (:career-business "405")
                          (:community-environment "604")
                          (:dancing "612")
                          (:games "535")
                          (:health-wellbeing "511")
                          (:hobbies-passions "571")
                          (:identity-language "622")
                          (:movement-politics "642")
                          (:music "395")
                          (:parents-family "673")
                          (:pets-animals "701")
                          (:religion-spirituality "593")
                          (:science-education "436")
                          (:social-activities "652")
                          (:sports-fitness "482")
                          (:support-coaching "449")
                          (:technology "546")
                          (:travel-outdoor "684")
                          (:writing "467")))
  (source "source" ((:events "EVENTS")
                    (:groups "GROUPS")))
  (suggested "suggested" ((nil "")
                          (t "TRUE")))
  (location "location" ((:default ""))))

(define-search-engine gitea
    (:shortcut "gitea"
     :fallback-url (quri:uri "https://codeberg.org/explore/repos")
     :base-search-url "https://codeberg.org/explore/repos?q=~a"
     :documentation "`nyxt:search-engine' for Gitea, a self-hosted Git forge.")
  (unfiltered "no_filter" ((t "1")
                           (nil "")))
  (sort-by "sort" ((:recently-update "recentupdate")
                   (:newest "newest")
                   (:oldest "oldest")
                   (:ascending "alphabetically")
                   (:descending "reversealphabetically")
                   (:least-updated "leastupdate")
                   (:most-stars "moststars")
                   (:fewest-stars "feweststars")
                   (:most-forks "mostforks")
                   (:fewest-forks "fewest forks"))))

(define-derived-search-engine gitea-users
    (gitea :shortcut "gitea-users"
           :base-search-url "https://codeberg.org/explore/users?q=~a"
           :fallback-url (quri:uri "https://codeberg.org/explore/users"))
    "`gitea' derived search engine for a Gitea's instance users.")

(define-derived-search-engine gitea-organizations
    (gitea :shortcut "gitea-organizations"
           :base-search-url "https://codeberg.org/explore/organizations?q=~a"
           :fallback-url (quri:uri "https://codeberg.org/explore/organizations"))
    "`gitea' derived search engine for a Gitea's instance organizations.")

(define-search-engine hacker-news
    (:shortcut "hacker-news"
     :fallback-url (quri:uri "https://hn.algolia.com")
     :base-search-url "https://hn.algolia.com/?q=~a"
     :documentation "`nyxt:search-engine' for Hacker News via Algolia Search.")
  (date-range "dateRange" ((:all "all")
                           (:past-day "last24h")
                           (:past-week "pastWeek")
                           (:past-month "pastMonth")
                           (:past-year "pastYear")
                           (:custom "custom")))
  (date-start "dateStart" ((:default "")))
  (date-end "dateEnd" ((:default "")))
  (sort-by "sort" ((:popularity "byPopularity")
                   (:date "byDate")))
  (search-type "type" ((:story "story")
                       (:all "all")
                       (:comment "comment"))))

(define-search-engine lobsters
    (:shortcut "lobsters"
     :fallback-url (quri:uri "https://lobste.rs")
     :base-search-url "https://lobste.rs/search?q=~a"
     :documentation "`nyxt:search-engine' for the computing-focused link-aggregator Lobsters.")
  (search-type "what" ((:default "stories")
                       (:comments "comments")))
  (order-by "order" ((:default "newest")
                     (:relevance "relevance")
                     (:points "points"))))

;; TODO:
;; - YouTube
;; - Amazon
;; - Facebook
;; - Gmaps
;; - Twitter
;; - Pinterest
;; - Ask
;; - Baidu
;; - WolframAlpha
;; - Boardreader
;; - Ecosia
;; - Qwant
;; - Search Encrypt
;; - Yandex
;; - Yandex.Images
;; - Gibiru
;; - Disconnect
;; - Yippy
;; - Swisscows
;; - Lukol
;; - Metager
;; - Gigablast
;; - Oskobo
;; - Infinity Search
;; - Mail.ru
;; - Rambler.ru
