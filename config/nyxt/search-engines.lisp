(in-package #:nyxt-user)

;;; SEARCH

;; all heavily inspired by nx-search-engine, credit

(defmacro define-search-engine (name (&key shortcut fallback-url base-search-url
                                        force-supply-p manual-delims-p completion-function
                                        documentation)
                                &body keywords)

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

(defun make-google-completion (&key request-args)
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

(define-search-engine google-1998
    (:shortcut "g98"
     :base-search-url "https://oldgoogle.neocities.org/search-1998.html?q=hi&num=10#gsc.tab=0&gsc.q=~a"
     :fallback-url (quri:uri "https://oldgoogle.neocities.org/1998/")
     :documentation "Google-1998 `nyxt:search-engine' for the 1998 version of Google"))
(define-search-engine google-2009
    (:shortcut "g09"
     :base-search-url "https://oldgoogle.neocities.org/2009/search/?hl=en&source=hp&q=~a"
     :fallback-url (quri:uri "https://oldgoogle.neocities.org/2009/")
     :documentation "Google-2009 `nyxt:search-engine' for the 2009 version of Google"))
(define-search-engine google-2010
    (:shortcut "g10"
     :base-search-url "https://oldgoogle.neocities.org/2010/search/?sclient=psy&hl=en&site=webhp&source=hp&q=~a"
     :fallback-url (quri:uri "https://oldgoogle.neocities.org/2010/")
     :documentation "Google-2010 `nyxt:search-engine' for the 2010 version of Google"))
(define-search-engine google-2011
    (:shortcut "g11"
     :base-search-url "https://oldgoogle.neocities.org/2012-search?sclient=psy&hl=en&site=&source=hp&q=a"
     :fallback-url (quri:uri "https://oldgoogle.neocities.org/2011/")
     :documentation "Google-2011 `nyxt:search-engine' for the 2011 version of Google"))
(define-search-engine google-2013
    (:shortcut "g13"
     :base-search-url "https://oldgoogle.neocities.org/2013-search?sclient=psy-ab&site=&source=hp&q=a"
     :fallback-url (quri:uri "https://oldgoogle.neocities.org/2013/")
     :documentation "Google-2013 `nyxt:search-engine' for the 2013 version of Google"))

(defun make-google-scholar-completion (&key request-args)
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

(defun make-wikipedia-completion (&key (suggestion-limit 10) (namespace :general) request-args)
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

(defun make-yahoo-completion (&key request-args (suggestion-limit 10))
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

(define-search-engine sourcehut
    (:shortcut "sourcehut"
     :fallback-url (quri:uri "https://sr.ht")
     :base-search-url "https://sr.ht/projects?search=~a"
     :documentation "Sourcehut project search `nyxt:search-engine'.")
  (sort-by "sort" ((:recent "recently-updated")
                   (:active "longest-active"))))

(define-search-engine discourse
    (:shortcut "discourse"
     :fallback-url (quri:uri "https://discourse.atlas.engineer")
     :base-search-url "https://discourse.atlas.engineer/search?q=~a"
     :documentation "`nyxt:search-engine' for Discourse-based instances. You can leverage this engine's
advanced search filters for more precise searches.")
  (search-type "search_type" ((:default "topics/posts")
                              (:categories "categories_tags")
                              (:users "users"))))

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

(define-search-engine nixpkgs
    (:shortcut "nix"
     :base-search-url "https://search.nixos.org/packages?channel=unstable&from=0&size=50&sort=relevance&type=packages&query=~a"
     :fallback-url (quri:uri "https://search.nixos.org/")
     :documentation "`nyxt:search-engine' for the package manager nix"))

(define-search-engine openstreetmaps 
    (:shortcut "osm"
     :base-search-url "https://www.openstreetmap.org/search?query=~a"
     :fallback-url (quri:uri "https://www.openstreetmap.org/")
     :documentation "`nyxt:search-engine' for openstreetmaps"))

(define-search-engine openai-chatgpt
    (:shortcut "gpt"
     :base-search-url "https://chat.openai.com/?q=~a"
     :fallback-url (quri:uri "https://chat.openai.com/")
     :documentation "`nyxt:search-engine' for openai's chatgpt"))

(define-search-engine xai-grok
    (:shortcut "grok"
     :base-search-url "https://grok.com/?q=~a"
     :fallback-url (quri:uri "https://x.ai/")
     :documentation "`nyxt:search-engine' for xai's grok"))

(define-configuration context-buffer
  ((search-engines (list ;; derived
		         (google :shortcut "gmaps"
                                 :object :maps)
		         (google :shortcut "image"
                                 :object :image)
		         (google :shortcut "yt"
                                 :object :video)
		         (google :shortcut "news"
                                 :object :news)
		         (google :shortcut "shopping"
                                 :object :shopping)
		         (google :shortcut "books"
                                 :object :books)
		         (google :shortcut "finance"
                                 :object :finance)
                         ;; old
                         (google-1998)
                         (google-2009)
                         (google-2010)
                         (google-2011)
                         (google-2013)
			 ;; ai
                         (openai-chatgpt)
                         (xai-grok)
                         ;; other
                         (wikipedia)
                         (yahoo)
                         (scihub)
                         (github)
                         (sourcehut)
                         (discourse)
                         (hacker-news)
                         (lobsters)
                         (nixpkgs)
                         (openstreetmaps)
                         ;; default
                         (google :shortcut "g"
                                 :safe-search nil)))))


