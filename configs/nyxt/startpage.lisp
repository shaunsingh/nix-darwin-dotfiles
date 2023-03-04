(in-package #:nyxt-user)

;; stolen from time.lisp
(defun sort-by-time (sequence &key (key #'last-access))
  "Return a timely ordered SEQUENCE by KEY.  More recent elements come first."
  (sort sequence #'local-time:timestamp> :key key))

;; stolen from nx-fruit
(defparameter list-of-fruits
  (list "abiu"
        "açaí"
        "acerola"
        "ackee"
        "african cucumber"
        "apple"
        "apricot"
        "avocado"
        "banana"
        "bilberry"
        "blackberry"
        "blackcurrant"
        "black sapote"
        "blueberry"
        "boysenberry"
        "breadfruit"
        "buddha's hand (fingered citron)"
        "cactus pear"
        "canistel"
        "cempedak"
        "cherimoya (Custard Apple)"
        "cherry"
        "chico fruit"
        "cloudberry"
        "coco De Mer"
        "coconut"
        "crab apple"
        "cranberry"
        "currant"
        "damson"
        "date"
        "dragonfruit (or Pitaya)"
        "durian"
        "egg Fruit"
        "elderberry"
        "feijoa"
        "fig"
        "finger Lime (or Caviar Lime)"
        "goji berry"
        "gooseberry"
        "grape"
        "raisin"
        "grapefruit"
        "grewia asiatica (phalsa or falsa)"
        "guava"
        "hala Fruit"
        "honeyberry"
        "huckleberry"
        "jabuticaba"
        "jackfruit"
        "jambul"
        "japanese plum"
        "jostaberry"
        "jujube"
        "juniper berry"
        "kaffir Lime"
        "kiwano (horned melon)"
        "kiwifruit"
        "kumquat"
        "lemon"
        "lime"
        "loganberry"
        "longan"
        "loquat"
        "lulo"
        "lychee"
        "magellan Barberry"
        "mamey Apple"
        "mamey Sapote"
        "mango"
        "mangosteen"
        "marionberry"
        "melon"
        "cantaloupe"
        "galia melon"
        "honeydew"
        "mouse melon"
        "musk melon"
        "watermelon"
        "miracle fruit"
        "monstera deliciosa"
        "mulberry"
        "nance"
        "nectarine"
        "orange"
        "blood orange"
        "clementine"
        "mandarine"
        "tangerine"
        "papaya"
        "passionfruit"
        "pawpaw"
        "peach"
        "pear"
        "persimmon"
        "plantain"
        "plum"
        "prune (dried plum)"
        "pineapple"
        "pineberry"
        "plumcot (or Pluot)"
        "pomegranate"
        "pomelo"
        "purple mangosteen"
        "quince"
        "raspberry"
        "salmonberry"
        "rambutan (or Mamin Chino)"
        "redcurrant"
        "rose apple"
        "salal berry"
        "salak"
        "satsuma"
        "shine Muscat or Vitis Vinifera"
        "sloe or Hawthorn Berry"
        "soursop"
        "star apple"
        "star fruit"
        "strawberry"
        "surinam cherry"
        "tamarillo"
        "tamarind"
        "tangelo"
        "tayberry"
        "ugli fruit"
        "white currant"
        "white sapote"
        "yuzu"
        "bell pepper"
        "chile pepper"
        "corn kernel"
        "cucumber"
        "eggplant"
        "jalapeño"
        "olive"
        "pea"
        "pumpkin"
        "squash"
        "tomato"
        "zucchini"))

;; nice words
(defparameter list-of-pretty-words
  (list "lovely"
        "wonderful"
        "delightful"
        "beautiful"
        "pleasant"
        "adorable"
        "sweet"
        "delicious"
        "charming"
        "fantastic"
        "gorgeous"
        "heavenly"
        "magnificent"
        "radiant"
        "splendid"
        "exquisite"
        "enchanting"
        "serene"
        "blissful"
        "harmonious"
        "majestic"
        "tranquil"
        "whimsical"
        "ethereal"
        "celestial"
        "idyllic"
        "mesmerizing"
        "spellbinding"
        "captivating"
        "fascinating"
        "riveting"
        "enthralling"
        "mesmerizing"
        "inspiring"))

;; have some alliteration word fun
(defun fruit-of-the-day-message ()
  (flet ((capitalize-word (word)
           (concatenate 'string (string-upcase (subseq word 0 1))
                              (subseq word 1))))
    (let* ((current-time (local-time:now))
           (current-day (aref local-time:+day-names+
                              (local-time:timestamp-day-of-week current-time)))
           (current-fruit (nth (mod (local-time:day-of current-time)
                                    (length list-of-fruits))
                               list-of-fruits))
           (matching-words (remove-if-not (lambda (word)
                                            (char= (char word 0)
                                                   (char current-fruit 0)))
                                          list-of-pretty-words))
           (word (if matching-words
                     (nth (random (length matching-words)) matching-words)
                     (nth (random (length list-of-pretty-words))
                          list-of-pretty-words))))
      (format nil "Have ~A ~A ~A ~A!"
              (if (member (char (string word) 0) '(#\a #\e #\i #\o #\u))
                  "an" "a")
              (capitalize-word word)
              (capitalize-word current-fruit)
              current-day))))

;; now to bring it all together
(define-internal-page-command-global startpage ()
    (buffer "*startpage*")
  "my custom startpage"
  (flet ((list-bookmarks (&key (limit 6) (separator " → "))
           (spinneret:with-html-string
             (let ((mode (make-instance 'nyxt/bookmark-mode:bookmark-mode)))
               (alexandria:if-let ((bookmarks (files:content (nyxt/bookmark-mode:bookmarks-file mode))))
                 (dolist (bookmark (serapeum:take limit (the list (sort-by-time bookmarks :key #'nyxt/bookmark-mode:date))))
                   (:li (title bookmark) separator
                        (:a :href (render-url (url bookmark))
                            (render-url (url bookmark)))))
                 (:p (format nil "No bookmarks in ~s." (files:expand (nyxt/bookmark-mode:bookmarks-file mode)))))))))
    (let ((current-year (local-time:timestamp-year (local-time:now)))
          (dashboard-style (theme:themed-css (theme *browser*)
                              ;; monospacing
                              `(*
                                :font-family ,*font*
                                :font-size "11px")
                              ;; general settings
                              `(body
                                :background-color ,*base00*
                                :color ,*base04*
                                :margin "4% 6%")
                              ;; enable overflow
                              `("#container"
                                :white-space "nowrap"
                                :overflow "hidden")
                              ;; remove margins
                              `("h1, h2, h3"
                                :margin "0px")
                              ;; colors
                              `("h2, h3"
                                :color ,*base05*)
                              ;; fonts
                              `("h1, #subtitle"
                                :color ,*base06*
                                :font-family "SF Pro Display"
                                :font-size "72px"
                                :margin-bottom "-9px") 
                              `("h2, #motto"
                                :font-family "SF Pro Text"
                                :font-size "27px"
                                :margin-left "3px")
                              `("h3"
                                :font-family "SF Pro Text"
                                :font-size "18px"
                                :margin-bottom "2px")
                              `("p, li, ul, a"
                                :font-size "13px")
                              ;; margins
                              `("#buttons"
                                :margin-left "9px") 
                              `("h3, ul"
                                :margin-left "27px")
                              `("ul"
                                :margin-top "0px"
                                :list-style-type "katakana")
                              `("a"
                                :white-space "pre-wrap")
                              `("#motto"
                                :margin"18px"
                                :color ,*base08*)
                              ;; system info render
                              `("#copyright"
                                :position "absolute"
                                :text-align "right"
                                :bottom "1.5em"
                                :right "1.5em"))))
     (spinneret:with-html-string
       (:nstyle dashboard-style)
       (:div :id "container"
        (:h1 "Welcome to " (:span :id "subtitle" "NYXT ☺"))
        (:div :id "buttons"
         (:nbutton :text "Restore Session"
           (nyxt::restore-history-by-name))
         (:nbutton :text "Open Repl"
           (nyxt/repl-mode:repl))
         (:nbutton :text "View Changelog"
           (nyxt:changelog))
         (:nbutton :text "View Bookmarks"
           (nyxt/bookmark-mode:list-bookmarks))
         (:nbutton :text "View Annotations"
           (nyxt/annotate-mode:show-annotations)))
        (:div :id "motto"
         "私たちのミッションは"
         (:br)
         "先端工学を用いて上質で"
         (:br)
         "機能的なデザインの"
         (:br)
         "製品を作り出すことです。")
        (:h2 "Recents")
        (:h3 "Bookmarks")
        (:ul (:raw (list-bookmarks :limit 9)))
        (:h3 "History")
        (:ul (:raw (nyxt::history-html-list :limit 9)))
        (:h2 (fruit-of-the-day-message))
        (:div :id "copyright"
          (format nil "version ~a ~a" (name nyxt::*renderer*) nyxt::+version+)
          (:br)
          (format nil "lisp ~a ~a" (lisp-implementation-type) (lisp-implementation-version))
          (:br)
          (format nil "host ~a@~a" (software-type) (software-version))
          (:br)
          (format nil "Atlas Engineer LLC, 2018-~a" current-year)
          (:br)
          (local-time:format-timestring nil (local-time:now) :format local-time:+rfc-1123-format+)))))))

;; set default url to startpage
(define-configuration browser
  ((default-new-buffer-url (quri:uri "nyxt:nyxt-user:startpage"))))
