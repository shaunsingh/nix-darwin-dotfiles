(in-package #:nyxt-user)

;;; STARTPAGE

(defvar *logo-svg* 
  (alexandria:read-file-into-string 
   (merge-pathnames #p".config/nyxt/nyoom-engineering.svg" 
                    (user-homedir-pathname))))

(defun sort-by-time (sequence &key (key #'last-access))
  "Return a timely ordered SEQUENCE by KEY.  More recent elements come first."
  (sort sequence #'local-time:timestamp> :key key))

;; largely taken from fruit-of-the-day package, credit
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

(define-internal-page-command-global startpage ()
    (buffer "*startpage*")
  "my custom startpage"
  (flet ((list-bookmarks (&key (limit 6) (separator " → "))
           (spinneret:with-html-string
             (let ((mode (make-instance 'nyxt/mode/bookmark:bookmark-mode)))
               (alexandria:if-let ((bookmark-content (ignore-errors (files:content (nyxt/mode/bookmark:bookmarks-file mode)))))
                 (dolist (bookmark (serapeum:take limit (the list (sort-by-time bookmark-content :key #'nyxt/mode/bookmark:date))))
                   (:li (title bookmark) separator
                        (:a :href (render-url (url bookmark))
                            (render-url (url bookmark)))))
                 (:p (format nil "No bookmarks in ~s." (files:expand (nyxt/mode/bookmark:bookmarks-file mode)))))))))
    (let ((current-year (local-time:timestamp-year (local-time:now)))
          (dashboard-style (theme:themed-css (theme *browser*)
                              `("#motto"
                                :font-size "27px"
                                :margin "18px"
                                :color ,*base08-*)
                              `("#buttons"
                                :margin-top "18px"
                                :font-size "18px")
                              `("#logo-container"
                                :margin-top "18px"
                                :margin-left "9px")
                              `("#copyright"
                                :font-family ,*mono*
                                :position "absolute"
                                :text-align "right"
                                :bottom "1.5em"
                                :right "1.5em"))))
     (spinneret:with-html-string
       (:nstyle dashboard-style)
       (:div :id "container"
         (:h1 "Welcome to " (:span :id "subtitle" "NYXT"))
         (:div :id "buttons"
          (:nbutton :text "Repl"
           '(nyxt/mode/repl:repl))
          (:nbutton :text "Manual"
           '(make-buffer-focus :url (nyxt-url 'manual)))
          (:nbutton :text "Changelog"
           '(make-buffer-focus :url (nyxt-url 'changelog)))
          (:nbutton :text "Bookmarks"
           '(nyxt/mode/bookmark:list-bookmarks))
          (:nbutton :text "Annotations"
           '(nyxt/mode/annotate:show-annotations)))
        (:div :id "logo-container"
          (:raw *logo-svg*))
        (:div :id "motto"
         "私たちのミッションは"
         (:br)
         "先端工学を用いて上質で"
         (:br)
         "機能的なデザインの"
         (:br)
         "製品を作り出すことです。")
        (:h2 "Bookmarks")
        (:ul (:raw (list-bookmarks :limit 9)))
        (:h3 (fruit-of-the-day-message))
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

(define-configuration browser
  ((default-new-buffer-url (quri:uri "nyxt:nyxt-user:startpage"))))
