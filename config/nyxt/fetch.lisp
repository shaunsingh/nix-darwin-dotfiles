(in-package #:nyxt-user)

;;; FETCH

(defun mem-total ()
    (float 
       (/ (parse-integer (string-trim "MemTotal:       kB " (uiop:read-file-line "/proc/meminfo" :at 0))) (* 1024 1024))))
(defun mem-free ()
  (float
    (/ (parse-integer (string-trim "MemFree:         kB" (uiop:read-file-line "/proc/meminfo" :at 1))) (* 1024 1024))))
(defun mem-cached ()
  (float
    (/ (parse-integer (string-trim "Cached:         kB" (uiop:read-file-line "/proc/meminfo" :at 4))) (* 1024 1024))))
(defun mem-used ()
   (- (mem-total) (+ (mem-cached) (mem-free))))

#+linux
(define-internal-page-command-global fetch ()
    (buffer "*fetch*")
  "my custom fetch"
  (let ((dashboard-style (theme:themed-css (theme *browser*)
                            `("#fetch"
                              :font-family ,*mono*
                              :font-size "18px"
                              :margin "18px"
                              :color ,*base05-*
                              :background-color ,*base01-*))))
   (spinneret:with-html-string
     (:nstyle dashboard-style)
     (:div :id "container"
      (:h1 "System " (:span :id "subtitle" "FETCH"))
      (:pre :id "fetch"
        (format nil "NYXT ~a ~a" (name nyxt::*renderer*) nyxt::+version+)
        (:br)
        (format nil "~a ~a" (lisp-implementation-type) (lisp-implementation-version))
        (:br)
        (format nil "HOST: ~a@~a" (machine-instance) (software-version))
        (:br)
        (format nil "WM: ~a" (uiop:getenv "XDG_CURRENT_DESKTOP"))
        (:br)
        (format nil "THEME: ~a (~a)" (uiop:getenv "GTK_THEME") "oxocarbon")
        (:br)
        (format nil "SHELL: ~a" (uiop:getenv "SHELL"))
        (:br)
        (format nil "RAM: ~,2f/~f GB" (mem-used) (fceiling (mem-total)))
        (:br)
        ;; doesn't work on m1
        ;; (format nil "CPU: ~a" (machine-version))
        "CPU: (8) @ 2.064GHz"
        (:br)
        (local-time:format-timestring nil (local-time:now) :format local-time:+rfc-1123-format+))))))
