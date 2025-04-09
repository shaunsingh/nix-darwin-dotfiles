(in-package #:nyxt-user)

;;; STATUS

;; ;; quick parenscript to grab out position in the buffer
;; (define-parenscript %percentage ()
;;   (defun percentage ()
;;     (let* ((height-of-window (ps:@ window inner-height))
;;            (content-scrolled (ps:@ window page-y-offset))
;;            (body-height (if (not (or (eql window undefined)
;;                                      (eql (ps:@ window document) undefined)
;;                                      (eql (ps:chain window
;;                                                     document
;;                                                     (get-elements-by-tag-name "body"))
;;                                           undefined)
;;                                      (eql (ps:chain window
;;                                                     document
;;                                                     (get-elements-by-tag-name "body")
;;                                                     0)
;;                                           undefined)
;;                                      (eql (ps:chain window
;;                                                     document
;;                                                     (get-elements-by-tag-name "body")
;;                                                     0
;;                                                     offset-height)
;;                                           undefined)))
;;                           (ps:chain window
;;                                     document
;;                                     (get-elements-by-tag-name "body")
;;                                     0
;;                                     offset-height)
;;                           0))
;;            (total (- body-height height-of-window))
;;            (prc (* (/ content-scrolled total) 100)))
;;       (if (> prc 100)
;;           100
;;           (round prc))))
;;   (percentage))
 
(defmethod my-format-status-load-status ((status status-buffer))
  (spinneret:with-html-string
   (:span (if (and (current-buffer)
                   (web-buffer-p (current-buffer)))
              (case (slot-value (current-buffer) 'nyxt::status)
                    (:unloaded "∅ ")
                    (:loading "∞ ")
                    (:finished ""))
            ""))))

(defmethod my-format-status-url ((status status-buffer))
  (let* ((buffer (current-buffer (window status)))
         (url-display (multiple-value-bind (aesthetic safe)
                          (render-url (url buffer))
                        (uiop:strcat
                         (if safe
                             (format nil "~a (~a)" safe aesthetic)
                             ;; RFC 2068 says 255 bytes is recommended max, thats 32 characters
                             ;; 62 is the average, so 32 should be ample for the nessecary info
                             (str:prune 32 aesthetic :ellipsis "…"))
                         (when (title buffer)
                           (str:concat " — " (title buffer)))
                         (when (find (url buffer) (remove buffer (buffer-list))
                                     :test #'url-equal :key #'url)
                           (format nil " (buffer ~a)" (id buffer)))))))
    (spinneret:with-html-string
      (:nbutton :buffer status :text url-display :title url-display
        '(nyxt:set-url)))))

(defun enabled-modes-string (buffer)
  (when (modable-buffer-p buffer)
    (format nil "~{~a~^~%~}" (mapcar #'princ-to-string (serapeum:filter #'enabled-p (modes buffer))))))

(defmethod my-format-minions ((status status-buffer))
  (let ((buffer (current-buffer (window status))))
    (if (modable-buffer-p buffer)
        (spinneret:with-html-string
          (:nbutton
            :buffer status
            :text ";-"
            :title (str:concat "Enabled modes: " (enabled-modes-string buffer))
            '(nyxt:toggle-modes)))
    "")))

(defmethod my-format-modes ((status status-buffer))
  (let ((buffer (current-buffer (window status))))
    (if (modable-buffer-p buffer)
      (str:concat "(" (enabled-modes-string buffer) ")")
      "")))

(defmethod format-status ((status status-buffer))
  (let* ((buffer (current-buffer (window status)))
         (buffer-count (1+ (or (position buffer
                                         (sort (buffer-list) #'url-equal :key #'url))
                               0))))
    (spinneret:with-html-string
      (:div :id "container"
            ;; for looks, I should probably make this functional
            (:div :id "vi-mode" "U:**-")
            (:div :id "buffers"
                  (format nil "[~a/~a]"
                      buffer-count
                      (length (buffer-list))))
            ;;(:div :id "percentage"
            ;;      (format nil "L~a"
            ;;          (%percentage)))
             (:div :id "url"
                   (:raw
                    (my-format-status-load-status status)
                    (my-format-status-url status)))
             (:div :id "tabs"
                   (:raw
                    (format-status-tabs status)))
             (:div :id "minions"
                   (:raw 
                    (my-format-minions status)))
             (:div :id "modes"
                   (:raw
                     (my-format-modes status)))))))

;; style
(define-configuration :status-buffer
  ((height 36)
   (style
    (theme:themed-css (theme *browser*)
      `(*
        :font-family ,*mono*
        :font-size "11px")
      `(body
        :margin "9px"
        :margin-top "11px"
        :background-color ,*base02-* 
        :color ,*base05-*)
      `("#container"
        :display "flex"
        :white-space "nowrap"
        :overflow "hidden")
      `("#vi-mode, #buffers, #load, #percentage, #url, .tab, #minions, #modes"
        :padding-left "9px")
      `("#modes"
        :color "#a2a9b0")
      `(button
        :all "unset")
      `((:and (:or .button .tab "#url") :hover)
        :font-weight "bold"
        :cursor "pointer")))))
