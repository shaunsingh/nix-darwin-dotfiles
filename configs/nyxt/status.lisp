(in-package #:nyxt-user)

;; quick parenscript to grab out position in the buffer
(define-parenscript %percentage ()
  (defun percentage ()
    (let* ((height-of-window (ps:@ window inner-height))
           (content-scrolled (ps:@ window page-y-offset))
           (body-height (if (not (or (eql window undefined)
                                     (eql (ps:@ window document) undefined)
                                     (eql (ps:chain window
                                                    document
                                                    (get-elements-by-tag-name "body"))
                                          undefined)
                                     (eql (ps:chain window
                                                    document
                                                    (get-elements-by-tag-name "body")
                                                    0)
                                          undefined)
                                     (eql (ps:chain window
                                                    document
                                                    (get-elements-by-tag-name "body")
                                                    0
                                                    offset-height)
                                          undefined)))
                          (ps:chain window
                                    document
                                    (get-elements-by-tag-name "body")
                                    0
                                    offset-height)
                          0))
           (total (- body-height height-of-window))
           (prc (* (/ content-scrolled total) 100)))
      (if (> prc 100)
          100
          (round prc))))
  (percentage))

;; hooks to make the percentage update after scroll
;; not sure where these are on nyxt-3

;; (hooks:add-hook nyxt/document-mode:scroll-to-top-after-hook
;;                 (hooks:make-handler-void #'nyxt::print-status))
;; (hooks:add-hook nyxt/document-mode:scroll-to-bottom-after-hook
;;                 (hooks:make-handler-void #'nyxt::print-status))
;; (hooks:add-hook nyxt/document-mode:scroll-page-up-after-hook
;;                 (hooks:make-handler-void #'nyxt::print-status))
;; (hooks:add-hook nyxt/document-mode:scroll-page-down-after-hook
;;                 (hooks:make-handler-void #'nyxt::print-status))
;; (hooks:add-hook nyxt/document-mode:scroll-down-after-hook
;;                 (hooks:make-handler-void #'nyxt::print-status))
;; (hooks:add-hook nyxt/document-mode:scroll-up-after-hook
;;                 (hooks:make-handler-void #'nyxt::print-status))
;; (hooks:add-hook nyxt/document-mode:scroll-to-top-after-hook
;;                 (hooks:make-handler-void #'nyxt::print-status))
;; (hooks:add-hook nyxt/document-mode:scroll-to-bottom-after-hook
;;                 (hooks:make-handler-void #'nyxt::print-status))

;; larger status buffer
(define-configuration status-buffer
  ((height 32)))

;; add a more minimal, comfy css
(defun my-status-style ()
  (theme:themed-css (theme *browser*)
    ;; by default nyxt sets a proportional font
    `(*
      :font-family ,*font*
      :font-size "11px")
    ;; add some padding around the body
    `(body
      :margin "9px"
      :background-color "#262626"
      :color "#f2f4f8")
    ;; let the statusline overflow
    `("#container"
      :display "flex"
      :white-space "nowrap"
      :overflow "hidden")
    ;; add a generous amount of padding around everything
    `("#vi-mode, #buffers, #percentage, #url, #minions, #tabs, #modes"
      :padding-left "9px")
    ;; url can be nice and bright
    `("#url"
      :color "#ffffff"
      :font-weight "bold")
    ;; modes can be dull and dark
    `("#modes"
      :color "#a2a9b0")
    ;; button tweaks incl vim color
    `(button
      :all "unset"
      :color "#dde1e6")
    ;; bold on hover
    `("button:hover"
      :font-weight "bold")))

;; change up the load status a bit
(defmethod my-format-status-load-status ((status status-buffer))
  (spinneret:with-html-string
    (:span (if (web-buffer-p (current-buffer))
               (case (slot-value (current-buffer) 'nyxt::status)
                 (:unloaded "∅ ")
                 (:loading "∞ ")
                 (:finished ""))
               ""))))

;; more minimal status url display
(defmethod my-format-status-url ((status status-buffer))
  (let* ((buffer (current-buffer (window status)))
         (content (multiple-value-bind (aesthetic safe)
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
      (:nbutton
        :buffer status
        :text content
        :title content
        (nyxt:set-url)))))

;; mimick emacs' minions
(defmethod my-format-minions ((status status-buffer))
  (let ((buffer (current-buffer (window status))))
    (if (modable-buffer-p buffer)
        (spinneret:with-html-string
          (:nbutton
            :buffer status
            :text ";-"
            :title (str:concat "Enabled modes: " (nyxt::modes-string buffer))
            (nyxt:toggle-modes)))
    "")))

;; redefine format-status to put changes in effect
(defmethod format-status ((status status-buffer))
  (let* ((buffer (current-buffer (window status)))
         (buffer-count (1+ (or (position buffer
                                         (sort (buffer-list) #'url-equal :key #'url))
                               0))))
    (setf (style status) (my-status-style))
    (spinneret:with-html-string
      (:div :id "container"
            ;; for looks, I should probably make this functional
            (:div :id "vi-mode" "U:**-")
            ;; display current/total buffesr
            (:div :id "buffers"
                  (format nil "[~a/~a]"
                      buffer-count
                      (length (buffer-list))))
            (:div :id "percentage"
                  (format nil "L~a"
                      (%percentage)))
            ;; format url and loading icon if nessecary
            (:div :id "url"
                  (:raw
                   (my-format-status-load-status status)
                   (my-format-status-url status)))
            ;; view modes
            (:div :id "minions"
                  (:raw 
                   (my-format-minions status)))
            ;; show open tabs
            (:div :id "tabs"
                  (:raw
                   (format-status-tabs status)))
            ;; show open tabs
            (:div :id "modes"
                  (:raw
                   "("
                   (nyxt::modes-string buffer)
                   ")"))))))
