(in-package #:nyxt-user)

;;; MPV

(defun execute-mpv (link)
  (uiop:launch-program (list "mpv" link) :ignore-error-status t))

(define-command-global mpv-hint ()
  "Show a set of element hints, and go to the user inputted one in the
currently active buffer."
  (nyxt/mode/hint:query-hints
   "open video in mpv"
   (lambda (hint)
     (let ((hint (if (listp hint) (car hint) hint)))
       (echo "~A" hint)
       (case (type-of hint)
         (nyxt/dom:a-element
          (echo "MPV launched with ~a" (url hint))
          (execute-mpv (quri:render-uri (url hint))))
         (t
          (echo "failed to launch mpv")
          (print (type-of hint))
          (print hint)))))))

(define-command-global open-mpv ()
  "executes mpv on the current buffer"
  (execute-mpv (quri:render-uri (url (current-buffer)))))

(defmethod url-sources-no-suggestions ((buffer buffer) return-actions)
  (append
   (list (make-instance 'global-history-source :actions-on-return return-actions)
         (make-instance 'nyxt/mode/search-buffer:search-buffer-source :actions-on-return return-actions))
   (alexandria:mappend (alexandria:rcurry #'url-sources return-actions) (modes buffer))))

(define-command-global mpv-url (&key (prefill-current-url-p t))
  "open an url in mpv"
  (let ((history (set-url-history *browser*)))
    (when history
      (containers:insert-item history (url (current-buffer))))
    (flet ((func (urls)
             (let* ((url (car urls))
                    (url-string
                      (cond ((typep url 'history-entry) (render-url (url url)))
                            ((stringp url)              url)
                            ((valid-url-p url)          (render-url url))
                            (t                          (render-url (url url))))))
               (echo "MPV launched with ~a" url)
               (execute-mpv url-string))))
      (prompt
       :prompt (format nil "Launch mpv on")
       :input (if prefill-current-url-p
                  (quri:render-uri (url (current-buffer))) "")
       :sources
       (url-sources-no-suggestions (current-buffer) (list #'func))
       :history history))))


