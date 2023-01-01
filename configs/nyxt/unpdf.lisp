(in-package #:nyxt-user)

;; I'm definining a new scheme to redirect PDF requests to. What it does is:
;; - Get the original file (if the URL is a filesystem path, simply use it).
;; - Save it to disk (if remote).
;; - Run pdftotext over the file.
;; - Display pdftotext output in a nice HTML page with interlinkable
;;   page numbers and page contents as <pre> tags.
(define-internal-scheme "unpdf"
    (lambda (url buffer)
      (let* ((url (quri:uri url))
             (original-url (quri:uri (quri:url-decode (quri:uri-path url))))
             (local-p (or (null (quri:uri-scheme original-url))
                          (string= "file" (quri:uri-scheme original-url))))
             (original-content (unless local-p
                                 (dex:get (quri:render-uri original-url) :force-binary t))))
        (flet ((display-pdf-contents (file)
                 (if (uiop:file-exists-p file)
                     (let ((pages (ignore-errors
                                   (uiop:split-string
                                    (uiop:run-program `("pdftotext" "-nodiag" ,(uiop:native-namestring file) "-")
                                                      :output '(:string :stripped t))
                                    :separator '(#\Page)))))
                       (spinneret:with-html-string
                         (:head
                          (:style (style buffer))
                          ;; A class to override the <pre> colors.
                          (:style (theme:themed-css (theme *browser*)
                                    #+nyxt-3-pre-release-1
                                    (.override
                                     :background-color theme:background
                                     :color theme:on-background
                                     :font-size "150%"
                                     :line-height "150%")
                                    #+(and nyxt-3 (not nyxt-3-pre-release-1))
                                    `(.override
                                      :background-color ,theme:background
                                      :color ,theme:on-background
                                      :font-size "150%"
                                      :line-height "150%"))))
                         (loop for page in pages
                               for number from 1
                               unless (uiop:emptyp page)
                                 do (:section
                                     :id (princ-to-string number)
                                     (:h2.override (:a :href (format nil "#~d" number))
                                              (princ-to-string number))
                                     (:pre.override (or page ""))))))
                     "")))
          (if local-p
              (display-pdf-contents (pathname (quri:uri-path original-url)))
              (uiop:with-temporary-file (:pathname path :type "pdf" :keep t)
                (log:debug "Temp file for ~a is ~a" url path)
                (alexandria:write-byte-vector-into-file
                 (coerce original-content '(vector (unsigned-byte 8))) path :if-exists :supersede)
                (display-pdf-contents path))))))
  :local-p t)

(define-command-global unpdf-download-this ()
  "A helper for unpdf: pages to download the original PDF to the regular destination.
Unpdf redirects all requests, even those that you need to read
elsewhere, thus I need this command."
  (let* ((buffer (current-buffer))
         (url (url buffer)))
    (if (string= "unpdf" (quri:uri-scheme url))
        (ffi-buffer-download buffer (quri:uri-path url))
        ;; I need to turn it into a mode someday...
        (echo-warning "This command is for unpdf: pages only, it's useless elsewhere!"))))

(defun redirect-pdf (request-data)
  (if (and (toplevel-p request-data)
           (uiop:string-prefix-p "application/pdf" (mime-type request-data)))
      ;; I should somehow prompt about downloading instead...
      (progn
        (echo "Redirecting to the unpdf URL...")
        (make-buffer-focus :url (quri:uri (str:concat "unpdf:" (render-url (url request-data)))))
        ;; Return nil to prevent Nyxt from downloading this PDF.
        nil)
      request-data))

(define-configuration web-buffer
  ((request-resource-hook (hooks:add-hook %slot-value% 'redirect-pdf))))

(define-configuration nyxt/file-manager-mode:file-source
  ((nyxt/file-manager-mode:supported-media-types `("pdf" ,@%slot-value%))))
