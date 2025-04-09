(in-package #:nyxt-user)

;; LAUNCHER

(defun string-remove-suffix (suffix string)
  "Remove SUFFIX from STRING if it is present."
  (if (and suffix 
           string 
           (>= (length string) (length suffix))
           (string= suffix (subseq string (- (length string) (length suffix)))))
      (subseq string 0 (- (length string) (length suffix)))
      string))

;; these are symlinks so might have issues?
(defun get-installed-applications ()
  #+linux
  "Retrieve a list of installed applications from NixOS applications directory."
  (remove-duplicates
   (remove nil
    (mapcar (lambda (file)
              (let ((filename (file-namestring file)))
                (when (and (str:ends-with? ".desktop" filename)
                           (not (str:starts-with? "." filename))
                           (not (string= filename "mimeinfo.cache")))
                  (string-remove-suffix ".desktop" filename))))
            (directory "/run/current-system/sw/share/applications/*.desktop"))
   :test #'string=)))

(defun prompt-application ()
  #+linux
  "Prompt user to select an application to launch."
  (let ((apps (get-installed-applications)))
    (first 
     (prompt 
      :prompt "Select Application to Launch"
      :sources (make-instance 
                'prompter:source 
                :name "Installed Applications"
                :constructor apps)))))

#+linux
(define-command-global launch-application ()
  "Launch a selected application."
  (let ((app (prompt-application)))
    (uiop:launch-program (format nil "gtk-launch ~A" app))
    (echo "Launching application: ~A" app)))


