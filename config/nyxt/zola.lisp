(in-package #:nyxt-user)

;;; ZOLA

(defun my-prompt-for-file ()
  (uiop:native-namestring
   (pathname
    (prompt1
     :prompt "Open file"
     :extra-modes 'nyxt/mode/file-manager:file-manager-mode
     :input (uiop:native-namestring (uiop:getcwd))
     :sources
     (list (make-instance 'nyxt/mode/file-manager:file-source
                          :name "Existing file"
                          :actions-on-return #'identity)
           (make-instance 'prompter:raw-source
                          :name "Create new file"))))))

(defun my-prompt-for-directory ()
  "Prompt the user to select a directory."
  (uiop:native-namestring
   (pathname
    (prompt1 
     :prompt "Select Directory"
     :extra-modes 'nyxt/mode/file-manager:file-manager-mode
     :input (uiop:native-namestring (uiop:getcwd))
     :sources 
     (list 
      (make-instance 'nyxt/mode/file-manager:file-source
                     :name "Directories"
                     :allow-directories t
                     :path-filter #'uiop:directory-pathname-p))))))

(defun find-zola-config-directory (file-path)
  "Find the nearest parent directory containing config.toml."
  (let ((dir (uiop:pathname-directory-pathname file-path)))
    (loop while dir
          when (probe-file (merge-pathnames "config.toml" dir))
            return dir
          do (setf dir (uiop:pathname-parent-directory-pathname dir)))))

(defun run-zola-serve (directory)
  "Run zola serve in the specified directory."
  (uiop:launch-program 
   (format nil "cd ~a && zola serve -p 1111 --interface 127.0.0.1" 
           (uiop:native-namestring directory))
   :output :interactive
   :error-output :interactive))

(defun kill-zola ()
  "Kill the zola pricess"
  (uiop:launch-program "pkill zola"
   :output :interactive
   :error-output :interactive))

(define-command-global zola-preview ()
  "Preview Zola project in a new buffer"
  (let* ((selected-directory (my-prompt-for-directory))
         (zola-uri (quri:uri "http://localhost:1111")))
      (run-zola-serve selected-directory)
      (make-buffer-focus :url zola-uri)))

#+linux
(define-panel-command-global zola-preview-split () 
  (panel "*zola preview*" :right) 
  "Open the Zola preview of the current markdown file on the right buffer"
  (run-thread "zola preview loader" 
    (setf (ffi-width panel) (round (/ (ffi-width (current-window)) 2)))
    (buffer-load (quri:uri "http://localhost:1111") :buffer panel))
  "")

#+linux
(define-command-global edit-and-preview-with-zola (&key (file (my-prompt-for-file)))
  "Open a markdown file with editor and start Zola preview if possible."
  (let ((buffer (make-instance 'my-editor-buffer 
                               :url (quri:make-uri :scheme "editor" :path file)))
        (zola-dir (find-zola-config-directory file)))
    (set-current-buffer buffer)
    (if zola-dir
        (progn
          (run-zola-serve zola-dir)
          (zola-preview-split)
          (echo "Zola preview started for directory: ~a" zola-dir))
        (echo "No Zola config.toml found in parent directories"))))

(define-command-global zola-kill ()
  "Stop Zola server"
  (kill-zola))

#+linux
(define-command-global close-zola-preview-split ()
  "Close Zola preview window and stop Zola server"
  (delete-all-panel-buffers)
  (kill-zola))


