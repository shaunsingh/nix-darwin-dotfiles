(in-package #:nyxt-user)

;;; GRIM

#+linux
(define-command-global screenshot ()
  "Take a screenshot"
  (run-thread "ScreenCapture loader"
    (sleep 0.3)
    (uiop:launch-program "grim")))

#+linux
(define-command-global screenshot-to-clipboard ()
  "Take a screenshot & copy to clipboard"
  (run-thread "ScreenCapture loader"
    (sleep 0.3)
    (uiop:launch-program "grim - | wl-copy")))

#+linux
(define-command-global screenshot-region ()
  "Take a screenshot and copy to clipboard"
  (run-thread "ScreenCapture loader"
    (sleep 0.3)
    (uiop:launch-program "grim -g \"$(slurp)\" - -t png | wl-copy -t image/png")))

;;; WF-RECORDER

#+linux
(define-command-global screen-record ()
  "Take a recording of the current display"
  (run-thread "ScreenCapture loader"
    (sleep 0.3)
    (uiop:launch-program "wf-recorder")))

#+linux
(define-command-global screen-record-region ()
  "Take a recording of a region on the current display"
  (run-thread "ScreenCapture loader"
    (sleep 0.3)
    (uiop:launch-program "wf-recorder -g \"$(slurp)\"")))
