;;;; nx-dark-reader.lisp

(in-package #:nx-dark-reader)

(nyxt::define-mode dark-reader-mode ()
  "A mode to load Dark Reader script and run it on the page.

For the explanation of `brightness', `contrast', `grayscale', `sepia', `background-color',
`text-color', `selection-color', `use-font', `font-family', `text-stroke', `stylesheet'
see Dark Reader docs and examples. The default values are mostly sensible, though."
  ((glyph "Δ")
   (script nil)
   (brightness
    100
    :type integer)
   (contrast
    100
    :type integer)
   (grayscale
    0
    :type integer)
   (sepia
    0
    :type integer)
   (background-color
    nil
    :type (or null string))
   (text-color
    nil
    :type (or null string))
   (selection-color
    nil
    :type (or null string))
   (use-font
    nil
    :type boolean)
   (font-family
    nil
    :type (or null string))
   (text-stroke
    nil
    :type (or null integer))
   (stylesheet
    nil
    :type (or null string))))

(defmethod enable ((mode dark-reader-mode) &key)
  (let ((dark-reader
          (make-instance
           'nyxt/user-script-mode:user-script
           :code (with-slots (brightness contrast grayscale sepia
                              background-color text-color selection-color
                              use-font font-family text-stroke
                              stylesheet)
                     mode
                   (format nil "// ==UserScript==
// @name          Dark Reader (Unofficial)
// @icon          https://darkreader.org/images/darkreader-icon-256x256.png
// @namespace     DarkReader
// @description	  Inverts the brightness of pages to reduce eye strain
// @version       4.7.15
// @author        https://github.com/darkreader/darkreader#contributors
// @homepageURL   https://darkreader.org/ | https://github.com/darkreader/darkreader
// @run-at        document-end
// @include       http://*/*
// @include       https://*/*
// @grant         none
// @require       https://cdn.jsdelivr.net/npm/darkreader/darkreader.min.js
// @noframes
// ==/UserScript==

DarkReader.setFetchMethod(window.fetch);
DarkReader.enable({
	brightness: ~d,
	contrast: ~d,
    grayscale: ~d,
	sepia: ~d,
    ~:[~;darkSchemeBackgroundColor: ~s,~]
    ~:[~;darkSchemeTextColor: ~s,~]
    ~:[~;selectionColor: ~s,~]
    ~:[~*~;useFont: true, fontFamily: ~s,~]
    ~:[~;textStroke: ~d,~]
    ~:[~;stylesheet: ~s,~]
});" brightness contrast grayscale sepia
background-color background-color
text-color text-color
selection-color selection-color
use-font font-family
text-stroke text-stroke
stylesheet stylesheet)))))
    (ffi-buffer-add-user-script (buffer mode) (setf (script mode) dark-reader))))

(defmethod disable ((mode dark-reader-mode) &key)
  (ffi-buffer-remove-user-script (buffer mode) (script mode)))
