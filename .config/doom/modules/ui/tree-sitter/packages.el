;; -*- no-byte-compile: t; -*-
;;; ui/tree-sitter/packages.el

(package! tree-sitter
  :ignore (null (bound-and-true-p module-file-suffix))
  :pin "7f5d0938002092ec08830a73f64961021303e1e9")
(package! tree-sitter-langs
  :ignore (null (bound-and-true-p module-file-suffix))
  :pin "5d362ce98dcf656d7a55fcad6ae21c0a2caca861")
