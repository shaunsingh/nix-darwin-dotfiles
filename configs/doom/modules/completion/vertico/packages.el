;; -*- no-byte-compile: t; -*-
;;; completion/vertico/packages.el

(package! vertico
  :recipe (:host github :repo "minad/vertico"
           :files ("*.el" "extensions/*.el"))
  :pin "665660f91004e4c6aad1e18b9cdc256607228c42")

(package! orderless :pin "62f71c34baca0b7d0adeab4a1c07d85ffcee80d9")

(package! consult :pin "e10775aab1cd4ee4af1e610128508b81288dadca")
(package! consult-dir :pin "08f543ae6acbfc1ffe579ba1d00a5414012d5c0b")
(when (featurep! :checkers syntax)
  (package! consult-flycheck :pin "92b259e6a8ebe6439f67d3d7ffa44b7e64b76478"))

(package! embark :pin "cf147d92d9becbdfd850c4b1a2c7f4b0b4aae892")
(package! embark-consult :pin "cf147d92d9becbdfd850c4b1a2c7f4b0b4aae892")

(package! marginalia :pin "09d8ab38a5a4aa55a83968dc3e454d11fee05255")

(package! wgrep :pin "f9687c28bbc2e84f87a479b6ce04407bb97cfb23")

(when (featurep! +icons)
  (package! all-the-icons-completion :pin "a0f34d68cc12330ab3992a7521f9caa1de3b8470"))
