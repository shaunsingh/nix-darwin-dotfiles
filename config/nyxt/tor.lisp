(in-package #:nyxt-user)

;;; TOR

(define-mode tor-proxy-mode (nyxt/mode/proxy:proxy-mode)
  "Launch tor & set proxy to local Tor SOCKS5 proxy."
  ((uiop:launch-program "tor")
    (nyxt/mode/proxy:proxy (make-instance 
                            'proxy
                            :url (quri:uri "socks5://localhost:9050")
                            :allowlist '("localhost")
                            :proxied-downloads-p t))))
