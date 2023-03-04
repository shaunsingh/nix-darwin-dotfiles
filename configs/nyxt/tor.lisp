(in-package #:nyxt-user)

(define-mode tor-proxy-mode (nyxt/proxy-mode:proxy-mode)
  "Set proxy to local Tor SOCKS5 proxy."
  ((nyxt/proxy-mode:proxy (make-instance 'proxy
                                         :url (quri:uri "socks5://localhost:9050")
                                         :allowlist '("localhost")
                                         :proxied-downloads-p t))))



