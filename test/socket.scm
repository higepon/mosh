(import (rnrs)
        (mosh socket)
        (only (srfi :13) string-contains)
        (http)
        (json)
        (mosh test))

;; (let ([server (make-server-socket "34565")])
;;   ;; predicate
;;   (test-true (socket? server))
;;   (test-false (bytevector? server))

;;   ;; port
;;   (test-true (binary-port? (socket-port server)))
;;   (socket-shutdown server SHUT_RD)
;;   (socket-close server))

;; (let ([server (make-server-socket "34565" AF_INET SOCK_DGRAM IPPROTO_UDP)])
;;   ;; predicate
;;   (test-true (socket? server))
;;   (test-false (bytevector? server))

;;   ;; port
;;   (test-true (binary-port? (socket-port server)))
;;   (socket-shutdown server SHUT_RDWR)
;;   (socket-close server))

;; (guard (ex
;;         [(i/o-error? ex)
;;          (test-true (and "Can't connected to Google, not connected to the Internet?" #f))]
;;         [else
;;          (raise ex)])
;;        (let ([client (make-client-socket "wiki.monaos.org" "80")])
;;          (test-true (socket? client))
;;          (test-true (> (socket-send client(string->utf8 "GET / HTTP/1.1\r\nHOST: www.google.co.jp\r\n\r\n")) 0))
;;          (let ([s (utf8->string (socket-recv client 100))])
;;            (test-true (string-contains s "HTTP/1.1")))
;;           )
;;  )

;; (when (ssl-supported?)
;;   (let ([client (make-client-socket "www.hatena.ne.jp" "443")])
;;     (test-false (ssl-socket? client))
;;     (socket-sslize! client)
;;     (test-true (ssl-socket? client))
;;     (test-true (> (socket-send client (string->utf8 "GET / HTTP/1.1\r\nHOST: www.hatena.ne.jp\r\n\r\n")) 0))
;;     (let ([s (utf8->string (socket-recv client 100))])
;;       (test-true (string-contains s "HTTP/1.1")))
;;     ))

(when (ssl-supported?)
  (test-equal "19292868552" (cdr (vector-ref (json-read (open-string-input-port (http-get "https://graph.facebook.com/19292868552"))) 0)))
  )

(test-true (string-contains (http-get "http://www.hatena.ne.jp") "</html>"))
(test-true (string-contains (http-get "http://www.hatena.ne.jp:80") "</html>"))
(test-true (string-contains (http-get "http://www.hatena.ne.jp:80/") "</html>"))
(test-results)
