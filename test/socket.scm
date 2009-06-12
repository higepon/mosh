(import (rnrs)
        (mosh socket)
        (only (srfi :13) string-contains)
        (mosh test))

(let ([server (make-server-socket "34565")])
  ;; predicate
  (test-true (socket? server))
  (test-false (bytevector? server))

  ;; port
  (test-true (binary-port? (socket-port server)))
  (socket-shutdown server SHUT_RD)
  (socket-close server))

(let ([server (make-server-socket "34565" AF_INET SOCK_DGRAM IPPROTO_UDP)])
  ;; predicate
  (test-true (socket? server))
  (test-false (bytevector? server))

  ;; port
  (test-true (binary-port? (socket-port server)))
  (socket-shutdown server SHUT_RDWR)
  (socket-close server))

(guard (ex
        [(i/o-error? ex)
         (test-true (and "Can't connected to Google, not connected to the Internet?" #f))]
        [else
         (raise ex)])
       (let ([client (make-client-socket "wiki.monaos.org" "http")])
         (test-true (socket? client))
         (test-true (> (socket-send client(string->utf8 "GET / HTTP/1.1\r\nHOST: wiki.monaos.org\r\n\r\n")) 0))
         (socket-shutdown client SHUT_WR)
         (test-true (string-contains (utf8->string (socket-recv client 100)) "HTTP/1.1"))
         ))
(test-results)
