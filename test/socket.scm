(import (rnrs)
        (mosh socket)
        (only (srfi :13) string-contains)
        (mosh test))

(test-begin "socket")

(test-begin "basic server")

(let ([server (make-server-socket "34565")])
  ;; predicate
  (test-true (socket? server))
  (test-false (bytevector? server))

  ;; port
  (test-true (binary-port? (socket-port server)))
  (socket-shutdown server SHUT_RD)
  (socket-close server))

(test-end) ; basic server

(test-begin "udp server")

(let ([server (make-server-socket "34565" AF_INET SOCK_DGRAM IPPROTO_UDP)])
  ;; predicate
  (test-true (socket? server))
  (test-false (bytevector? server))

  ;; port
  (test-true (binary-port? (socket-port server)))
  (socket-shutdown server SHUT_RDWR)
  (socket-close server))

(test-end) ; udp server

(test-begin "basic client")
(guard (ex
        [(i/o-error? ex)
         (test-assert (and "Can't connected to Google, not connected to the Internet?" #f))]
        [else
         (raise ex)])
       (let ([client (make-client-socket "wiki.monaos.org" "http")])
         (test-true (socket? client))
         (test-true (> (socket-send client(string->utf8 "GET / HTTP/1.1\r\nHOST: wiki.monaos.org\r\n\r\n")) 0))
         (socket-shutdown client SHUT_WR)
         (test-assert (string-contains (utf8->string (socket-recv client 100)) "HTTP/1.1"))
         ))
(test-end) ;; basic-client

(test-end)
