(import (rnrs)
        (mosh)
        (match)
        (mosh ffi)
        (only (srfi :19 time) date->string current-date)
        (mosh irc client))

(display size-of-bool)
(display size-of-short)
(display  size-of-int)
(display  size-of-long)
(display  size-of-void*)
(display  size-of-size_t)
(display align-of-bool)
(display align-of-short)
(display align-of-int)
(display align-of-long)
(display align-of-void*)
(display align-of-size_t)
(display align-of-float)
(display align-of-double)
(display align-of-int8_t)
(display align-of-int16_t)
(display align-of-int32_t)
(display align-of-int64_t)
(display on-darwin)
(display on-linux)
(display on-freebsd)
(display on-openbsd)
(display on-windows)


;; (define (irc-bot server port nick channel irc-client)
;;   (let ([socket (make-client-socket server port)])
;;     (define (send text)
;;       (assert (<= (string-length text) 510))
;;       (socket-send socket (string->utf8 (string-append text "\r\n"))))
;;     (define (recv)
;;       (utf8->string (socket-recv socket 512)))
;;     (define (say text)
;;       (send (format "PRIVMSG ~a :~a" channel text)))
;;     (send (format "NICK ~a" nick))
;;     (send (format "USER ~a 0 * :~a" nick nick))
;;     (send (format "JOIN ~a" channel))
;;     (call/cc (lambda (return)
;;     (let loop ([data (recv)])
;;       (irc-client `(RAW ,data) return say send)
;;       (cond
;;        [(zero? (string-length data))
;;         (irc-client (list 'CLOSED ) return say send)
;;         (return)
;;         ]
;;        [(#/:([^!]+).*PRIVMSG[^:]+:(.*)/ data) =>
;;         (lambda (m)
;;           (irc-client (list 'PRIVMSG (m 1) (m 2)) return say send))]
;;        [(#/^PING/ data)
;;         (send "PONG 0")]
;;        [(#/:[^\s]+\s+(\d+).*:(.+)/ data) =>
;;         (lambda (m)
;;         (irc-client `(STATUS ,(string->number (m 1)) ,(m 2)) return say send))
;;         ]
;;        [(#/:([^!]+).*NICK.*:(.*)/ data) =>
;;         (lambda (m) (irc-client `(NICK ,(m 1) ,(m 2)) return say send))
;;         ]
;;        [(#/:([^!]+).*JOIN/ data) =>
;;         (lambda (m) (irc-client `(JOIN ,(m 1)) return say send))
;;         ]
;;        [(#/:([^!]+).*PART/ data) =>
;;         (lambda (m) (irc-client `(PART ,(m 1)) return say send))
;;         ]
;;        [(#/:([^!]+).*TOPIC.*:(.*)/ data) =>
;;         (lambda (m) (irc-client `(TOPIC ,(m 1) ,(m 2)) return say send))
;;         ]
;;        [else
;;         (format #t "data=~a\n" data)])

;;       (loop (recv)))))
;;     (socket-close socket)))

;; (irc-client
;;  "irc.freenode.net" "6666" "kaela" "#higepon"
;;  (lambda (msg return privmsg send)
;;    (match msg
;;      [('PRIVMSG who message)
;;       (format #t "~a <~a> ~a\n" (date->string (current-date) "~H:~M") who message)]
;;      [('STATUS 433 messaage)
;;       (error 'irc (format "~a" messaage))]
;;      [('NICK from to)
;;       (format #t "nick from =~a to =~a\n" from to)]
;;      [('PART name)
;;       (format #t "part name=~a\n" name)]
;;      [('JOIN name)
;;       (format #t "join name=~a\n" name)]
;;      [('TOPIC who topic)
;;       (format #t "topic who=~a topic=~a\n" who topic)]
;;      [('ERROR e)
;;       (return e)]
;;      [('RAW text)
;;       #;(format (current-error-port) "LOG:~a\n" text)
;;       #f]
;;      [else #f])))
