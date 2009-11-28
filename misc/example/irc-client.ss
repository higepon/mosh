(import (rnrs)
        (mosh)
        (mosh socket))

(define (irc-bot server port nick channel)
  (let ([socket (make-client-socket server port)])
    (define (send text)
      (assert (<= (string-length text) 510))
      (socket-send socket (string->utf8 (string-append text "\r\n"))))
    (define (recv)
      (utf8->string (socket-recv socket 512)))
    (define (say text)
      (send (format "PRIVMSG ~a :~a" channel text)))
    (send (format "NICK ~a" nick))
    (send (format "USER ~a 0 * :~a" nick nick))
    (send (format "JOIN ~a" channel))
    (let loop ([data (recv)])
      (cond
       [(#/:([^!]+).*PRIVMSG[^:]+:(.*)/ data) =>
        (lambda (m)
          (format #t "<~a> ~a\n" (m 1) (m 2))
          (say (format "~s ってなあに？" (m 2))))]
       [(#/^PING/ data)
        (send "PONG 0")]
       [(#/:.*433.*Nickname is already in use.*/ data)
        (error 'irc "Nickname is already in use")])
      (loop (recv)))
    (socket-close socket)))

(irc-bot "irc.freenode.net" "6666" "kaela" "#higepon")
