; client.ss - IRC Client library
;
;   Copyright (c) 2009  Higepon(Taro Minowa)  <higepon@users.sourceforge.jp>
;
;   Redistribution and use in source and binary forms, with or without
;   modification, are permitted provided that the following conditions
;   are met:
;
;   1. Redistributions of source code must retain the above copyright
;      notice, this list of conditions and the following disclaimer.
;
;   2. Redistributions in binary form must reproduce the above copyright
;      notice, this list of conditions and the following disclaimer in the
;      documentation and/or other materials provided with the distribution.
;
;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;
;  $Id: client.ss 621 2008-11-09 06:22:47Z higepon $

#|
    Title: IRC Client

    Simple IRC Client.


    Example:
    (start code)
    ;; Just echo back
    (import (rnrs)
            (match)
            (mosh irc client))

    (irc-client
     "irc.freenode.net" "6666" "kaela" "#higepon"
     (lambda (msg return privmsg send)
       (match msg
         [('PRIVMSG who message)
          (privmsg message)]
         [else #f])))
    (end code)

    Example2:
    (start code)
    ;; Logger
    (import (rnrs)
            (mosh)
            (match)
            (only (srfi :19 time) date->string current-date)
            (mosh irc client))
    (irc-client
     "irc.freenode.net" "6666" "kaela" "#higepon"
     (lambda (msg return privmsg send)
       (match msg
         [('PRIVMSG who message)
          (format #t "~a <~a> ~a\n" (date->string (current-date) "~H:~M") who message)]
         [('STATUS 433 messaage)
          (error 'irc (format "~a" messaage))]
         [('NICK from to)
          (format #t "nick from =~a to =~a\n" from to)]
         [('PART name)
          (format #t "part name=~a\n" name)]
         [('JOIN name)
          (format #t "join name=~a\n" name)]
         [('TOPIC who topic)
          (format #t "topic who=~a topic=~a\n" who topic)]
         [('ERROR e)
          (return e)]
         [('RAW text)
          #;(format (current-error-port) "LOG:~a\n" text)
          #f]
         [else #f])))
    (end code)

    library: (mosh irc client)

    IRC Client Library
|#

(library (mosh irc client)
  (export irc-client)
  (import (rnrs)
          (mosh)
          (mosh socket))

  #|
      Function: irc-client

      Prototype:
      > (irc-client server port nick channel client-proc)

  |#
  (define (irc-client server port nick channel irc-client)
    (let ([socket (make-client-socket server port)])
      (define (send text)
        (assert (<= (string-length text) 510))
        (socket-send socket (string->utf8 (string-append text "\r\n"))))
      (define (recv)
        (utf8->string (socket-recv socket 512)))
      (define (privmsg text)
        (send (format "PRIVMSG ~a :~a" channel text)))
      (send (format "NICK ~a" nick))
      (send (format "USER ~a 0 * :~a" nick nick))
      (send (format "JOIN ~a" channel))
      (call/cc
       (lambda (return)
         (let loop ([data (recv)])
           (irc-client `(RAW ,data) return privmsg send)
           (cond
            [(zero? (string-length data))
             (irc-client (list 'CLOSED ) return privmsg send)
             (return)]
            [(#/:([^!]+).*PRIVMSG[^:]+:(.*)/ data) =>
             (lambda (m)
               (irc-client (list 'PRIVMSG (m 1) (m 2)) return privmsg send))]
            [(#/^PING/ data)
             (send "PONG 0")]
            [(#/:[^\s]+\s+(\d+).*:(.+)/ data) =>
             (lambda (m)
               (irc-client `(STATUS ,(string->number (m 1)) ,(m 2)) return privmsg send))]
            [(#/:([^!]+).*NICK.*:(.*)/ data) =>
             (lambda (m) (irc-client `(NICK ,(m 1) ,(m 2)) return privmsg send))]
            [(#/:([^!]+).*JOIN/ data) =>
             (lambda (m) (irc-client `(JOIN ,(m 1)) return privmsg send))]
            [(#/:([^!]+).*PART/ data) =>
             (lambda (m) (irc-client `(PART ,(m 1)) return privmsg send))]
            [(#/:([^!]+).*TOPIC.*:(.*)/ data) =>
             (lambda (m) (irc-client `(TOPIC ,(m 1) ,(m 2)) return privmsg send))]
            [else #f])
            (loop (recv)))))
       (socket-close socket)))
)
