; echo-client.ss - Sample of Echo client
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
;  $Id: socket.ss 621 2008-11-09 06:22:47Z higepon $

(import (rnrs)
        (mosh)
        (mosh concurrent)
        (mosh socket))

(define (create-echo-clients num)
  (let loop ([i 0]
             [ret '()])
    (cond
     [(= i num) ret]
     [else
      (loop (+ i 1)
            (cons (spawn (lambda (arg)
                           (let loop ()
                             (let ([socket (make-client-socket "127.0.0.1" "4649")])
                               (socket-send socket (string->utf8 "hello"))
                               (format #t "Reply from Server: ~a\n" (utf8->string (socket-recv socket 100)))
                               (socket-close socket)
                               (loop))))
                         '()
                         '((rnrs) (mosh concurrent) (mosh) (mosh socket)))
                  ret))])))

(let ([pid* (create-echo-clients 3)])
  (join! (car pid*)))

