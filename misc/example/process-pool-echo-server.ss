; process-pool-echo-server.ss - Sample of Multi-Process pooled Echo server.
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
        (srfi :42)
        (mosh)
        (mosh concurrent)
        (mosh socket))

(define (make-pool num)
  (let loop ([i 0]
             [ret '()])
    (cond
     [(= i num) ret]
     [else
      (let ([pid (spawn
                  (lambda (arg)
                    (define (main-loop supervisor index)
                      (format #t "main-loop ~d\n" index)
                       (receive
                           [('connection conn)
                            (format #t "child<~d> start echo\n" index)
                            (let loop ([data (socket-recv conn 100)])
                              (cond
                               [(zero? (bytevector-length data))
                                (socket-close conn)
                                (display "Echo server: STOP\n")
                                (! supervisor `(done ,(self)))
                                (main-loop supervisor index)]
                               [else
                                (display "before crash")
 ;                               (car 3)
                                (format #t "received ~s\n" (utf8->string data))
                                (socket-send conn data 0)
                                (loop (socket-recv conn 100))]))]))
                    (receive
                      [('supervisor supervisor index)
                       (format #t "child start \n")
                       (main-loop supervisor index)])
                    )
                  '()
                '((rnrs) (mosh concurrent) (mosh) (mosh socket)))])
        (! pid `(supervisor ,(self) ,i))
        (loop (+ i 1) (cons pid ret)))])))

(define (process-listen)
  (let ([pid (spawn
              (lambda (arg)
                (let ([server (make-server-socket "4649")])
                  (receive
                      [('supervisor supervisor)
                       (let loop ([conn (socket-accept server)])
                         (! supervisor `(connection ,conn))
                         (loop (socket-accept server)))))))
              '()
              '((rnrs) (mosh concurrent) (mosh) (mosh socket)))])
    (! pid `(supervisor ,(self)))
    pid))

  (let ([ready* (make-pool 20)]
        [working* '()]
        [listen (process-listen)])
    (define (loop)
    (receive
      [('exit why)
       (display why)]
      [('connection conn)
       (display "conn comes\n")
       (cond
        [(pair? ready*)
         (set! working* (append  working* (list (car ready*))))
         (! (car ready*) `(connection ,conn))
         (set! ready* (remq (car ready*) ready*))
         (loop)]
        [else
           (error 'pool "not enough pool")])]
      [('done from)
       (set! working* (remq from working*))
       (set! ready* (append ready* (list from)))
       (loop)]))
    (loop)
    (join! listen))
