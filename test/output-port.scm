; output-port.scm - Tests for <output port>
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
;  $Id: test.ss 621 2008-11-09 06:22:47Z higepon $


(import (rnrs)
        (mosh)
        (mosh test))

;; custom output-port
(let* ([accum '()]
       [p (make-custom-binary-output-port
           "custom out"
           (lambda (bv start count)
             (let ([bv2 (make-bytevector count)])
               (bytevector-copy! bv start bv2 0 count)
               (set! accum (append
                            (reverse (bytevector->u8-list bv2))
                            accum))
               count))
           (lambda () (length accum))
           (lambda (pos) (set! accum (list-tail accum (- (length accum) pos))))
           (lambda () 'ok))])
  (test/t (port-has-port-position? p))
  (test/t (port-has-set-port-position!? p))
  (test* (port-position p) 0)
  (put-bytevector p #vu8(2 4 6))
  (flush-output-port p)
  (test* accum '(6 4 2))
  (test* (port-position p) 3)
  (set-port-position! p 2)
  (test* (port-position p) 2)
  (test* accum '(4 2))
  ;(put-bytevector p #vu8(3 7 9 11) 2 1)
  ;(flush-output-port p)
  ;(test* accum '(9 4 2))
  ;(close-port p)
  )

;; standard-output-port doesn't suport port-position on Mosh.
(test/f (port-has-port-position? (standard-output-port)))
(test/f (port-has-set-port-position!? (standard-output-port)))
(test/violation? (set-port-position! (standard-output-port) 0))
(test/violation? (port-position (standard-output-port)))

;; textual-output-port doesn't suport port-position on Mosh.
(test/f (port-has-set-port-position!? (current-output-port)))
(test/f (port-has-port-position? (current-output-port)))
(test/violation? (set-port-position! (current-output-port) 0))
(test/violation? (port-position (current-output-port)))

;; string-output-port
(let-values (([port get-string] (open-string-output-port)))
  (test/t (port-has-set-port-position!? port))
  (test/t (port-has-port-position? port))
  (test* (port-position port) 0)
  (display #\a port)
  (test* (port-position port) 1)
  (set-port-position! port 2)
  (display "cd" port)
  (test* (port-position port) 4)
  (set-port-position! port 1)
  (display #\b port)
  (test* (get-string) "abcd")
  (test* (port-position port) 0)
  (test* (get-string) "")
  (display #\a port)
  (test* (get-string) "a")
  (test* (port-position port) 0)
)

(test-end)
