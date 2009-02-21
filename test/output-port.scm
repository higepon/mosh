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

#|
    (test/exn (open-file-output-port "io-tmp1"
                                     (file-options no-create))
              &i/o-file-does-not-exist)
    (test/exn (open-file-output-port "io-tmp1"
                                     (file-options no-create no-fail))
              &i/o-file-does-not-exist)
    (test/exn (open-file-output-port "io-tmp1"
                                     (file-options no-create no-truncate))
              &i/o-file-does-not-exist)
    (test/exn (open-file-output-port "io-tmp1"
                                     (file-options no-create no-fail no-truncate))
              &i/o-file-does-not-exist)

    ;; Create:
    (let ([p (open-file-output-port "io-tmp1")])
      (test (file-exists? "io-tmp1") #t)
      (test (port? p) #t)
      (test (binary-port? p) #t)
      (test (textual-port? p) #f)
      (test (output-port? p) #t)
      (test (input-port? p) #f)
      (test/unspec (flush-output-port p))
      (test/unspec (close-port p)))

    ;; Don't re-create:
    (test/exn (open-file-output-port "io-tmp1")
              &i/o-file-already-exists)
    (test/exn (open-file-output-port "io-tmp1" (file-options no-truncate))
              &i/o-file-already-exists)

    ;; Re-open if 'no-create is specified:
    (let ([p (open-file-output-port "io-tmp1"
                                    (file-options no-create))])
      (test/unspec (close-port p)))
    
    ;; Re-open if 'no-fail is specified:
    (let ([p (open-file-output-port "io-tmp1"
                                    (file-options no-fail))])
      (test/unspec (close-port p)))

    ;; Create if 'no-fail is specified and it doesn't exist:
    (test/unspec (delete-file "io-tmp1"))
    (let ([p (open-file-output-port "io-tmp1"
                                    (file-options no-fail no-truncate))])
      (test/unspec (close-port p)))
    (test/unspec (delete-file "io-tmp1"))
    (let ([p (open-file-output-port "io-tmp1"
                                    (file-options no-fail))])
      (test/unspec (put-bytevector p #vu8(99 101 98 100)))
      (test/unspec (close-port p)))

    ;; Check that 'no-truncate doesn't truncate:
    (let ([p (open-file-output-port "io-tmp1"
                                    (file-options no-fail no-truncate))])
      (test/unspec (put-bytevector p #vu8(97)))
      (test/unspec (close-port p)))
    (let ([p (open-file-output-port "io-tmp1"
                                    (file-options no-create no-truncate))])
      (test/unspec (put-bytevector p #vu8(96)))
      (test/unspec (close-port p)))
    (let ([p (open-file-output-port "io-tmp1"
                                    (file-options no-create no-truncate))])
      (test (port-has-port-position? p) #t)
      (test (port-has-set-port-position!? p) #t)
      (test (port-position p) 0)
      (test/unspec (set-port-position! p 6))
      (test (port-position p) 6)
      (test/unspec (put-bytevector p #vu8(102)))
      (test/unspec (close-port p)))
    ;; Otherwise, truncate:
    (let ([p (open-file-output-port "io-tmp1"
                                    (file-options no-fail))])
      (test/unspec (close-port p)))
    ;; ----------------------------------------
    ;; Check buffer modes? Just make sure they're accepted:

    (let ([p (open-file-output-port "io-tmp1" (file-options no-create) 'line)])
      (test (output-port-buffer-mode p) 'line)
      (close-port p))
    (let ([p (open-file-output-port "io-tmp1" (file-options no-create) 'block)])
      (test (output-port-buffer-mode p) 'block)
      (close-port p))
    (let ([p (open-file-output-port "io-tmp1" (file-options no-create) 'none)])
      (test (output-port-buffer-mode p) 'none)
      (close-port p))

    ;; ----------------------------------------
    ;; Transcoders

    (let ([p (open-file-output-port "io-tmp1" (file-options no-create) 
                                    'block (make-transcoder (latin-1-codec)))])
      (when (port-has-port-position? p)
        (test/unspec (port-position p))
        (when (port-has-set-port-position!? p)
          (let ([pos (port-position p)])
            (test/unspec (set-port-position! p pos)))))
      (test (binary-port? p) #f)
      (test (textual-port? p) #t)
      (test/unspec (put-string p "apple"))
      (test/unspec (put-string p "berry" 3))
      (test/unspec (put-string p "berry" 1 1))
      (close-port p))
    (let ([p (open-file-output-port "io-tmp1" (file-options no-create) 
                                    'block (make-transcoder (utf-8-codec)))])
      (test/unspec (put-string p "app\x3BB;e"))
      (close-port p))
    (let ([p (open-file-output-port "io-tmp1" (file-options no-create) 
                                    'block (make-transcoder (utf-16-codec)))])
      (test/unspec (put-string p "app\x3BB;e"))
      (close-port p))
    (let-values ([(p get) (open-bytevector-output-port)])
      (test (output-port? p) #t)
      (test (binary-port? p) #t)
      (test (textual-port? p) #f)
      (test/unspec (put-u8 p 10))
      (test/unspec (put-bytevector p #vu8(11 12 13)))
      (test/unspec (put-bytevector p #vu8(14 15 16 17 18) 4))
      (test/unspec (put-bytevector p #vu8(14 15 16 17 18) 2 1))
      (test (get) #vu8(10 11 12 13 18 16))
      (test (get) #vu8())
      (close-port p))
    (test (call-with-bytevector-output-port
           (lambda (p)
             (put-bytevector p #vu8(1 2 3))))
          #vu8(1 2 3))

    (test (call-with-bytevector-output-port
           (lambda (p)
             (put-string p "app\x3BB;e"))
           (make-transcoder (utf-8-codec)))
          #vu8(97 112 112 206 187 101))
    (let-values ([(p get) (open-string-output-port)])
      (test/unspec (put-string p "app\x3BB;e"))
      (test (get) "app\x3BB;e")
      (test (get) "")
      (close-port p))
    (test (call-with-string-output-port
           (lambda (p)
             (test/unspec (put-string p "app\x3BB;y"))))
          "app\x3BB;y")
|#

(test/f (output-port? (standard-input-port)))
(test/t (output-port? (standard-output-port)))
(test/t (output-port? (standard-error-port)))
(test/f (output-port? (current-input-port)))
(test/t (output-port? (current-output-port)))

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
  (put-bytevector p #vu8(3 7 9 11) 2 1)
  (flush-output-port p)
  (test* accum '(9 4 2))
  (close-port p)
  )

#|
(let* ([accum '()]
       [p (make-custom-textual-output-port
           "custom out"
           (lambda (str start count)
             (let ([str (substring str start count)])
               (set! accum (append
                            (reverse (string->list str))
                            accum))
               count))
           (lambda () (length accum))
           (lambda (pos) (set! accum (list-tail accum (- (length accum) pos))))
           (lambda () 'ok))])
  (test/t (port-has-port-position? p))
  (test/t (port-has-set-port-position!? p))
  (test* (port-position p) 0)
  ;(put-string p "ab")
  (test* (port-position p) 2)
  ;(put-string p "c")
  (flush-output-port p)
  (test* accum '(#\c #\b #\a))
  (test* (port-position p) 3)
  ;(set-port-position! p 2)
  (test* (port-position p) 2)
  (test* accum '(#\b #\a))
  ;(put-string p "xyzw" 2 1)
  (flush-output-port p)
  (test* accum '(#\z #\b #\a))
  (close-port p))
|#

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
