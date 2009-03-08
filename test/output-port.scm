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
;  $Id$


(import (rnrs)
        (mosh)
        (mosh test)
        (mosh shell)
        (mosh process))

;; test utilitiy
(def-command rm)
(define (cp from to)
  (let-values  ([(pid cin cout cerr) (spawn "cp" (list from to) (list #f #f #f))])
    (waitpid pid)
    #f))
(cp "./test/test.txt" "./test/test.txt.temp")

(define (with-all-buffer-mode file proc)
  (let ([tmp-file (format "~a.temp" file)])
    (for-each (lambda (mode)
                (cp file tmp-file)
                (proc mode tmp-file))
              (list (buffer-mode none) (buffer-mode block) (buffer-mode line)))))

(define (empty-file-exists? path)
  (let ([port (open-file-input-port path (file-options))])
    (let ([ret (eof-object? (get-u8 port))])
      (close-port port)
      ret)))

(define (with-all-buffer-mode-simple proc)
    (for-each (lambda (mode)
                (proc mode))
              (list (buffer-mode none) (buffer-mode block) (buffer-mode line))))

#|
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

(test* (output-port-buffer-mode (standard-output-port)) 'line)
(test* (output-port-buffer-mode (standard-error-port)) 'none)

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
  (put-string p "ab")
  (test* (port-position p) 2)
  (put-string p "c")
  (flush-output-port p)
  (test* accum '(#\c #\b #\a))
  (test* (port-position p) 3)
  (set-port-position! p 2)
  (test* (port-position p) 2)
  (test* accum '(#\b #\a))
  (put-string p "xyzw" 2 1)
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
  ;(display "cd" port)
  (put-string port "cd")
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

(let ([port (open-file-output-port "./test/test.txt.temp" (file-options no-fail))])
      (set-port-position! port 4000)
          (put-bytevector port (make-bytevector 9000 #x13))
              (close-port port))

(let ([port (open-file-input-port "./test/test.txt.temp" (file-options no-fail))])
      (set-port-position! port 4000)
          (let ([bv (get-bytevector-n port 9000)])
                  (display bv)
                        (test/t (for-all (lambda (x) (= #x13 x)) (bytevector->u8-list bv)))))

#|  file-options

    (file-options)
      If file exists:     raise &file-already-exists
      If does not exist:  create new file
|#

(test/exception i/o-file-already-exists-error?
                (open-file-output-port "./test/utf16.txt"))

(test/exception i/o-file-already-exists-error?
                (open-file-output-port "./test/utf16.txt" (file-options)))

(with-all-buffer-mode-simple
 (lambda (mode)
   (test/exception i/o-file-already-exists-error?
                   (open-file-output-port "./test/utf16.txt" (file-options) mode))))

(with-all-buffer-mode-simple
 (lambda (mode)
   (test/exception i/o-file-already-exists-error?
                   (open-file-output-port "./test/utf16.txt" (file-options) mode (make-transcoder (utf-16-codec))))))

(with-all-buffer-mode-simple
 (lambda (mode)
   (let ([port (open-file-output-port "./not-exists")])
     (close-port port)
     (test/t (empty-file-exists? "./not-exists"))
     (rm "-f" "./not-exists"))))

(with-all-buffer-mode-simple
 (lambda (mode)
   (let ([port (open-file-output-port "./not-exists" (file-options))])
     (close-port port)
     (test/t (empty-file-exists? "./not-exists"))
     (rm "-f" "./not-exists"))))

(with-all-buffer-mode-simple
 (lambda (mode)
   (let ([port (open-file-output-port "./not-exists" (file-options) mode)])
     (close-port port)
     (test/t (empty-file-exists? "./not-exists"))
     (rm "-f" "./not-exists"))))

(with-all-buffer-mode-simple
 (lambda (mode)
   (let ([port (open-file-output-port "./not-exists" (file-options) mode (make-transcoder (utf-16-codec)))])
     (close-port port)
     (test/t (empty-file-exists? "./not-exists"))
     (rm "-f" "./not-exists"))))


#|
    (file-options no-create)
      If file exists:     truncate
      If does not exist:  raise &file-does-not-exist
|#

(with-all-buffer-mode "test/utf16.txt"
 (lambda (mode file)
   (let ([port (open-file-output-port file (file-options no-create))])
     (close-port port)
     (test/t (empty-file-exists? file)))))

(with-all-buffer-mode "test/utf16.txt"
 (lambda (mode file)
   (let ([port (open-file-output-port file (file-options no-create) mode)])
     (close-port port)
     (test/t (empty-file-exists? file)))))

(with-all-buffer-mode "test/utf16.txt"
 (lambda (mode file)
   (let ([port (open-file-output-port file (file-options no-create) mode (make-transcoder (utf-16-codec)))])
     (close-port port)
     (test/t (empty-file-exists? file)))))

(with-all-buffer-mode-simple
 (lambda (mode)
   (test/exception i/o-file-does-not-exist-error?
                   (open-file-output-port "./not-exists" (file-options no-create)))))

(with-all-buffer-mode-simple
 (lambda (mode)
   (test/exception i/o-file-does-not-exist-error?
                   (open-file-output-port "./not-exists" (file-options no-create) mode))))

(with-all-buffer-mode-simple
 (lambda (mode)
   (test/exception i/o-file-does-not-exist-error?
                   (open-file-output-port "./not-exists" (file-options no-create) mode (make-transcoder (utf-16-codec))))))

#|
    (file-options no-fail)
      If file exists:     truncate
      If does not exist:  create new file
|#

(with-all-buffer-mode "test/utf16.txt"
 (lambda (mode file)
   (let ([port (open-file-output-port file (file-options no-fail))])
     (close-port port)
     (test/t (empty-file-exists? file)))))

(with-all-buffer-mode "test/utf16.txt"
 (lambda (mode file)
   (let ([port (open-file-output-port file (file-options no-fail) mode)])
     (close-port port)
     (test/t (empty-file-exists? file)))))

(with-all-buffer-mode "test/utf16.txt"
 (lambda (mode file)
   (let ([port (open-file-output-port file (file-options no-fail) mode (make-transcoder (utf-16-codec)))])
     (close-port port)
     (test/t (empty-file-exists? file)))))

(with-all-buffer-mode-simple
 (lambda (mode)
   (let ([port (open-file-output-port "./not-exists" (file-options no-fail))])
     (close-port port)
     (test/t (empty-file-exists? "./not-exists"))
     (rm "-f" "./not-exists"))))

(with-all-buffer-mode-simple
 (lambda (mode)
   (let ([port (open-file-output-port "./not-exists" (file-options no-fail) mode)])
     (close-port port)
     (test/t (empty-file-exists? "./not-exists"))
     (rm "-f" "./not-exists"))))

(with-all-buffer-mode-simple
 (lambda (mode)
   (let ([port (open-file-output-port "./not-exists" (file-options no-fail) mode (make-transcoder (utf-16-codec)))])
     (close-port port)
     (test/t (empty-file-exists? "./not-exists"))
     (rm "-f" "./not-exists"))))

#|
    (file-options no-truncate)
      If file exists:     raise &file-already-exists
      If does not exist:  create new file
|#

(test/exception i/o-file-already-exists-error?
                (open-file-output-port "./test/utf16.txt" (file-options no-truncate)))

(with-all-buffer-mode-simple
 (lambda (mode)
   (test/exception i/o-file-already-exists-error?
                   (open-file-output-port "./test/utf16.txt" (file-options no-truncate) mode))))

(with-all-buffer-mode-simple
 (lambda (mode)
   (test/exception i/o-file-already-exists-error?
                   (open-file-output-port "./test/utf16.txt" (file-options no-truncate) mode (make-transcoder (utf-16-codec))))))

(with-all-buffer-mode-simple
 (lambda (mode)
   (let ([port (open-file-output-port "./not-exists" (file-options no-truncate))])
     (close-port port)
     (test/t (empty-file-exists? "./not-exists"))
     (rm "-f" "./not-exists"))))

(with-all-buffer-mode-simple
 (lambda (mode)
   (let ([port (open-file-output-port "./not-exists" (file-options no-truncate) mode)])
     (close-port port)
     (test/t (empty-file-exists? "./not-exists"))
     (rm "-f" "./not-exists"))))

(with-all-buffer-mode-simple
 (lambda (mode)
   (let ([port (open-file-output-port "./not-exists" (file-options no-truncate) mode (make-transcoder (utf-16-codec)))])
     (close-port port)
     (test/t (empty-file-exists? "./not-exists"))
     (rm "-f" "./not-exists"))))

#|
    (file-options no-create no-fail)
      If file exists:     truncate
      If does not exist:  [N.B.] R6RS say nothing about this case, we choose raise &file-does-not-exist
|#
(with-all-buffer-mode "test/utf16.txt"
 (lambda (mode file)
   (let ([port (open-file-output-port file (file-options no-create no-fail))])
     (close-port port)
     (test/t (empty-file-exists? file)))))

(with-all-buffer-mode "test/utf16.txt"
 (lambda (mode file)
   (let ([port (open-file-output-port file (file-options no-create no-fail) mode)])
     (close-port port)
     (test/t (empty-file-exists? file)))))

(with-all-buffer-mode "test/utf16.txt"
 (lambda (mode file)
   (let ([port (open-file-output-port file (file-options no-create no-fail) mode (make-transcoder (utf-16-codec)))])
     (close-port port)
     (test/t (empty-file-exists? file)))))

(with-all-buffer-mode-simple
 (lambda (mode)
   (test/exception i/o-file-does-not-exist-error?
                   (open-file-output-port "./not-exists" (file-options no-create no-fail)))))

(with-all-buffer-mode-simple
 (lambda (mode)
   (test/exception i/o-file-does-not-exist-error?
                   (open-file-output-port "./not-exists" (file-options no-create no-fail) mode))))

(with-all-buffer-mode-simple
 (lambda (mode)
   (test/exception i/o-file-does-not-exist-error?
                   (open-file-output-port "./not-exists" (file-options no-create no-fail) mode (make-transcoder (utf-16-codec))))))

#|
    (file-options no-fail no-truncate)
      If file exists:     set port position to 0 (overwriting)
      If does not exist:  create new file
|#

(with-all-buffer-mode "test/utf16.txt"
 (lambda (mode file)
   (let ([port (open-file-output-port file (file-options no-fail no-truncate))])
     (close-port port)
     (test/f (empty-file-exists? file)))))

(with-all-buffer-mode "test/utf16.txt"
 (lambda (mode file)
   (let ([port (open-file-output-port file (file-options no-fail no-truncate) mode)])
     (close-port port)
     (test/f (empty-file-exists? file)))))

(with-all-buffer-mode "test/utf16.txt"
 (lambda (mode file)
   (let ([port (open-file-output-port file (file-options no-fail no-truncate) mode (make-transcoder (utf-16-codec)))])
     (close-port port)
     (test/f (empty-file-exists? file)))))

(with-all-buffer-mode-simple
 (lambda (mode)
   (let ([port (open-file-output-port "./not-exists" (file-options no-fail no-truncate))])
     (close-port port)
     (test/t (empty-file-exists? "./not-exists"))
     (rm "-f" "./not-exists"))))

(with-all-buffer-mode-simple
 (lambda (mode)
   (let ([port (open-file-output-port "./not-exists" (file-options no-fail no-truncate) mode)])
     (close-port port)
     (test/t (empty-file-exists? "./not-exists"))
     (rm "-f" "./not-exists"))))

(with-all-buffer-mode-simple
 (lambda (mode)
   (let ([port (open-file-output-port "./not-exists" (file-options no-fail no-truncate) mode (make-transcoder (utf-16-codec)))])
     (close-port port)
     (test/t (empty-file-exists? "./not-exists"))
     (rm "-f" "./not-exists"))))


#|
    (file-options no-create no-truncate)
      If file exists:     set port position to 0 (overwriting)
      If does not exist:  raise &file-does-not-exist
|#

(with-all-buffer-mode "test/utf16.txt"
 (lambda (mode file)
   (let ([port (open-file-output-port file (file-options no-create no-truncate))])
     (close-port port)
     (test/f (empty-file-exists? file)))))

(with-all-buffer-mode "test/utf16.txt"
 (lambda (mode file)
   (let ([port (open-file-output-port file (file-options no-create no-truncate) mode)])
     (close-port port)
     (test/f (empty-file-exists? file)))))

(with-all-buffer-mode "test/utf16.txt"
 (lambda (mode file)
   (let ([port (open-file-output-port file (file-options no-create no-truncate) mode (make-transcoder (utf-16-codec)))])
     (close-port port)
     (test/f (empty-file-exists? file)))))

(with-all-buffer-mode-simple
 (lambda (mode)
   (test/exception i/o-file-does-not-exist-error?
                   (open-file-output-port "./not-exists" (file-options no-create no-truncate)))))

(with-all-buffer-mode-simple
 (lambda (mode)
   (test/exception i/o-file-does-not-exist-error?
                   (open-file-output-port "./not-exists" (file-options no-create no-truncate) mode))))

(with-all-buffer-mode-simple
 (lambda (mode)
   (test/exception i/o-file-does-not-exist-error?
                   (open-file-output-port "./not-exists" (file-options no-create no-truncate) mode (make-transcoder (utf-16-codec))))))

#|
    (file-options no-create no-fail no-truncate)
      If file exists:     set port position to 0 (overwriting)
      If does not exist:  [N.B.] R6RS say nothing about this case, we choose raise &file-does-not-exist
|#

(with-all-buffer-mode "test/utf16.txt"
 (lambda (mode file)
   (let ([port (open-file-output-port file (file-options no-create no-truncate no-fail))])
     (close-port port)
     (test/f (empty-file-exists? file)))))

(with-all-buffer-mode "test/utf16.txt"
 (lambda (mode file)
   (let ([port (open-file-output-port file (file-options no-create no-truncate no-fail) mode)])
     (close-port port)
     (test/f (empty-file-exists? file)))))

(with-all-buffer-mode "test/utf16.txt"
 (lambda (mode file)
   (let ([port (open-file-output-port file (file-options no-create no-truncate no-fail) mode (make-transcoder (utf-16-codec)))])
     (close-port port)
     (test/f (empty-file-exists? file)))))

(with-all-buffer-mode-simple
 (lambda (mode)
   (test/exception i/o-file-does-not-exist-error?
                   (open-file-output-port "./not-exists" (file-options no-create no-fail no-truncate)))))

(with-all-buffer-mode-simple
 (lambda (mode)
   (test/exception i/o-file-does-not-exist-error?
                   (open-file-output-port "./not-exists" (file-options no-create no-fail no-truncate) mode))))

(with-all-buffer-mode-simple
 (lambda (mode)
   (test/exception i/o-file-does-not-exist-error?
                   (open-file-output-port "./not-exists" (file-options no-create no-fail no-truncate) mode (make-transcoder (utf-16-codec))))))

;; ----------------------------------------
;; Check buffer modes? Just make sure they're accepted:

(let ([p (open-file-output-port "./test/test.txt.temp" (file-options no-create) 'line)])
  (test* (output-port-buffer-mode p) 'line)
  (close-port p))
(let ([p (open-file-output-port "./test/test.txt.temp" (file-options no-create) 'block)])
  (test* (output-port-buffer-mode p) 'block)
  (close-port p))
(let ([p (open-file-output-port "./test/test.txt.temp" (file-options no-create) 'none)])
  (test* (output-port-buffer-mode p) 'none)
  (close-port p))

(test-end)
