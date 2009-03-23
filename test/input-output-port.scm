; input-output-port.scm - Tests for <input/output port>
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
        (rnrs mutable-strings)
        (mosh)
        (mosh process)
        (mosh shell)
        (mosh test))

;; N.B rm and cp should be written in pure scheme
(define (rm file-name)
  (when (file-exists? file-name)
    (delete-file file-name)))

(define (cp from to)
  (call-with-port (open-file-output-port to (file-options no-fail) (buffer-mode none))
    (lambda (out)
      (call-with-port (open-file-input-port from (file-options no-fail) (buffer-mode none))
        (lambda (in)
          (let ([bv (get-bytevector-all in)])
            (format #t "cp bv length = ~a\n" (bytevector-length bv))
            (put-bytevector out bv)
            (close-port in)
            (close-port out)))))))

;;(cp "./test/test.txt" "./test/test.txt.dat")

;; (define (cp from to)
;;   (let-values  ([(pid cin cout cerr) (spawn "cp" (list from to) (list #f #f #f))])
;;     (waitpid pid)
;;     #f))


(define-syntax test-positions
    (syntax-rules ()
      [(_ make)
       (begin
         (let* ([p (make "custom"
                     (lambda (? start count) 0)
                     (lambda () 0)
                     #f
                     (lambda () 'ok))])
           (test* (port-has-port-position? p) #t)
           (test* (port-has-set-port-position!? p) #f)
           (test* (port-position p) 0)
           (close-port p))
         (let* ([p (make "custom"
                     (lambda (? start count) 0)
                     #f
                     (lambda (pos) 'ok)
                     (lambda () 'ok))])
           (test* (port-has-port-position? p) #f)
           (test* (port-has-set-port-position!? p) #t)
           (set-port-position! p 0)
           (close-port p))
         (let* ([p (make "custom"
                     (lambda (? start count) 0)
                     #f
                     #f
                     (lambda () 'ok))])
           (test* (port-has-port-position? p) #f)
           (test* (port-has-set-port-position!? p) #f)
           (close-port p))))])


(define (with-all-buffer-mode file proc)
  (let ([tmp-file (format "~a.temp" file)])
    (for-each (lambda (mode)
                (cp file tmp-file)
                (proc mode tmp-file))
              (list (buffer-mode none) (buffer-mode block) (buffer-mode line)))))

(define (with-all-buffer-mode-simple proc)
    (for-each (lambda (mode)
                (proc mode))
              (list (buffer-mode none) (buffer-mode block) (buffer-mode line))))

;; binary-port
(with-all-buffer-mode "./test/test.txt"
 (lambda (mode file)
   (let ([port  (open-file-input/output-port file (file-options no-fail no-truncate) mode)])
     (define (write-and-back c)
       (put-u8 port c)
       (set-port-position! port (- (port-position port) 1)))
     (test/t (input-port? port))
     (test/t (port-has-set-port-position!? port))
     (test/t (port-has-port-position? port))
     (test* (get-u8 port) #x2f)
     (test* (port-position port) 1)
     ;; write!
     (put-u8 port #x2e)
     ;; write!
     (write-and-back #xfb)
     (test* (get-u8 port) #xfb)
     (test* (port-position port) 3)
     (set-port-position! port 8193) ;; over the buffer boundary
     (test* (port-position port) 8193)
     (test* (get-u8 port) 46)
     (set-port-position! port 8190)
     (test* (port-position port) 8190)
     (test* (get-u8 port) 32)
     (test* (lookahead-u8 port) 110)
     (test* (port-position port) 8191)
     (test* (get-u8 port) 110)
     (set-port-position! port 8190)
     ;; read over the boundary
     (test* (get-bytevector-n port 30) #vu8(32 110 50 46 116 111 70 108 111 110 117 109 40 41 41 59 10 32 32 32 32 125 32 101 108 115 101 32 123 10))
     (test* (port-position port) 8220)
     ;; read over the boundary and size > buffer-size
     (set-port-position! port 4000)
     (let ([bv1 (make-bytevector 10000)]
           [bv2 (get-bytevector-n port 10000)])
       (test* (bytevector-u8-ref bv2 0) 123)
       (test* (bytevector-u8-ref bv2 9999) 108)
       (set-port-position! port 4000)
       (test* (get-bytevector-n! port bv1 0 10000) 10000)
       ;; write!
       (put-u8 port #xfc)
       (test/t (equal? bv1 bv2)))
     ;; read-some
     (set-port-position! port 4000)
     (let ([bv (get-bytevector-some port)])
       (test/t (> (bytevector-length bv) 0))
       ;; yeah wrote data is here
       (test* (bytevector-u8-ref bv 0) 123))
     ;; read-all
     (set-port-position! port 4000)
     (let ([bv (get-bytevector-all port)])
       (test* (bytevector-length bv) 34861)
       (test* (bytevector-u8-ref bv 0) 123)
       (test* (bytevector-u8-ref bv 34860) 10))
     (test* (port-position port) 38861)
     (set-port-position! port 4000)
     (put-bytevector port (make-bytevector 9000 #x13))
     (close-port port))

   ;; check the written data
   (let ([port  (open-file-input/output-port file (file-options no-fail no-truncate) mode)])
     #f
     (test* (get-u8 port) #x2f)
     (test* (port-position port) 1)
     (test* (get-u8 port) #x2e)
     (test* (port-position port) 2)
     (test* (get-u8 port) #xfb)
     (set-port-position! port 14000)
     (test* (get-u8 port) #xfc)
     (set-port-position! port 4000)
     (let ([bv (get-bytevector-n port 9000)])
       (test/t (bytevector? bv))
       (test* (bytevector-length bv) 9000)
       (test/t (for-all (lambda (x) (= #x13 x)) (bytevector->u8-list bv)))
      ))
   ))

;; textual port
(with-all-buffer-mode "./test/utf16.txt"
 (lambda (mode file)
   (let ([port (open-file-input/output-port file (file-options no-fail no-truncate) mode (make-transcoder (utf-16-codec)))])
     (test/t (input-port? port))
     (test* (read port) "あいう")
     (test/t (port-eof? port))
     (close-port port))))

;; test utilitiy
(define (empty-file-exists? path)
  (let ([port (open-file-input-port path (file-options))])
    (let ([ret (eof-object? (get-u8 port))])
      (close-port port)
      ret)))

#|  file-options

    (file-options)
      If file exists:     raise &file-already-exists
      If does not exist:  create new file
|#

(test/exception i/o-file-already-exists-error?
                (open-file-input/output-port "./test/utf16.txt"))

(test/exception i/o-file-already-exists-error?
                (open-file-input/output-port "./test/utf16.txt" (file-options)))

(with-all-buffer-mode-simple
 (lambda (mode)
   (test/exception i/o-file-already-exists-error?
                   (open-file-input/output-port "./test/utf16.txt" (file-options) mode))))

(with-all-buffer-mode-simple
 (lambda (mode)
   (test/exception i/o-file-already-exists-error?
                   (open-file-input/output-port "./test/utf16.txt" (file-options) mode (make-transcoder (utf-16-codec))))))

(with-all-buffer-mode-simple
 (lambda (mode)
   (let ([port (open-file-input/output-port "./not-exists")])
     (close-port port)
     (test/t (empty-file-exists? "./not-exists"))
     (rm "./not-exists"))))

(with-all-buffer-mode-simple
 (lambda (mode)
   (let ([port (open-file-input/output-port "./not-exists" (file-options))])
     (close-port port)
     (test/t (empty-file-exists? "./not-exists"))
     (rm "./not-exists"))))

(with-all-buffer-mode-simple
 (lambda (mode)
   (let ([port (open-file-input/output-port "./not-exists" (file-options) mode)])
     (close-port port)
     (test/t (empty-file-exists? "./not-exists"))
     (rm "./not-exists"))))

(with-all-buffer-mode-simple
 (lambda (mode)
   (let ([port (open-file-input/output-port "./not-exists" (file-options) mode (make-transcoder (utf-16-codec)))])
     (close-port port)
     (test/t (empty-file-exists? "./not-exists"))
     (rm "./not-exists"))))


#|
    (file-options no-create)
      If file exists:     truncate
      If does not exist:  raise &file-does-not-exist
|#

(with-all-buffer-mode "test/utf16.txt"
 (lambda (mode file)
   (let ([port (open-file-input/output-port file (file-options no-create))])
     (close-port port)
     (test/t (empty-file-exists? file)))))

(with-all-buffer-mode "test/utf16.txt"
 (lambda (mode file)
   (let ([port (open-file-input/output-port file (file-options no-create) mode)])
     (close-port port)
     (test/t (empty-file-exists? file)))))

(with-all-buffer-mode "test/utf16.txt"
 (lambda (mode file)
   (let ([port (open-file-input/output-port file (file-options no-create) mode (make-transcoder (utf-16-codec)))])
     (close-port port)
     (test/t (empty-file-exists? file)))))

(with-all-buffer-mode-simple
 (lambda (mode)
   (test/exception i/o-file-does-not-exist-error?
                   (open-file-input/output-port "./not-exists" (file-options no-create)))))

(with-all-buffer-mode-simple
 (lambda (mode)
   (test/exception i/o-file-does-not-exist-error?
                   (open-file-input/output-port "./not-exists" (file-options no-create) mode))))

(with-all-buffer-mode-simple
 (lambda (mode)
   (test/exception i/o-file-does-not-exist-error?
                   (open-file-input/output-port "./not-exists" (file-options no-create) mode (make-transcoder (utf-16-codec))))))

#|
    (file-options no-fail)
      If file exists:     truncate
      If does not exist:  create new file
|#

(with-all-buffer-mode "test/utf16.txt"
 (lambda (mode file)
   (let ([port (open-file-input/output-port file (file-options no-fail))])
     (close-port port)
     (test/t (empty-file-exists? file)))))

(with-all-buffer-mode "test/utf16.txt"
 (lambda (mode file)
   (let ([port (open-file-input/output-port file (file-options no-fail) mode)])
     (close-port port)
     (test/t (empty-file-exists? file)))))

(with-all-buffer-mode "test/utf16.txt"
 (lambda (mode file)
   (let ([port (open-file-input/output-port file (file-options no-fail) mode (make-transcoder (utf-16-codec)))])
     (close-port port)
     (test/t (empty-file-exists? file)))))

(with-all-buffer-mode-simple
 (lambda (mode)
   (let ([port (open-file-input/output-port "./not-exists" (file-options no-fail))])
     (close-port port)
     (test/t (empty-file-exists? "./not-exists"))
     (rm "./not-exists"))))

(with-all-buffer-mode-simple
 (lambda (mode)
   (let ([port (open-file-input/output-port "./not-exists" (file-options no-fail) mode)])
     (close-port port)
     (test/t (empty-file-exists? "./not-exists"))
     (rm "./not-exists"))))

(with-all-buffer-mode-simple
 (lambda (mode)
   (let ([port (open-file-input/output-port "./not-exists" (file-options no-fail) mode (make-transcoder (utf-16-codec)))])
     (close-port port)
     (test/t (empty-file-exists? "./not-exists"))
     (rm "./not-exists"))))

#|
    (file-options no-truncate)
      If file exists:     raise &file-already-exists
      If does not exist:  create new file
|#

(test/exception i/o-file-already-exists-error?
                (open-file-input/output-port "./test/utf16.txt" (file-options no-truncate)))

(with-all-buffer-mode-simple
 (lambda (mode)
   (test/exception i/o-file-already-exists-error?
                   (open-file-input/output-port "./test/utf16.txt" (file-options no-truncate) mode))))

(with-all-buffer-mode-simple
 (lambda (mode)
   (test/exception i/o-file-already-exists-error?
                   (open-file-input/output-port "./test/utf16.txt" (file-options no-truncate) mode (make-transcoder (utf-16-codec))))))

(with-all-buffer-mode-simple
 (lambda (mode)
   (let ([port (open-file-input/output-port "./not-exists" (file-options no-truncate))])
     (close-port port)
     (test/t (empty-file-exists? "./not-exists"))
     (rm "./not-exists"))))

(with-all-buffer-mode-simple
 (lambda (mode)
   (let ([port (open-file-input/output-port "./not-exists" (file-options no-truncate) mode)])
     (close-port port)
     (test/t (empty-file-exists? "./not-exists"))
     (rm "./not-exists"))))

(with-all-buffer-mode-simple
 (lambda (mode)
   (let ([port (open-file-input/output-port "./not-exists" (file-options no-truncate) mode (make-transcoder (utf-16-codec)))])
     (close-port port)
     (test/t (empty-file-exists? "./not-exists"))
     (rm "./not-exists"))))

#|
    (file-options no-create no-fail)
      If file exists:     truncate
      If does not exist:  [N.B.] R6RS say nothing about this case, we choose raise &file-does-not-exist
|#
(with-all-buffer-mode "test/utf16.txt"
 (lambda (mode file)
   (let ([port (open-file-input/output-port file (file-options no-create no-fail))])
     (close-port port)
     (test/t (empty-file-exists? file)))))

(with-all-buffer-mode "test/utf16.txt"
 (lambda (mode file)
   (let ([port (open-file-input/output-port file (file-options no-create no-fail) mode)])
     (close-port port)
     (test/t (empty-file-exists? file)))))

(with-all-buffer-mode "test/utf16.txt"
 (lambda (mode file)
   (let ([port (open-file-input/output-port file (file-options no-create no-fail) mode (make-transcoder (utf-16-codec)))])
     (close-port port)
     (test/t (empty-file-exists? file)))))

(with-all-buffer-mode-simple
 (lambda (mode)
   (test/exception i/o-file-does-not-exist-error?
                   (open-file-input/output-port "./not-exists" (file-options no-create no-fail)))))

(with-all-buffer-mode-simple
 (lambda (mode)
   (test/exception i/o-file-does-not-exist-error?
                   (open-file-input/output-port "./not-exists" (file-options no-create no-fail) mode))))

(with-all-buffer-mode-simple
 (lambda (mode)
   (test/exception i/o-file-does-not-exist-error?
                   (open-file-input/output-port "./not-exists" (file-options no-create no-fail) mode (make-transcoder (utf-16-codec))))))

#|
    (file-options no-fail no-truncate)
      If file exists:     set port position to 0 (overwriting)
      If does not exist:  create new file
|#

(with-all-buffer-mode "test/utf16.txt"
 (lambda (mode file)
   (let ([port (open-file-input/output-port file (file-options no-fail no-truncate))])
     (close-port port)
     (test/f (empty-file-exists? file)))))

(with-all-buffer-mode "test/utf16.txt"
 (lambda (mode file)
   (let ([port (open-file-input/output-port file (file-options no-fail no-truncate) mode)])
     (close-port port)
     (test/f (empty-file-exists? file)))))

(with-all-buffer-mode "test/utf16.txt"
 (lambda (mode file)
   (let ([port (open-file-input/output-port file (file-options no-fail no-truncate) mode (make-transcoder (utf-16-codec)))])
     (close-port port)
     (test/f (empty-file-exists? file)))))

(with-all-buffer-mode-simple
 (lambda (mode)
   (let ([port (open-file-input/output-port "./not-exists" (file-options no-fail no-truncate))])
     (close-port port)
     (test/t (empty-file-exists? "./not-exists"))
     (rm "./not-exists"))))

(with-all-buffer-mode-simple
 (lambda (mode)
   (let ([port (open-file-input/output-port "./not-exists" (file-options no-fail no-truncate) mode)])
     (close-port port)
     (test/t (empty-file-exists? "./not-exists"))
     (rm "./not-exists"))))

(with-all-buffer-mode-simple
 (lambda (mode)
   (let ([port (open-file-input/output-port "./not-exists" (file-options no-fail no-truncate) mode (make-transcoder (utf-16-codec)))])
     (close-port port)
     (test/t (empty-file-exists? "./not-exists"))
     (rm "./not-exists"))))


#|
    (file-options no-create no-truncate)
      If file exists:     set port position to 0 (overwriting)
      If does not exist:  raise &file-does-not-exist
|#

(with-all-buffer-mode "test/utf16.txt"
 (lambda (mode file)
   (let ([port (open-file-input/output-port file (file-options no-create no-truncate))])
     (close-port port)
     (test/f (empty-file-exists? file)))))

(with-all-buffer-mode "test/utf16.txt"
 (lambda (mode file)
   (let ([port (open-file-input/output-port file (file-options no-create no-truncate) mode)])
     (close-port port)
     (test/f (empty-file-exists? file)))))

(with-all-buffer-mode "test/utf16.txt"
 (lambda (mode file)
   (let ([port (open-file-input/output-port file (file-options no-create no-truncate) mode (make-transcoder (utf-16-codec)))])
     (close-port port)
     (test/f (empty-file-exists? file)))))

(with-all-buffer-mode-simple
 (lambda (mode)
   (test/exception i/o-file-does-not-exist-error?
                   (open-file-input/output-port "./not-exists" (file-options no-create no-truncate)))))

(with-all-buffer-mode-simple
 (lambda (mode)
   (test/exception i/o-file-does-not-exist-error?
                   (open-file-input/output-port "./not-exists" (file-options no-create no-truncate) mode))))

(with-all-buffer-mode-simple
 (lambda (mode)
   (test/exception i/o-file-does-not-exist-error?
                   (open-file-input/output-port "./not-exists" (file-options no-create no-truncate) mode (make-transcoder (utf-16-codec))))))

#|
    (file-options no-create no-fail no-truncate)
      If file exists:     set port position to 0 (overwriting)
      If does not exist:  [N.B.] R6RS say nothing about this case, we choose raise &file-does-not-exist
|#

(with-all-buffer-mode "test/utf16.txt"
 (lambda (mode file)
   (let ([port (open-file-input/output-port file (file-options no-create no-truncate no-fail))])
     (close-port port)
     (test/f (empty-file-exists? file)))))

(with-all-buffer-mode "test/utf16.txt"
 (lambda (mode file)
   (let ([port (open-file-input/output-port file (file-options no-create no-truncate no-fail) mode)])
     (close-port port)
     (test/f (empty-file-exists? file)))))

(with-all-buffer-mode "test/utf16.txt"
 (lambda (mode file)
   (let ([port (open-file-input/output-port file (file-options no-create no-truncate no-fail) mode (make-transcoder (utf-16-codec)))])
     (close-port port)
     (test/f (empty-file-exists? file)))))

(with-all-buffer-mode-simple
 (lambda (mode)
   (test/exception i/o-file-does-not-exist-error?
                   (open-file-input/output-port "./not-exists" (file-options no-create no-fail no-truncate)))))

(with-all-buffer-mode-simple
 (lambda (mode)
   (test/exception i/o-file-does-not-exist-error?
                   (open-file-input/output-port "./not-exists" (file-options no-create no-fail no-truncate) mode))))

(with-all-buffer-mode-simple
 (lambda (mode)
   (test/exception i/o-file-does-not-exist-error?
                   (open-file-input/output-port "./not-exists" (file-options no-create no-fail no-truncate) mode (make-transcoder (utf-16-codec))))))

;; custom
(let* ([save #f]
           [p (make-custom-binary-input/output-port
               "custom in"
               (lambda (bv start end)
                 (bytevector-u8-set! bv start 7)
                 1)
               (lambda (bv start end)
                 (set! save (bytevector-u8-ref bv start))
                 1)
               #f #f #f)])
      (put-u8 p 10)
      (flush-output-port p)
      (test* save 10)
      (test* (get-u8 p) 7)
      (close-port p))
(test-positions make-custom-binary-input-port)

(let* ([save #f]
           [p (make-custom-textual-input/output-port
               "custom in"
               (lambda (str start end)
                 (string-set! str start #\!)
                 1)
               (lambda (str start end)
                 (set! save (string-ref str start))
                 1)
               #f #f #f)])
      (put-char p #\q)
      (flush-output-port p)
      (test* save #\q)
      (test* (get-char p) #\!)
      (close-port p))

(test-end)
