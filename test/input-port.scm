; input-port.scm - Tests for <input port>
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

(define (with-all-buffer-mode proc)
  (for-each proc (list (buffer-mode none) (buffer-mode block) (buffer-mode line))))

;; open-file-input-port with transcoder
(with-all-buffer-mode
 (lambda (mode)
   (let ([port (open-file-input-port "./test/utf16.txt" (file-options) mode (make-transcoder (utf-16-codec)))])
     (test/t (input-port? port))
     (test* (read port) "あいう")
     (test/f (port-eof? port)) ;; #f for textual port
     (close-port port))))

;; open-bytevector-input-port
(let ([port (open-bytevector-input-port (u8-list->bytevector (map char->integer (string->list "abc")))
                                        (make-transcoder (utf-8-codec)))])
  (test* (read-char port) #\a)
  (test* (read-char port) #\b)
  (test* (read-char port) #\c)
  (test/t (eof-object? (read-char port))))

(let ([port (open-bytevector-input-port (u8-list->bytevector (map char->integer (string->list "abc"))))])
  (test* (get-u8 port) (char->integer #\a))
  (test* (get-u8 port) (char->integer #\b))
  (test* (get-u8 port) (char->integer #\c))
  (test/t (eof-object? (get-u8 port))))

;; custom input-port
(let* ([pos 0]
       [p (make-custom-binary-input-port
           "xyz"
           (lambda (bv start count)
             (if (= pos 16)
                 0
                 (begin
                   (set! pos (+ 1 pos))
                   (bytevector-u8-set! bv start pos)
                   1)))
           (lambda () pos)
           (lambda (p) (set! pos p))
           (lambda () 'ok))])
  (test/t (port-has-port-position? p))
  (test/t (port-has-set-port-position!? p))
  (test* (port-position p) 0)
  (test* (get-bytevector-n p 3) #vu8(1 2 3))
  (test* (port-position p) 3)
  (test* (lookahead-u8 p) 4)
  (test* (lookahead-u8 p) 4)
  (test* (port-position p) 3)
  (set-port-position! p 10)
  (get-bytevector-n p 2)
  (test* (get-bytevector-n p 2) #vu8(13 14))
  (test* (get-bytevector-n p 2) #vu8(15 16))
  (test* (get-bytevector-n p 2) (eof-object))
  (set-port-position! p 2)
  (test* (get-bytevector-n p 3) #vu8(3 4 5))
  (test* (format "~a" p) "<custom input port xyz>")
  (set-port-position! p 2)
  ;; some
  (let ([bv (get-bytevector-some p)])
    (test/t (> (bytevector-length bv) 0))
    (test* (bytevector-u8-ref bv 0) 3))
  ;; all
  (set-port-position! p 0)
  (let ([bv (get-bytevector-all p)])
    (test* (bytevector-length bv) 16))
  (test* (port-position p) 16)
  (close-port p))

;; standard-input-port doesn't suport port-position on Mosh.
(test/f (port-has-port-position? (standard-input-port)))
(test/f (port-has-set-port-position!? (standard-input-port)))
(test/violation? (set-port-position! (standard-input-port) 0))
(test/violation? (port-position (standard-input-port)))

;; string-input-port should support port position
(let ([in (open-string-input-port "0123456")])
  (test/t (port-has-port-position? in))
  (test/t (port-has-set-port-position!? in))
  (test* (port-position in) 0)
  (test* (read-char in) #\0)
  (test* (port-position in) 1)
  (set-port-position! in 5)
  (test* (read-char in) #\5)
  (test* (format "~a" in) "<string input port>")
  (set-port-position! in 0)
  (test* (get-string-n in 3) "012")
  (let ([s (make-string 3 #\space)])
    (set-port-position! in 1)
    (test* (get-string-n! in s 1 2) 2)
    (test* s " 12"))
  (set-port-position! in 0)
  (test* (get-string-all in) "0123456")
  (close-port in))

;; get-datum with error
(test/exception lexical-violation? (get-datum (open-string-input-port "(")))
(test/exception i/o-read-error? (get-datum (open-string-input-port "(")))

;; textual-input-port doesn't suport port-position on Mosh.
(test/f (port-has-set-port-position!? (current-input-port)))
(test/f (port-has-port-position? (current-input-port)))
(test/violation? (set-port-position! (current-input-port) 0))
(test/violation? (port-position (current-input-port)))

;; file-binary-input-port
(with-all-buffer-mode
 (lambda (mode)
   (let ([port  (open-file-input-port "./test/test.txt" (file-options) mode)])
     (test/t (input-port? port))
     (test/t (port-has-set-port-position!? port))
     (test/t (port-has-port-position? port))
     (test* (get-u8 port) #x2f)
     (test* (port-position port) 1)
     (test* (get-u8 port) #x2f)
     (test* (port-position port) 2)
     (test* (get-u8 port) #x20)
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
       (test/t (equal? bv1 bv2)))
     ;; read-some
     (set-port-position! port 4000)
     (let ([bv (get-bytevector-some port)])
       (test/t (> (bytevector-length bv) 0))
       (test* (bytevector-u8-ref bv 0) 123))
     ;; read-all
     (set-port-position! port 4000)
     (let ([bv (get-bytevector-all port)])
       (test* (bytevector-length bv) 34861)
       (test* (bytevector-u8-ref bv 0) 123)
       (test* (bytevector-u8-ref bv 34860) 10))
     (test* (port-position port) 38861)
     (close-port port))))

(test-end)
