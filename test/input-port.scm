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
        (rnrs mutable-strings)
        (mosh)
        (mosh test))

(test-begin "port")

(when (>= (length (command-line)) 2)
  (test-skip (test-not-match-name (cadr (command-line)))))

(define (with-all-buffer-mode proc)
  (for-each proc (list (buffer-mode none) (buffer-mode block) (buffer-mode line))))

(test-begin "simple")

;; open-file-input-port with transcoder
(with-all-buffer-mode
 (lambda (mode)
   (let ([port (open-file-input-port "./test/utf16.txt" (file-options) mode (make-transcoder (utf-16-codec)))])
     (test-true (input-port? port))
     (test-equal (read port) "あいう")
     (test-equal (read-char port) #\newline)
     (test-true (port-eof? port))
     (close-port port))))

(test-end)

;; open-bytevector-input-port
(let ([port (open-bytevector-input-port (u8-list->bytevector (map char->integer (string->list "abc")))
                                        (make-transcoder (utf-8-codec)))])
  (test-equal (read-char port) #\a)
  (test-equal (read-char port) #\b)
  (test-equal (read-char port) #\c)
  (test-true (eof-object? (read-char port))))

(let ([port (open-bytevector-input-port (u8-list->bytevector (map char->integer (string->list "abc"))))])
  (test-equal (get-u8 port) (char->integer #\a))
  (test-equal (get-u8 port) (char->integer #\b))
  (test-equal (get-u8 port) (char->integer #\c))
  (test-true (eof-object? (get-u8 port))))

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
  (test-true (port-has-port-position? p))
  (test-true (port-has-set-port-position!? p))
  (test-equal (port-position p) 0)
  (test-equal (get-bytevector-n p 3) #vu8(1 2 3))
  (test-equal (port-position p) 3)
  (test-equal (lookahead-u8 p) 4)
  (test-equal (lookahead-u8 p) 4)
  (test-equal (port-position p) 3)
  (set-port-position! p 10)
  (get-bytevector-n p 2)
  (test-equal (get-bytevector-n p 2) #vu8(13 14))
  (test-equal (get-bytevector-n p 2) #vu8(15 16))
  (test-equal (get-bytevector-n p 2) (eof-object))
  (set-port-position! p 2)
  (test-equal (get-bytevector-n p 3) #vu8(3 4 5))
  (test-equal (format "~a" p) "<custom-input-port xyz>")
  (set-port-position! p 2)
  ;; some
  (let ([bv (get-bytevector-some p)])
    (test-true (> (bytevector-length bv) 0))
    (test-equal (bytevector-u8-ref bv 0) 3))
  ;; all
  (set-port-position! p 0)
  (let ([bv (get-bytevector-all p)])
    (test-equal (bytevector-length bv) 16))
  (test-equal (port-position p) 16)
  (close-port p))

;; standard-input-port doesn't suport port-position on Mosh.
(test-false (port-has-port-position? (standard-input-port)))
(test-false (port-has-set-port-position!? (standard-input-port)))
(test-error (set-port-position! (standard-input-port) 0))
(test-error (port-position (standard-input-port)))

;; string-input-port should support port position
(let ([in (open-string-input-port "0123456")])
  (test-true (port-has-port-position? in))
  (test-true (port-has-set-port-position!? in))
  (test-equal (port-position in) 0)
  (test-equal (read-char in) #\0)
  (test-equal (port-position in) 1)
  (set-port-position! in 5)
  (test-equal (read-char in) #\5)
  (test-equal (format "~a" in) "<string-input-port>")
  (set-port-position! in 0)
  (test-equal (get-string-n in 3) "012")
  (let ([s (make-string 3 #\space)])
    (set-port-position! in 1)
    (test-equal (get-string-n! in s 1 2) 2)
    (test-equal s " 12"))
  (set-port-position! in 0)
  (test-equal (get-string-all in) "0123456")
  (close-input-port in))

(let ([in (open-string-input-port "012\n34\n567\n")])
  (test-equal (get-line in) "012")
  (test-equal (peek-char in) #\3)
  (test-equal (get-line in) "34")
  (test-equal (get-line in) "567")
  (test-true (eof-object? (get-line in)))
  (close-port in))

;; get-datum with error
(test-error lexical-violation? (get-datum (open-string-input-port "(")))
(test-error i/o-read-error? (get-datum (open-string-input-port "(")))

;; textual-input-port doesn't suport port-position on Mosh.
(test-false (port-has-set-port-position!? (current-input-port)))
(test-false (port-has-port-position? (current-input-port)))
(test-error (set-port-position! (current-input-port) 0))
(test-error (port-position (current-input-port)))

;; file-binary-input-port
(with-all-buffer-mode
 (lambda (mode)
   (let ([port  (open-file-input-port "./test/test.txt" (file-options) mode)])
     (test-true (input-port? port))
     (test-true (port-has-set-port-position!? port))
     (test-true (port-has-port-position? port))
     (test-equal (get-u8 port) #x2f)
     (test-equal (port-position port) 1)
     (test-equal (get-u8 port) #x2f)
     (test-equal (port-position port) 2)
     (test-equal (get-u8 port) #x20)
     (test-equal (port-position port) 3)
     (set-port-position! port 8193) ;; over the buffer boundary
     (test-equal (port-position port) 8193)
     (test-equal (get-u8 port) 46)
     (set-port-position! port 8190)
     (test-equal (port-position port) 8190)
     (test-equal (get-u8 port) 32)
     (test-equal (lookahead-u8 port) 110)
     (test-equal (port-position port) 8191)
     (test-equal (get-u8 port) 110)
     (set-port-position! port 8190)
     ;; read over the boundary
     (test-equal (get-bytevector-n port 30) #vu8(32 110 50 46 116 111 70 108 111 110 117 109 40 41 41 59 10 32 32 32 32 125 32 101 108 115 101 32 123 10))
     (test-equal (port-position port) 8220)
     ;; read over the boundary and size > buffer-size
     (set-port-position! port 4000)
     (let ([bv1 (make-bytevector 10000)]
           [bv2 (get-bytevector-n port 10000)])
       (test-equal (bytevector-u8-ref bv2 0) 123)
       (test-equal (bytevector-u8-ref bv2 9999) 108)
       (set-port-position! port 4000)
       (test-equal (get-bytevector-n! port bv1 0 10000) 10000)
       (test-true (equal? bv1 bv2)))
     ;; read-some
     (set-port-position! port 4000)
     (let ([bv (get-bytevector-some port)])
       (test-true (> (bytevector-length bv) 0))
       (test-equal (bytevector-u8-ref bv 0) 123))
     ;; read-all
     (set-port-position! port 4000)
     (let ([bv (get-bytevector-all port)])
       (test-equal (bytevector-length bv) 34861)
       (test-equal (bytevector-u8-ref bv 0) 123)
       (test-equal (bytevector-u8-ref bv 34860) 10))
     (test-equal (port-position port) 38861)
     (close-port port))))

;; current-input-port
(test-true (input-port? (current-input-port)))

;; call-with-port
(call-with-port (open-string-input-port "012\n34\n567\n")
  (lambda (p)
    (test-equal (get-line p) "012")))

(let* ([pos 0]
           [p (make-custom-textual-input-port
               "custom in"
               (lambda (bv start count)
                 (if (= pos 16)
                     0
                     (begin
                       (set! pos (+ 1 pos))
                       (string-set! bv start (integer->char (+ 96 pos)))
                       1)))
               (lambda () pos)
               (lambda (p) (set! pos p))
               (lambda () 'ok))])
      (port-position p)
      (test-equal (get-string-n p 3) "abc")
      (test-equal (lookahead-char p) #\d)
      (test-equal (lookahead-char p) #\d)
      (test-equal (get-string-n p 7) "defghij")
      (get-string-n p 2)
      (test-equal (get-string-n p 2) "mn")
      (test-equal (get-string-n p 2) "op")
      (test-equal (get-string-n p 2) (eof-object))
      (close-port p))

(test-begin "re2c greeding bug check")

(let ([p (open-string-input-port
           "ab cd ef gh ij kl mn op qr st uv wx yz\n")])
  (test-equal 'ab (get-datum p))
  (test-eqv #\space  (get-char p))
  (test-equal 'cd (get-datum p))
  (test-eqv #\space (get-char p))
  (close-port p))

(let ([p (open-string-input-port
           "\"abcdefghijklmnopqrstuvwxyz\"")])
  (test-equal "string length greater than 18 (fill buffer)" "abcdefghijklmnopqrstuvwxyz" (get-datum p))
  (close-port p))

(test-end)

(test-end)
