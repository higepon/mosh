; memcached.ss
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
;  $Id: concurrent.ss 621 2008-11-09 06:22:47Z higepon $

;; Memcached client API.
;; This library is undocumented, APIs is subject to change without notice.
(library (memcached)
         (export
          memcached-connect
          memcached-set!
          memcached-get
          memcached-gets)
         (import (rnrs)
                 (mosh)
                 (only (srfi :1) alist-cons)
                 (only (srfi :13) string-join)
                 (mosh socket))

(define-record-type memcached-client
  (fields
   (immutable socket)))

(define (memcached-connect server port)
  (make-memcached-client (make-client-socket server port)))

(define (memcached-recv conn)
  (let ([buffer-size 4096]
        [socket (memcached-client-socket conn)])
      (let loop ([ret (make-bytevector 0)]
                 [data (socket-recv socket buffer-size)])
        (let* ([total-size (+ (bytevector-length ret) (bytevector-length data))]
               [new (make-bytevector total-size)])
          (bytevector-copy! ret 0 new 0 (bytevector-length ret))
          (bytevector-copy! data 0 new (bytevector-length ret) (bytevector-length data))
          (if (= (bytevector-length data) buffer-size)
              (loop new (socket-recv socket buffer-size))
              new)))))

(define (memcached-send conn text)
  (memcached-send-bv conn (string->utf8 text)))

(define (memcached-send-bv conn bv)
  (socket-send (memcached-client-socket conn) bv))

(define (memcached-set! conn key flags expiry bv-value)
    (memcached-send conn (format "set ~a 0 0 ~d\r\n" key (bytevector-length bv-value)))
    (memcached-send-bv conn bv-value)
    (memcached-send conn "\r\n")
    (memcached-recv conn))

;; todo: return value should be bytevector
(define (memcached-gets conn . keys)
  (memcached-send conn (format "get ~a\r\n" (string-join keys " ")))
  (let ([response (memcached-recv conn)])
    (display (utf8->string response))
    (let loop ([lines (string-split (utf8->string response) #\newline)]
               [ret '()])
      (cond
       [(#/^VALUE ([^\s]+) [^\s]+ \d+$/ (car lines)) =>
        (lambda (m)
          (loop (cddr lines) (alist-cons (m 1) (cadr lines) ret)))]
       [(#/END/ (car lines))
        (reverse ret)]
       [else
        (error 'memcach-get/s "malformed gets replry" (car lines))]))))

(define (memcached-get conn key)
  (let ([ret (assoc key (memcached-gets conn key))])
    (if ret
        (cdr ret)
        #f)))
)
