; http.ss
;
;   Copyright (c) 2010  Higepon(Taro Minowa)  <higepon@users.sourceforge.jp>
;
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
(library (http)
  (export http-get)
  (import (rnrs)
          (mosh)
          (mosh control)
          (mosh socket)
          )

;; This library is undocumented. APIs is subject to change without notice.

;; ToDo
;;   http-get->bytevector
;;   Now utf-8 only

(define (read-header port)
  (let loop ([line (get-line port)]
             [header* '()])
    (cond
     [(eof-object? line) (reverse header*)]
     [(zero? (string-length line))
      (reverse header*)]
     [else
      (loop (get-line port) (cons line header*))])))

(define (read-body port)
  (let loop ([line (get-line port)]
             [line* '()])
    (cond
     [(eof-object? line) (apply string-append (reverse line*))]
     [else
      (loop (get-line port) (cons line line*))])))

(define (http-get host port path)
  (let ([p
         (transcoded-port (socket-port (make-client-socket host port))
                          (make-transcoder (utf-8-codec)))])
    (display (format "GET ~a HTTP/1.1\r\nHost: ~a\r\nUser-Agent: Mosh Scheme (http)\r\n\r\n" path host) p)
    (read-header p)
    (let1 body (read-body p)
      (values body 200) ;; todo
      (close-port p)
      body)))

)
