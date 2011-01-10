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
          (irregex)
          (match)
          (srfi :8)
          (mosh socket)
          )

;; This library is undocumented. APIs is subject to change without notice.

;; ToDo
;;   http-get->bytevector
;;   Now utf-8 only

(define (get-content-length header*)
  (let loop ([header* header*])
    (cond
     [(null? header*) #f]
     [(irregex-search "^Content-Length: ([0-9]+)" (car header*)) =>
      (lambda (m) (string->number (irregex-match-substring m 1)))]
     [else
      (loop (cdr header*))])))

(define (read-header p)
  (let loop ([c1 (get-u8 p)]
             [c2 (get-u8 p)]
             [header '()]
             [header* '()])
    (cond
     [(and (null? header) (and (= c1 #x0d) (= c2 #x0a)))
      header*]
     [(and (= c1 #x0d) (= c2 #x0a))
      (loop (get-u8 p) (get-u8 p) '() (cons (utf8->string (u8-list->bytevector (reverse header))) header*))]
     [(= c2 #x0d)
      (loop c2 (get-u8 p) (cons c1 header) header*)]
     [else
      (loop (get-u8 p) (get-u8 p) (cons c2 (cons c1 header)) header*)])))

(define (parse-uri uri)
  (values "graph.facebook.com" "443" "/19292868552" #t))

(define http-get
  (match-lambda*
   [(host port path)
    (let ([p (socket-port (make-client-socket host port))])
      (put-bytevector p (string->utf8 (format "GET ~a HTTP/1.1\r\nHost: ~a\r\nUser-Agent: Mosh Scheme (http)\r\n\r\n" path host)))
      (let1 header* (read-header p)
        (let1 content-length (get-content-length header*)
          (let loop ([i 0]
                     [body* '()])
            (cond
             [(= i content-length)
              (close-port p)
              (utf8->string (u8-list->bytevector (reverse body*)))]
             [else
              (loop (+ i 1) (cons (get-u8 p) body*))])))))]
   [(host port path #t)
    (let* ([socket (make-client-socket host port)]
           [dummy (socket-sslize! socket)]
           [p (socket-port socket)])
      (put-bytevector p (string->utf8 (format "GET ~a HTTP/1.1\r\nHost: ~a\r\nUser-Agent: Mosh Scheme (http)\r\n\r\n" path host)))
      (let1 header* (read-header p)
        (let1 content-length (get-content-length header*)
          (let loop ([i 0]
                     [body* '()])
            (cond
             [(= i content-length)
              (close-port p)
              (utf8->string (u8-list->bytevector (reverse body*)))]
             [else
              (loop (+ i 1) (cons (get-u8 p) body*))])))))]
   [(uri)
    (receive (host port path ssl?) (parse-uri uri)
        (http-get host port path ssl?))
    ]))

)
