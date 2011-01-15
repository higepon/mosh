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
  (export http-get->utf8 http-get http-post http-post->utf8)
  (import (rnrs)
          (mosh)
          (mosh control)
          (irregex)
          (match)
          (srfi :8)
          (only (srfi :13) string-null? string-join)
          (shorten)
          (mosh socket)
          (uri)
          )

;; This library is undocumented. APIs is subject to change without notice.

;; ToDo
;;   http-get->bytevector
;;   Now utf-8 only
;;   redirect
;;   status code
;;   refactor get-content-length get-location
(define (get-content-length header*)
  (cond
   [(assoc "Content-Length" header*) => (^(cl) (string->number (cdr cl)))]
   [else #f]))

(define (get-location header*)
  (cond
   [(assoc "Location" header*) => cdr]
   [else #f]))

(define (get-status header*)
  (string->number (cdr (assoc "Status" header*))))

(define (header*->alist header*)
  (map (^(header)
         (cond
          [(irregex-search "([^:]+): (.+)" header) =>
           (^m (cons (irregex-match-substring m 1) (irregex-match-substring m 2)))]
          [(irregex-search "^HTTP/1.[01] ([0-9]+).*" header) =>
           (^m (cons "Status" (irregex-match-substring m 1)))]
          [else
           (error 'header*->alist "invalid HTTP Response Header" header)])) header*))

(define (read-header p)
  (let loop ([c1 (get-u8 p)]
             [c2 (get-u8 p)]
             [header '()]
             [header* '()])
    (cond
     [(and (null? header) (and (= c1 #x0d) (= c2 #x0a)))
      (header*->alist header*)]
     [(and (eof-object? c1) (eof-object? c2))
      (cons (utf8->string (u8-list->bytevector (reverse header))) header*)]
     [(and (= c1 #x0d) (= c2 #x0a))
      (loop (get-u8 p) (get-u8 p) '() (cons (utf8->string (u8-list->bytevector (reverse header))) header*))]
     [(= c2 #x0d)
      (loop c2 (get-u8 p) (cons c1 header) header*)]
     [else
      (loop (get-u8 p) (get-u8 p) (cons c2 (cons c1 header)) header*)])))

(define (parse-uri uri)
  (cond
   [(irregex-search '(: (=> scheme (or "http" "https")) "://" (=> host (+ (~ ":" "/"))) ":" (=> port (+ numeric)) (=> path (? "/") (* any))) uri) =>
    (lambda (m)
      (let* ([path (irregex-match-substring m 'path)]
             [path (if (string-null? path) "/" path)])
           (values (irregex-match-substring m 'host)
                   (irregex-match-substring m 'port)
                   path
                   (string=? (irregex-match-substring m 'scheme) "https"))))]
   [(irregex-search '(: (=> scheme (or "http" "https")) "://" (=> host (+ (~ ":" "/"))) (=> path (? "/") (* any))) uri) =>
    (lambda (m)
      (let* ([scheme (irregex-match-substring m 'scheme)]
             [host (irregex-match-substring m 'host)]
             [path (irregex-match-substring m 'path)]
             [path (if (zero? (string-length path)) "/" path)])
           (values host (if (string=? scheme "https") "443" "80") path (string=? scheme "https"))))]
   [else
    (assertion-violation 'parse-uri "malformed uri" uri)]))

(define http-get->utf8
  (match-lambda*
   [(host port path ssl?)
    (receive (body status header*) (http-get host port path ssl?)
             (values (utf8->string body) status header*))]
   [(uri)
    (receive (host port path ssl?) (parse-uri uri)
      (http-get->utf8 host port path ssl?))]))

(define http-post->utf8
  (match-lambda*
   [(host port path ssl? data)
    (receive (body status header*) (http-post host port path ssl? data)
             (values (utf8->string body) status header*))]
   [(uri data)
    (receive (host port path ssl?) (parse-uri uri)
      (http-post->utf8 host port path ssl? data))]))


(define http-get
  (match-lambda*
   [(host port path ssl?)
    (let1 socket (make-client-socket host port)
      (when (and ssl? (not (ssl-supported?)))
        (assertion-violation 'http-get "ssl is not supprted"))
      (when ssl?
        (socket-sslize! socket))
      (let1 p (socket-port socket)
        (put-bytevector p (string->utf8 (format "GET ~a HTTP/1.1\r\nHost: ~a\r\nUser-Agent: Mosh Scheme (http)\r\n\r\n" path host)))
        (let* ([header* (read-header p)]
               [status (get-status header*)])
          (cond
           [(get-location header*) =>
            (^(location) (http-get location))]
           [else
            (case status
              [(200)
               (let1 content-length (get-content-length header*)
                 (let loop ([i 0]
                            [body* '()])
                   (cond
                    [(= i content-length)
                     (close-port p)
                     (values (u8-list->bytevector (reverse body*)) status header*)]
                    [else
                     (loop (+ i 1) (cons (get-u8 p) body*))])))]
              [else
               (values #vu8() status header*)])]))))]
   [(uri)
    (receive (host port path ssl?) (parse-uri uri)
        (http-get host port path ssl?))
    ]))

(define (alist->urlencoded alist)
  (string-join
   (map (match-lambda
            [(key . value) (string-append (uri-encode key) "=" (uri-encode value))]) alist)
   "&"))

(define http-post
  (match-lambda*
   [(host port path ssl? data)
    (let1 socket (make-client-socket host port)
      (when (and ssl? (not (ssl-supported?)))
        (assertion-violation 'http-get "ssl is not supprted"))
      (when ssl?
        (socket-sslize! socket))
      (let ([p (socket-port socket)]
            [param (alist->urlencoded data)])
        ;; To prevent Chunked Transfer-Encoding, we don't use HTTP/1.1.
        (put-bytevector p (string->utf8 (format "POST ~a HTTP/1.0\r\nHost: ~a\r\nUser-Agent: Mosh Scheme (http)\r\nContent-Type: application/x-www-form-urlencoded\r\nContent-Length: ~d\r\n\r\n~a" path host (string-length param) param)))
        (let* ([header* (read-header p)]
               [status (get-status header*)])
          (cond
           [(get-location header*) =>
            (^(location) (http-get location))]
           [else
            (case status
              [(200)
               (let1 content-length (get-content-length header*)
                 (let loop ([i 0]
                            [body* '()]
                            [u8 (get-u8 p)])
                   (cond
                    [(or (and content-length (= i content-length)) (eof-object? u8))
                     (close-port p)
                     (values (u8-list->bytevector (reverse body*)) status header*)]
                    [else
                     (loop (+ i 1) (cons u8 body*) (get-u8 p))])))]
              [else
               (values #vu8() status header*)])]))))]
   [(uri data)
    (receive (host port path ssl?) (parse-uri uri)
      (http-post host port path ssl? data))
    ]))
)
