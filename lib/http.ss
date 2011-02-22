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
  (export http-get->utf8 http-get http-post http-post->utf8 http-post-json)
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
          [(#/([^:]+): (.*)/ header) =>
           (^m (cons (m 1) (m 2)))]
          [(#/^HTTP\/1.[01] ([0-9]+).*/ header) =>
           (^m (cons "Status" (m 1)))]
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
   [(host port path secure?)
    (receive (body status header*) (http-get host port path secure?)
             (values (utf8->string body) status header*))]
   [(uri)
    (receive (host port path secure?) (parse-uri uri)
      (http-get->utf8 host port path secure?))]))

(define http-post->utf8
  (match-lambda*
   [(host port path secure? param-alist)
    (receive (body status header*) (http-post host port path secure? param-alist)
             (values (utf8->string body) status header*))]
   [(uri param-alist)
    (receive (host port path secure?) (parse-uri uri)
      (http-post->utf8 host port path secure? param-alist))]))

(define http-get
  (match-lambda*
   [(host port path secure?)
    (http-send-request host port secure? (make-get-request path host))]
   [(uri)
    (receive (host port path secure?) (parse-uri uri)
        (http-get host port path secure?))
    ]))

;; fb-post-feed works with uri-encode.
(define (alist->urlencoded alist)
  (string-join
   (map (match-lambda
            [(key . value) (string-append (uri-encode key) "=" (uri-encode value))]) alist)
;            [(key . value) (string-append key "=" value)]) alist)
   "&"))

(define (make-get-request path host)
;  (write (format "GET ~a HTTP/1.1\r\nHost: ~a\r\nUser-Agent: Mosh Scheme (http)\r\n\r\n" path host) (current-error-port))
  (string->utf8 (format "GET ~a HTTP/1.1\r\nHost: ~a\r\nUser-Agent: Mosh Scheme (http)\r\n\r\n" path host)))

(define (make-post-request path host param-alist)
  (let1 param (alist->urlencoded param-alist)
;    (format (current-error-port) "param=<~s>" param)
    ;; To prevent Chunked Transfer-Encoding, we don't use HTTP/1.1.
    (string->utf8 (format "POST ~a HTTP/1.0\r\nHost: ~a\r\nUser-Agent: Mosh Scheme (http)\r\nContent-Type: application/x-www-form-urlencoded\r\nContent-Length: ~d\r\n\r\n~a" path host (string-length param) param))))

(define (make-post-request-json path host json . cookie)
  (if (pair? cookie)
      (string->utf8 (format "POST ~a HTTP/1.0\r\nHost: ~a\r\nUser-Agent: Mosh Scheme (http)\r\nContent-Type: application/json\r\nCookie: ~a\r\nContent-Length: ~d\r\n\r\n~a" path host (car cookie) (string-length json) json))
      ;; To prevent Chunked Transfer-Encoding, we don't use HTTP/1.1.
      (string->utf8 (format "POST ~a HTTP/1.0\r\nHost: ~a\r\nUser-Agent: Mosh Scheme (http)\r\nContent-Type: application/json\r\nContent-Length: ~d\r\n\r\n~a" path host (string-length json) json))))


(define (http-receive-response p)
  (let* ([header* (read-header p)]
         [status (get-status header*)])
    (cond
     [(get-location header*) =>
      (^(location) (http-get location))]
     [else
      (case status
        [(200)
         (cond
          [(get-content-length header*) =>
           (^(len)
             (let loop ([i 0]
                        [body* '()])
             (cond
              [(= i len)
               (close-port p)
               (values (u8-list->bytevector (reverse body*)) status header*)]
              [else
               (loop (+ i 1) (cons (get-u8 p) body*))])))]
          [else
           (let loop ([body* '()])
             (let1 u8 (get-u8 p)
             (cond
              [(eof-object? u8)
               (close-port p)
               (values (u8-list->bytevector (reverse body*)) status header*)]
              [else
               (loop (cons u8 body*))])))])]
        [else
         (values #vu8() status header*)])])))

(define (http-send-request host port secure? request)
  (let1 socket (make-client-socket host port)
    (when (and secure? (not (ssl-supported?)))
      (assertion-violation 'http-get "ssl is not supprted"))
    (when secure?
      (socket-sslize! socket))
    (let1 p (socket-port socket)
      (put-bytevector p request)
      (http-receive-response p))))

(define http-post-json
  (match-lambda*
   [(host port path secure? json . cookie)
    (http-send-request host port secure? (apply make-post-request-json path host json cookie))]
   [(uri json . cookie)
    (receive (host port path secure?) (parse-uri uri)
      (apply http-post-json host port path secure? json cookie))
    ]))


(define http-post
  (match-lambda*
   [(host port path secure? param-alist)
    (http-send-request host port secure? (make-post-request path host param-alist))]
   [(uri param-alist)
    (receive (host port path secure?) (parse-uri uri)
      (http-post host port path secure? param-alist))
    ]))


)
