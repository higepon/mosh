; Facebook Graph API
;
;   Copyright (c) 2011  Higepon(Taro Minowa)  <higepon@users.sourceforge.jp>
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
(library (facebook)
  (export fb-news fb-friends fb-picture fb-post-feed)
  (import (rnrs)
          (mosh)
          (mosh control)
          (http)
          (match)
          (json))

;; This library is undocumented. APIs is subject to change without notice.

(define (call-json-api api token)
  (let-values (([body status header*] (http-get->utf8 (format "https://graph.facebook.com/me/~a?access_token=~a" api token))))
    (case status
      [(200)
       (match (json-read (open-string-input-port body))
         [#(("data" . data) paging ...)
          (map vector->list data)]
         [json
          (error 'call-json-api (format "facebook API error: ~a" json))])]
      [else
       (aif (assoc "WWW-Authenticate" header*)
            (error 'call-json-api (format "facebook API error: ~a:~a" (car it) (cdr it)) `(,api ,token))
            (error 'call-json-api "facebook API error: unknown error" `(,api ,token)))])))

(define (fb-friends token)
  (call-json-api 'friends token))

(define (fb-news token)
  (call-json-api 'home token))

(define (fb-picture token)
  (let-values (([body status header*] (http-get (format "https://graph.facebook.com/me/picture?access_token=~a" token))))
    (case status
      [(200) body]
      [else
       (aif (assoc "WWW-Authenticate" header*)
            (error 'call-json-api (format "facebook API error: ~a:~a" (car it) (cdr it)) `(picture ,token))
            (error 'call-json-api "facebook API error: unknown error" `(picture ,token)))])))

(define (fb-post-feed token message)
(let-values (([body status header*] (http-post (format "https://graph.facebook.com/me/feed?access_token=~a" token) `(("message" . ,message)))))
    (case status
      [(200)
       (match (json-read (open-string-input-port body))
         [#(("data" . data) paging ...)
          (map vector->list data)]
         [json
          (error 'call-json-api (format "facebook API error: ~a" json))])]
      [else
       (aif (assoc "WWW-Authenticate" header*)
            (error 'call-json-api (format "facebook API error: ~a:~a" (car it) (cdr it)) `(,"feed" ,token))
            (error 'call-json-api "facebook API error: unknown error" `(,"feed" ,token)))])))

)
