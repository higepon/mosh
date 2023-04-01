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
  (export fb-comments->json fb-news fb-friends fb-picture fb-post-feed fb-user fb-get-token fb-picture-path fb-friend fb-post-like fb-post-comment fb-news->json )
  (import (rnrs)
          (mosh)
          (mosh control)
          (http)
          (match)
          (json))

;; This library is undocumented. APIs are subject to change without notice.

(define (fb-get-token code client-id client-secret redirect-uri)
  (let1 tokens (http-get->utf8 (format "https://graph.facebook.com/oauth/access_token?client_id=~a&redirect_uri=~a&client_secret=~a&code=~a"
                                client-id redirect-uri client-secret code))
    (match (string-split tokens #\&)
      ([token . more]
       (cadr (string-split token #\=))))))

(define (fb-picture-path id)
  (format "http://graph.facebook.com/~a/picture" id))

(define (call-json-api id api token)
  (let-values (([body status header*] (http-get->utf8 (format "https://graph.facebook.com/~a/~a?access_token=~a" id api token))))
    (case status
      [(200)
       (match (json-read (open-string-input-port body))
         [#(("data" . data) paging ...)
          (map vector->list data)]
         [json
          (vector->list json)])]
      [else
       (aif (assoc "WWW-Authenticate" header*)
            (error 'call-json-api (format "facebook API error: ~a ~a:~a" status (car it) (cdr it)) `(,api ,token))
            (error 'call-json-api "facebook API error: unknown error" `(,api ,token)))])))

(define (call-json-api->json id api token)
  (let-values (([body status header*] (http-get->utf8 (format "https://graph.facebook.com/~a/~a?access_token=~a" id api token))))
    (case status
      [(200)
       body]
      [else
       (aif (assoc "WWW-Authenticate" header*)
            (error 'call-json-api (format "facebook API error: ~a ~a:~a" status (car it) (cdr it)) `(,api ,token))
            (error 'call-json-api "facebook API error: unknown error" `(,api ,token)))])))

(define (fb-news->json token)
  (call-json-api->json 'me 'home token))

(define (fb-comments->json token post_id)
  (call-json-api->json post_id 'comments token))

(define (fb-friends token)
  (call-json-api 'me 'friends token))

(define (fb-user token)
  (call-json-api 'me "" token))

(define (fb-friend id token)
  (call-json-api id "" token))

(define (fb-news token)
  (call-json-api 'me 'home token))

(define (fb-picture token)
  (let-values (([body status header*] (http-get (format "https://graph.facebook.com/me/picture?access_token=~a" token))))
    (case status
      [(200) body]
      [else
       (aif (assoc "WWW-Authenticate" header*)
            (error 'call-json-api (format "facebook API error: ~a:~a" (car it) (cdr it)) `(picture ,token))
            (error 'call-json-api "facebook API error: unknown error" `(picture ,token)))])))

(define fb-post-feed
  (match-lambda*
   [(token message)
    (fb-post-feed "me" token message)]
    [(who token message)
     (let-values (([body status header*] (http-post->utf8 (format "https://graph.facebook.com/~a/feed?access_token=~a" who token) `(("message" . ,message)))))
       (case status
         [(200)
          (match (json-read (open-string-input-port body))
            [#(("data" . data) paging ...)
             (map vector->list data)]
            [#((id . _))
             '()]
            [json
             (error 'call-json-api (format "facebook API error: ~a" json))])]
         [else
          (aif (assoc "WWW-Authenticate" header*)
               (error 'call-json-api (format "facebook API error: ~a:~a" (car it) (cdr it)) `(,"feed" ,token))
               (error 'call-json-api "facebook API error: unknown error" `(,"feed" ,token)))]))]))

(define (fb-post-comment token object-id message)
  (let-values (([body status header*] (http-post->utf8 (format "https://graph.facebook.com/~a/comments?access_token=~a" object-id token) `(("message" . ,message)))))
    (case status
      [(200) #t]
      [else
       (aif (assoc "WWW-Authenticate" header*)
            (error 'call-json-api (format "facebook API error: ~a:~a" (car it) (cdr it)) `(,"feed" ,token))
            (error 'call-json-api "facebook API error: unknown error" `(,"feed" ,token)))])))

(define (fb-post-like token object-id)
  (let-values (([body status header*] (http-post->utf8 (format "https://graph.facebook.com/~a/likes?access_token=~a" object-id token) `())))
    (case status
      [(200) #t]
      [else
       (aif (assoc "WWW-Authenticate" header*)
            (error 'fb-post-like (format "facebook API error: ~a:~a" (car it) (cdr it)) `(("token" . ,token) ("object-id" . ,object-id)))
            (error 'fb-post-like "facebook API error: unknown error" `(("token" . ,token) ("object-id" . ,object-id))))])))

)
