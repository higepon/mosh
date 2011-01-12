(import (rnrs)
        (http)
        (json)
        (match)
        (mosh)
        (shorten)
        (mosh control)
        (mosh test))

;; (define access-token "abcdefg")
(include "test/access_token.scm")

(define (fb-friends token)
  (match (json-read (open-string-input-port (http-get->utf8 (format "https://graph.facebook.com/me/friends?access_token=~a" token))))
    [#(("data" . friends))
     (map vector->list friends)]
    [else
     '()]))

(define (fb-news token)
  (match (json-read (open-string-input-port (http-get->utf8 (format "https://graph.facebook.com/me/home?access_token=~a" token))))
    [#(("data" . news) paging)
     (map vector->list news)]
    [else
     '()]))


(let1 friends (fb-friends access-token)
  (test-true (list? friends)))

(for-each print (map (^(news) (match (assoc "from" news)
;                       [x (write x)]
                       [("from" . #(("name" . name) ("id" . id)))
                        (cons name (cdr (assoc "message" news)))]
                       [("from" . #(("name" . name) ("category" . category) ("id" . id)))
                        (cons name (cdr (assoc "message" news)))]))
              (fb-news access-token)))

(test-results)
