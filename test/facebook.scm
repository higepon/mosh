(import (rnrs)
        (match)
        (mosh)
        (shorten)
        (facebook)
        (mosh control)
        (mosh test))

;; (define access-token "abcdefg")
(include "test/access_token.scm")

(define (assoc-value obj lst fallback)
  (aif (assoc obj lst)
       (cdr it)
       fallback))

;; (let1 friends (fb-friends access-token)
;;   (test-true (list? friends)))

;; (for-each (^(news) (match (assoc "from" news)
;;                      [("from" . #(("name" . name) ("id" . id)))
;;                       (test-true #t)]
;;                      [("from" . #(("name" . name) ("category" . category) ("id" . id)))
;;                       (test-true #t)]
;;                      [else
;;                       (test-true #f)]))
;;           (fb-news access-token))

(let1 jpg (fb-picture access-token)
  (define jpg-magic #xd8ff)
  (test-true (bytevector? jpg))
  (test-equal jpg-magic (bytevector-u16-ref jpg 0 'little)))

(test-results)
