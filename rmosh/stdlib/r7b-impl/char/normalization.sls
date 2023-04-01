#!r6rs
(library (r7b-impl char normalization)
         (export string-ni<=? string-ni<? string-ni=? string-ni>=? string-ni>?)
         (import (rnrs))

;; string normalization
;; We will use Unicode NFC
(define (string-normalize str)
  str)

(define (make-ni proc)
  (lambda x (apply proc (map x string-normalize))))

(define string-ni=? (make-ni string=?))
(define string-ni<? (make-ni string<?))
(define string-ni>? (make-ni string>?))
(define string-ni<=? (make-ni string<=?))
(define string-ni>=? (make-ni string>=?))

)
