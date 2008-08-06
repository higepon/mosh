(import (rnrs))

;; (let ([c (make-message-condition "hige")])
;;   (display c)
;;   (display (message-condition? c)))

(define-record-type (&cond1 make-cond1 real-cond1?)
  (parent &condition)
  (fields
   (immutable x real-cond1-x)))
(define cond1?
  (condition-predicate
   (record-type-descriptor &cond1)))
(define cond1-x
  (condition-accessor
   (record-type-descriptor &cond1)
   real-cond1-x))
(define foo (make-cond1 'foo))
(display (condition? foo)); => #t
(display (cond1? foo)); => #t
(display (cond1-x foo)); => foo
