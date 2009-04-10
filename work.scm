(import (except (rnrs) define-record-type)
        (srfi :99)
        (mosh test))

(test-begin "srfi-99")

(define :point
  (make-rtd 'point '#((mutable x) (mutable y))))

(define make-point (rtd-constructor :point))

(define point? (rtd-predicate :point))
(define point-x (rtd-accessor :point 'x))
(define point-y (rtd-accessor :point 'y))
(define point-x-set! (rtd-mutator :point 'x))
(define point-y-set! (rtd-mutator :point 'y))

(define p1 (make-point 1 2))
(test-true (point? p1))
(test-eqv 1 (point-x p1))
(test-eqv 2 (point-y p1))
(point-x-set! p1 5)
(test-eqv 5 (point-x p1))

(test-end)
