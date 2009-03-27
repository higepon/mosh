(import (rnrs)
        (mosh)
        (mosh test2)
        )

(define (main args)
  (define :point
    (make-record-type-descriptor
     'point #f
     #f #f #f
     '#((mutable x) (mutable y))))
  (define :point-cd
    (make-record-constructor-descriptor :point #f #f))
  (define make-point (record-constructor :point-cd))
  (define point? (record-predicate :point))
  (define point-x (record-accessor :point 0))
  (define point-y (record-accessor :point 1))
  (define point-x-set! (record-mutator :point 0))
  (define point-y-set! (record-mutator :point 1))
  (define p1 (make-point 1 2))

;; (test-begin "hage")
 (test-begin "hage")


  (when (>= (length args) 1)
    (test-skip (test-not-match-name (cadr args))))
      


;; (let ()
  (test-assert "hage" 3)
  (test-assert (point? p1))
  (test-assert (point-x p1))
  (test-assert (point-y p1))
  (point-y-set! p1 3)
  (test-assert (point-y p1))
(test-skip (test-not-match-name "hage"))
(test-end)
)

(main (command-line))


