(import (rnrs)
        (mosh test))

(let ()
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
  (test-true (condition? foo))
  (test-true (cond1? foo))
  (test-eq (cond1-x foo) 'foo))

(let ()
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
  (define-record-type (&cond2 make-cond2 real-cond2?)
    (parent &condition)
    (fields
     (immutable y real-cond2-y)))
  (define cond2?
    (condition-predicate
     (record-type-descriptor &cond2)))
  (define cond2-y
    (condition-accessor
     (record-type-descriptor &cond2)
     real-cond2-y))
  (define bar (make-cond2 'bar))
  (test-true (condition? (condition foo bar)))
  (test-true (cond1? (condition foo bar)))
  (test-true (cond2? (condition foo bar)))
  (test-true (cond1? (condition foo)))
  (test-eq (cond1-x (condition foo bar)) 'foo)
  (test-eq (cond2-y (condition foo bar)) 'bar)
  (test-equal (simple-conditions (condition foo bar))
                 (list foo bar))
  (test-equal (simple-conditions
               (condition foo (condition bar)))
           (list foo bar))
)


(let ()
  (define-record-type (&cond1 make-cond1 real-cond1?)
    (parent &message)
    (fields
     (immutable x real-cond1-x)))
  #t
)

(let ()
  (define-condition-type &c &condition
    make-c c?
    (x c-x))
  (define-condition-type &c1 &c
    make-c1 c1?
    (a c1-a))
  (define-condition-type &c2 &c
    make-c2 c2?
    (b c2-b))
  (define v1 (make-c1 "V1" "a1"))
  (define v2 (make-c2 "V2" "b2"))
  (define v3 (condition
              (make-c1 "V3/1" "a3")
              (make-c2 "V3/2" "b3")))
  (define v4 (condition v1 v2))
  (define v5 (condition v2 v3))
  (test-true  (c? v1))
  (test-true  (c1? v1))
  (test-false  (c2? v1))
  (test-equal (c-x v1) "V1")
  (test-equal (c1-a v1) "a1")
  (test-true  (c? v2))
  (test-false  (c1? v2))
  (test-true  (c2? v2))
  (test-equal (c-x v2) "V2")
  (test-equal (c2-b v2) "b2")
  (test-true  (c? v3))
  (test-true  (c1? v3))
  (test-true  (c2? v3))
  (test-equal (c-x v3) "V3/1")
  (test-equal (c1-a v3) "a3")
  (test-equal (c2-b v3) "b3")
  (test-true  (c? v4))
  (test-true  (c1? v4))
  (test-true  (c2? v4))
  (test-equal (c-x v4) "V1")
  (test-equal (c1-a v4) "a1")
  (test-equal (c2-b v4) "b2")
  (test-true  (c? v5))
  (test-true  (c1? v5))
  (test-true  (c2? v5))
  (test-equal (c-x v5) "V2")
  (test-equal (c1-a v5) "a3")
  (test-equal (c2-b v5) "b2")
)

(let ()
  (test-true  (warning? (make-warning)))
  (test-true  (serious-condition? (make-serious-condition)))
  (test-true  (violation? (make-violation)))
  (test-true  (assertion-violation? (make-assertion-violation)))
  (test-true  (irritants-condition? (make-irritants-condition '(1 2 3))))
  (test-true  (who-condition? (make-who-condition #f)))
  (test-true  (non-continuable-violation? (make-non-continuable-violation)))
  (test-true  (implementation-restriction-violation? (make-implementation-restriction-violation)))
  (test-true  (lexical-violation? (make-lexical-violation)) )
  (test-true  (syntax-violation? (make-syntax-violation 'form 'subform)))
  (test-true  (undefined-violation? (make-undefined-violation)))
)
(test-results)
