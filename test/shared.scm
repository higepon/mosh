(import (rnrs)
        (good_shared)
        (mosh test))

(let ([x '#1=(a . #1#)])
  (test-eq 'a (car x))
  (test-eq x (cdr x)))

(let ([x '#1=#(a #1#)])
  (test-true (vector? x))
  (test-eq 2 (vector-length x))
  (test-eq 'a (vector-ref x 0))
  (test-eq x (vector-ref x 1)))

(let ([x '#(#1="abc" #1# #1#)])
  (test-true (vector? x))
  (test-eq 3 (vector-length x))
  (test-equal "abc" (vector-ref x 0))
  (test-eq (vector-ref x 0) (vector-ref x 1))
  (test-eq (vector-ref x 1) (vector-ref x 2)))

(let ([x '(#1=a . #1#)])
  (test-eq 'a (car x))
  (test-eq 'a (cdr x)))

(let ([x '#1=(#1#)])
  (test-eq x (car x)))

(let ([x '(#1=a #2=b #1# #2#)])
  (test-equal '(a b a b) x))

(test-results)
