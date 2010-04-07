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


(let ()
(define b
 '#0=(#1=(10 #8# 12)
         #2=(20 21 22)
         #3=(#5# #4# #3#)
         #4=(40 #9# 42)
         #5=(#2# 51 52)
         #6=(60 61 62)
         #7=(#1# 71 #2#)
         #8=(80 81 82)
         #9=(90 91 #7#)
         )
 )
(write 'done)
(newline))

(test-results)
