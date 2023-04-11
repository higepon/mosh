(import (rnrs)
        (match)
        (mosh test))


(test-equal '(0 1 2 3 4 5)
            (match '(0 (1 2) (3 4 5))
              ((a (b c) (d e f))
               (list a b c d e f))))

(test-equal '(number 123)
            (match 123
              ((? string? x) (list 'string x))
              ((? number? x) (list 'number x))))

#;(test-eqv 42
          (match '(the answer is 42)
            (`(the answer is ,value) value)
            (else #f)))
#;(test-false
 (match '(the answer was 42)
   (`(the answer is ,value) value)
   (else #f)))

(test-eqv 'd
          (match '(a b c d)
            ((the answer is value) value)
            (else #f)))

(test-results)
