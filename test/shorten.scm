(import (rnrs)
        (shorten)
        (mosh test))

(test-equal '1 (apply (^() 1) '()))
(test-equal '(1 2 3 4) (map (^(x) x) '(1 2 3 4)))
(test-equal '(2 4 6 8) (map (^(x y) (+ x y)) '(1 2 3 4) '(1 2 3 4)))

(test-equal '(1 2 3 4) (map (^a a) '(1 2 3 4)))
(test-equal '(1 2 3 4) (map (^b b) '(1 2 3 4)))
(test-equal '(1 2 3 4) (map (^_ _) '(1 2 3 4)))

(test-equal '((1 5) (2 6) (3 7) (4 8)) (map (^a* a*) '(1 2 3 4) '(5 6 7 8)))

(test-results)
