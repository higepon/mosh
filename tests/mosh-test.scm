(import (prefix (mosh test) mosh-test:)
        (srfi :64)
        (rnrs))

(define-syntax test-true
  (lambda (x)
    (syntax-case x ()
      [(_ expr)
      #'(test-eq #t expr)])))

(define-syntax test-false
  (lambda (x)
    (syntax-case x ()
      [(_ expr)
      #'(test-eq #f expr)])))


(test-begin "basic assert")

(test-true (mosh-test:test-true #t))
(test-true (mosh-test:test-true (number? 3)))
(test-false (mosh-test:test-true (number? #\a)))

;; mosh-test:test-true evaluate expression only once.
(define once 0)
(test-true (mosh-test:test-true (begin (set! once (+ once 1)) #t)))
(test-eq 1 once)

(test-true (condition? (mosh-test:test-true (car 3))))

(test-false (mosh-test:test-false (pair? 3)))

(mosh-test:test-eq 3 (+ 1 2))
(mosh-test:test-eq 4 (+ 1 2))

(mosh-test:fail "my tests failed")

(mosh-test:test-error error? 3)

(mosh-test:test-error pair? (car 3))

(mosh-test:test-null 3)

(mosh-test:test-write-equal "#\\b" #\a)


(test-end)
