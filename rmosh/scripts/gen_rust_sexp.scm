;; Generate Rust code to construct sexp* in Rust.
(import (scheme base))
(import (match))
(import (only (mosh) format))
(import (mosh test))

(define (gen-number n)
  (format "Object::Number(~a)" n))

(define (gen sexp)
  (match sexp
    [(? number? n) (gen-number n)]))

(test-equal "Object::Number(1)" (gen 1))
(test-results)
