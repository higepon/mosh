;; Generate Rust code to construct sexp* in Rust.
(import (scheme base))
(import (match))
(import (only (mosh) format))
(import (only (mosh control) let1))
(import (mosh test))

(define sym-idx  0)
(define sym* '())

;; Number
(define (gen-number n)
  (format "Object::Number(~a)" n))

;; Symbol
(define (gen-symbol sym)
  (let1 var (next-sym-var)
    (set! sym* (cons `(,var . ,sym) sym*))
    (format "~a" var)))

(define (next-sym-var)
  (let1 var (format "sym~a" sym-idx)
    (set! sym-idx (+ sym-idx 1))
     var))

(define (gen sexp)
  (match sexp
    [(? number? n) (gen-number n)]
    [(? symbol? sym) (gen-symbol sym)]
  ))
    
;; Test helpers
(define (reset)
  (set! sym-idx 0)
  (set! sym* '()))

;; Test Numbers.
(test-equal "Object::Number(1)" (gen 1))

;; Test Symbols.
(reset)
(test-equal "sym0" (gen 'a))
(test-equal '(("sym0" . a)) sym*)
(test-equal "sym1" (gen 'b))
(test-equal '(("sym1" . b) ("sym0" . a)) sym*)
(test-results)
