;; Generate Rust code to construct sexp* in Rust.
(import (scheme base))
(import (match))
(import (only (mosh) format))
(import (only (mosh control) let1))
(import (mosh test))

(define sym-idx  0)
(define list-idx  0)
(define sym* '())
(define list* '())

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

;; List.
(define (gen-list expr*)
  (let loop ([var* '()]
             [expr* expr*])
    (cond
      [(null? expr*)
        (let1 var (next-list-var)
          (set! list* (cons `(,var . ,var*) list*))
          var)]
      [else
        (let1 var (gen (car expr*))
          (loop (cons var var*) (cdr expr*)))])))

(define (next-list-var)
  (let1 var (format "list~a" list-idx)
    (set! list-idx (+ list-idx 1))
     var))     

;; The main generator.
(define (gen sexp)
  (match sexp
    [(? number? n) (gen-number n)]
    [(? symbol? sym) (gen-symbol sym)]
    [(expr* ...) (gen-list expr*)]
  ))
    
;; Test helpers
(define (reset)
  (set! sym-idx 0)
  (set! list-idx 0)  
  (set! sym* '()))

;; Test Numbers.
(test-equal "Object::Number(1)" (gen 1))

;; Test Symbols.
(reset)
(test-equal "sym0" (gen 'a))
(test-equal '(("sym0" . a)) sym*)
(test-equal "sym1" (gen 'b))
(test-equal '(("sym1" . b) ("sym0" . a)) sym*)

;; Test Pairs.
(reset)
(test-equal "list0" (gen '(1)))
(test-equal '(("list0" . ("Object::Number(1)"))) list*)
(test-results)
