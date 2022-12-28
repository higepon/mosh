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
          (set! list* (cons `(,var . ,(reverse var*)) list*))
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
  (set! sym* '())
  (set! list* '()))

;; Test Numbers.
(test-equal "Object::Number(1)" (gen 1))

;; Test Symbols.
(reset)
(test-equal "sym0" (gen 'a))
(test-equal '(("sym0" . a)) sym*)
(test-equal "sym1" (gen 'b))
(test-equal '(("sym0" . a) ("sym1" . b)) (reverse sym*))

;; Test Pairs.
(reset)
(test-equal "list0" (gen '(1)))
(test-equal '(("list0" . ("Object::Number(1)"))) (reverse list*))

(reset)
(test-equal "list0" (gen '(1 2)))
(test-equal '(("list0" . ("Object::Number(1)" "Object::Number(2)"))) (reverse list*))

(reset)
(test-equal "list0" (gen '(1)))
(test-equal '(("list0" . ("Object::Number(1)"))) (reverse list*))
(test-equal "list1" (gen '(1 2)))
(test-equal '(("list0" . ("Object::Number(1)")) ("list1" . ("Object::Number(1)" "Object::Number(2)"))) (reverse list*))

(reset)
(test-equal "list1" (gen '(1 (2))))
(test-equal '(("list0" . ("Object::Number(2)")) ("list1" . ("Object::Number(1)" "list0"))) (reverse list*))

(reset)
(test-equal "list2" (gen '(1 (2 (3 4)))))
(test-equal '(("list0" . ("Object::Number(3)" "Object::Number(4)"))
              ("list1" . ("Object::Number(2)" "list0"))
              ("list2" . ("Object::Number(1)" "list1"))) (reverse list*))

(reset)
(test-equal "list0" (gen '(a b)))
(test-equal '(("sym0" . a) ("sym1" . b)) (reverse sym*))
(test-equal '(("list0" . ("sym0" "sym1"))) (reverse list*))

(reset)
(test-equal "list1" (gen '(a (b))))
(test-equal '(("sym0" . a) ("sym1" . b)) (reverse sym*))
(test-equal '(("list0" . ("sym1")) ("list1" . ("sym0" "list0"))) (reverse list*))


(test-results)
