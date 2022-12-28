;; Generate Rust code to construct sexp* in Rust.
(import (scheme base))
(import (scheme write))
(import (match))
(import (only (mosh) format))
(import (only (mosh control) let1))
(import (mosh test))
(import (only (srfi :13) string-join))

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
    (set! sym* (cons `(,sym . ,var) sym*))
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
(test-equal '((a . "sym0")) sym*)
(test-equal "sym1" (gen 'b))
(test-equal '((a . "sym0") (b . "sym1")) (reverse sym*))

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
(test-equal '((a . "sym0") (b . "sym1")) (reverse sym*))
(test-equal '(("list0" . ("sym0" "sym1"))) (reverse list*))

(reset)
(test-equal "list1" (gen '(a (b))))
(test-equal '((a . "sym0") (b . "sym1")) (reverse sym*))
(test-equal '(("list0" . ("sym1")) ("list1" . ("sym0" "list0"))) (reverse list*))

(reset)
(test-equal "list2" (gen '(a (b (c d)))))
(test-equal '((a . "sym0") (b . "sym1") (c . "sym2") (d . "sym3")) (reverse sym*))
(test-equal '(("list0" . ("sym2" "sym3")) ("list1" . ("sym1" "list0")) ("list2" . ("sym0" "list1"))) (reverse list*))

(reset)
(test-equal "list1" (gen '((a) b)))
(test-equal '((a . "sym0") (b . "sym1")) (reverse sym*))
(test-equal '(("list0" . ("sym0")) ("list1" . ("list0" "sym1"))) (reverse list*))

(test-results)

(gen '((srfi 0) (srfi 1) (srfi 11) (srfi 13) (srfi 14) (srfi 16) (srfi 176) (srfi 19) (srfi 2) (srfi 23) (srfi 26) (srfi 27) (srfi 31) (srfi 37) (srfi 38) (srfi 39) (srfi 41) (srfi 42) (srfi 43) (srfi 48) (srfi 6) (srfi 61) (srfi 64) (srfi 67) (srfi 78) (srfi 8) (srfi 9) (srfi 98) (srfi 99) (srfi 151)
    (mosh)))

(for-each 
  (lambda (sym) 
    (match sym
      [(var . val)
        (format #t "let ~a = vm.gc.symbol_intern(\"~a\");\n" var val)]))
  (reverse sym*))

(for-each 
  (lambda (list) 
    (match list
      [(var . elm*)
        (format #t "let ~a = vm.gc.listn(&[~a]);\n" var (string-join elm* ","))]))
  (reverse list*))

  