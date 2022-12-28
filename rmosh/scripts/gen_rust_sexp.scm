;; Generate Rust code to construct sexp* in Rust.
(import (scheme base))
(import (scheme write))
(import (match))
(import (only (mosh) format))
(import (only (mosh control) let1))
(import (mosh test))
(import (only (srfi :13) string-join))

(define str-idx  0)
(define sym-idx  0)
(define pair-idx  0)
(define list-idx  0)
(define str* '())
(define sym* '())
(define pair* '())
(define list* '())

;; Reset the stage.
(define (reset)
  (set! str-idx 0)
  (set! sym-idx 0)
  (set! pair-idx 0)    
  (set! list-idx 0)  
  (set! str* '())  
  (set! sym* '())
  (set! pair* '())
  (set! list* '()))

;; Char.
(define (gen-char c)
  (format "Object::Char('~a')" c))

;; Number.
(define (gen-number n)
  (format "Object::Number(~a)" n))

;; Boolean.
(define (gen-boolean b)
  (format "Object::~a" (if b "True" "False")))

;; String
(define (gen-string str)
  (let1 var (next-str-var)
    (set! str* (cons `(,str . ,var) str*))
    (format "~a" var)))

(define (next-str-var)
  (let1 var (format "str~a" str-idx)
    (set! str-idx (+ str-idx 1))
     var))

;; Symbol
(define (gen-symbol sym)
  (cond
    [(assq sym sym*) => (match-lambda [(_ . var) var])]
    [else
      (let1 var (next-sym-var)
        (set! sym* (cons `(,sym . ,var) sym*))
        (format "~a" var))]))

(define (next-sym-var )
  (let1 var (format "sym~a" sym-idx)
    (set! sym-idx (+ sym-idx 1))
     var))

;; Dot pair.
(define (gen-pair first second)
  (let* ([var1 (gen first)]
         [var2 (gen second)]
         [var (next-pair-var)])
    (set! pair* (cons `(,var . (,var1 . ,var2)) pair*))
    var))

(define (next-pair-var)
  (let1 var (format "pair~a" pair-idx)
    (set! pair-idx (+ pair-idx 1))
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
    [(? char? n) (gen-char n)]
    [(? number? n) (gen-number n)]
    [(? string? str) (gen-string str)]    
    [(? symbol? sym) (gen-symbol sym)]
    [(? boolean? b) (gen-boolean b)]
    [() "Object::Nil"]
    [(expr* ...) (gen-list expr*)]
    [(first . second) (gen-pair first second)]
    [else
      (error (format "sexp ~s didn't match" sexp))]
  ))

;; Test Chars.
(test-equal "Object::Char('a')" (gen #\a))

;; Test Numbers.
(test-equal "Object::Number(1)" (gen 1))

;; Test Booleans.
(test-equal "Object::True" (gen #t))
(test-equal "Object::False" (gen #f))

;; Test Strings
(reset)
(test-equal "str0" (gen "a"))
(test-equal '(("a" . "str0")) str*)
(test-equal "str1" (gen "b"))
(test-equal '(("a" . "str0") ("b" . "str1")) (reverse str*))

;; Test Symbols.
(reset)
(test-equal "sym0" (gen 'a))
(test-equal '((a . "sym0")) sym*)
(test-equal "sym1" (gen 'b))
(test-equal '((a . "sym0") (b . "sym1")) (reverse sym*))

;; Test Dot Pairs.
(reset)
(test-equal "pair0" (gen '(1 . 2)))
(test-equal '(("pair0" . ("Object::Number(1)" . "Object::Number(2)"))) (reverse pair*))

(reset)
(test-equal "pair1" (gen '(1 2 . 3)))
(test-equal '(("pair0" "Object::Number(2)" . "Object::Number(3)") ("pair1" "Object::Number(1)" . "pair0")) (reverse pair*))

;; Test Pairs.
(reset)
(test-equal "Object::Nil" (gen '()))
(test-equal '() (reverse list*))

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
(test-equal "list0" (gen '(a b a)))
(test-equal '((a . "sym0") (b . "sym1")) (reverse sym*))
(test-equal '(("list0" . ("sym0" "sym1" "sym0"))) (reverse list*))

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

(reset)
(gen '((srfi 0) (srfi 1) (srfi 11) (srfi 13) (srfi 14) (srfi 16) (srfi 176) (srfi 19) (srfi 2) (srfi 23) (srfi 26) (srfi 27) (srfi 31) (srfi 37) (srfi 38) (srfi 39) (srfi 41) (srfi 42) (srfi 43) (srfi 48) (srfi 6) (srfi 61) (srfi 64) (srfi 67) (srfi 78) (srfi 8) (srfi 9) (srfi 98) (srfi 99) (srfi 151)
    (mosh)))

(gen "")

(for-each 
  (lambda (sym) 
    (match sym
      [(val . var)
        (format #t "let ~a = vm.gc.symbol_intern(\"~a\");\n" var val)]))
  (reverse sym*))

(for-each 
  (lambda (list) 
    (match list
      [(var . elm*)
        (format #t "let ~a = vm.gc.listn(&[~a]);\n" var (string-join elm* ","))]))
  (reverse list*))

(for-each 
  (lambda (str) 
    (match str
      [(val . var)
        (format #t "let ~a = vm.gc.new_string(~s);\n" var val)]))
  (reverse str*))
  