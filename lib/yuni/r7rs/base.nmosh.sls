(library (yuni r7rs base)
         (export

           ;; Procedure and coninuations
           call-with-current-continuation
           call/cc apply dynamic-wind procedure?
           ;; Core syntax
           begin if lambda 
           define 
           set! 
           ;; Generic 
           eq? equal?  eqv? 
           ;; syntax-rules
           define-syntax 
           let-syntax syntax-rules
           ;; Lists/pairs
           reverse 
           caar cadr car cdr cdddar cddddr
           map cons append assoc assq assv for-each 
           list-ref list-set!
           list-tail list? list length 
           make-list 
           member memq memv null?  pair? 
           ;; Vectors
           list->vector make-vector vector->list 
           vector-fill! vector-for-each vector-length
           vector-map vector-ref vector-set!
           vector? vector 
           ;; Blob
           make-blob 
           blob-length blob-u8-ref blob-u8-set! blob? 
           partial-blob 
           ;; Strings
           make-string list->string number->string
           string->list string->number 
           string->vector string-append string-copy
           string-fill! string-for-each string-length
           string-map string-ref string-set!
           string<=? string<? string=?
           string>=? string>? string?
           string substring 
           vector->string

           ;; Numeric
           * + - / <= < = >= > abs 
           ceiling denominator even?
           exact->inexact exact-integer-sqrt exact-integer?  
           exact?  expt floor 
           gcd inexact->exact inexact?  integer? 
           complex?  lcm min modulo negative?  zero?  truncate max 
           positive?  number? numerator odd?
           rational? rationalize real? remainder round quotient

           ;; Derived
           or and 
           case-lambda case cond do
           let* letrec* letrec-syntax
           letrec let quasiquote quote 

           ;; Boolean
           boolean?  not 

           ;; symbols
           symbol?  string->symbol symbol->string

           ;; char
           integer->char char->integer
           char-alphabetic? char-lower-case? char-numeric?
           char-upper-case? char-whitespace? char<=?
           char<? char=? char>=? char>? char? 

           ;; exceptions
           guard raise-continuable raise
           with-exception-handler

           ;; records
           define-record-type
           ;; error
           error syntax-error 
           ;; parameter
           make-parameter parameterize
           ;; mutable-pairs
           set-car! set-cdr!

           ;; renamed  
           list-copy vector-copy blob-copy blob-copy!
           partial-blob-copy!

           ;; removed from draft 1
           ;; => display nil newline 
           ;;
           ;;
           ;; import 
           ;; include 
           )
         (import
           (except (rnrs) cond case syntax-rules)
           (yuni r7rs syntax-rules)
           (yuni r7rs cond-case)
           (yuni r7rs blob)
           (rnrs r5rs)
           (rnrs mutable-pairs)
           (rnrs mutable-strings)
           (only (system) parameterize make-parameter))

(define (string->vector x)
  (list->vector (string->list x)))
(define (string-map proc . x)
  (list->string (apply map (cons proc (map string->list x)))))
(define (vector->string x)
  (list->string (vector->list x)))
(define (exact-integer? x)
  (and (exact? x)
       (integer? x)))

(define (list-set! list k obj) 
  (if (= k 0)
    (set-car! list obj)
    (list-set! (cdr list) (- k 1) obj)))

(define (list-copy2 cur x)
  (if (pair? x)
    (list-copy2 (cons (car x) cur) (cdr x))
    (reverse cur)))

(define (list-copy x)
  (list-copy2 '() x))

(define (make-list3 cur len obj)
  (if (= 0 len)
    cur
    (make-list3 (cons obj cur) (- len 1) obj)))

(define make-list 
  (case-lambda
    ((len) (make-list len (if #f #f)))
    ((len obj)
     (make-list3 '() len obj))))

(define (vector-copy-itr i len x y)
  (unless (= len i)
    (vector-set! y i (vector-ref x i))
    (vector-copy-itr (+ i 1) len x y)))

(define (vector-copy x)
  (let* ((cnt (vector-length x))
         (y (make-vector cnt)))
    (vector-copy-itr 0 cnt x y)))

(define-syntax syntax-error
  (syntax-rules ()
    ((_ msg obj ...)
     (syntax-violation #f msg (list obj ...)))))

)
