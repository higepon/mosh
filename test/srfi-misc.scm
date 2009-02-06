(import (except (rnrs) let-values error define-record-type cond)
        (srfi :0)
        (srfi :1)
        (srfi :2)
        (srfi :6)
        (srfi :8)
        (srfi :9)
        (srfi :11)
        (only (srfi :13) string-reverse string-concatenate)
        (srfi :14)
        (srfi :16)
        (srfi :19)
        (srfi :23)
        (srfi :26)
        (srfi :27)
        (srfi :31)
        (srfi :37)
        (srfi :38)
        (srfi :39)
        (srfi :41)
        (srfi :42)
        (only (srfi :43) vector=)
        (srfi :48)
        (srfi :61)
        (srfi :67)
        (srfi :78)
        (rnrs mutable-pairs)
        (mosh test))

;;;;;  SRFI-0   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cond-expand
 (mosh
   (define name 'mosh))
 (else
  (define name 'other)))

(test* name 'mosh)

;;;;;  SRFI-1   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test* (first '(1 2)) 1)
(test* (second '(1 2)) 2)

;;;;;  SRFI-6   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test* (let ((q (open-output-string))
             (x '(a b c)))
         (write (car x) q)
         (write (cdr x) q)
         (get-output-string q)) "a(b c)")

;;;;;  SRFI-8   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test* (receive (x y) (values 1 2)
         (- x y)) -1)

;;;;;  SRFI-9   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ()
  (define-record-type pare
    (kons x y) pare?
    (x kar set-kar!)
    (y kdr))
  (test/t (pare? (kons 2 3)))
  (test/f (pare? (cons 2 3))))

;;;;;  SRFI-11   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test* (let-values (([x y] (values 1 2)))
         (- x y)) -1)

;;;;;  SRFI-13   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test* (string-reverse "あいうえお") "おえういあ")
(test* (string-concatenate '("ab" "cd")) "abcd")

;;;;;  SRFI-14   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test/t (char-set #\a #\b #\c))

;;;;;  SRFI-16   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test/t (let ()
          (define plus
            (case-lambda
             (() 0)
             ((x) x)
             ((x y) (+ x y))
             ((x y z) (+ (+ x y) z))
             (args (apply + args))))
          (test* (plus) 0)
          (test*  (plus 1) 1)
          (test*  (plus 1 2 3) 6)))

;;;;;  SRFI-19   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tested on another place

;;;;;  SRFI-23   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test/exception error? (error "hoge" 1 2 3))

;;;;;  SRFI-27   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ([x (random-integer 10)])
  (test/t (< x 10))
  (test/t (integer? x)))

;;;;;  SRFI-31   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ()
  (define F (rec (F N)
                 ((rec (G K L)
                       (if (zero? K) L
                           (G (- K 1) (* K L)))) N 1)))
  (test* (F 0) 1)
  (test* (F 10) 3628800))

;;;;;  SRFI-37   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test/t (option? (option '(#\d "debug") #f #t
                         (lambda (option name arg debug batch paths files)
                           (values (or arg "2") batch paths files)))))

;;;;;  SRFI-38   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ([p (open-output-string)])
  (define a (cons 'val1 'val2))
  (set-cdr! a a)
  (write-with-shared-structure a p)
  (test* (get-output-string p) "#1=(val1 . #1#)"))

;;;;;  SRFI-39   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test* ((make-parameter 1234)) 1234)

;;;;;  SRFI-41   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ()
  (define strm123
    (stream-cons 1
                 (stream-cons 2
                              (stream-cons 3
                                           stream-null))))
  (define iter
    (stream-lambda (f x)
                   (stream-cons x (iter f (f x)))))
  (define nats (iter (lambda (x) (+ x 1)) 0))
  (define stream-add
    (stream-lambda (s1 s2)
                   (stream-cons
                    (+ (stream-car s1) (stream-car s2))
                    (stream-add (stream-cdr s1)
                                (stream-cdr s2)))))
  (define evens (stream-add nats nats))
  (test* (stream-car strm123) 1)
  (test* (stream-car (stream-cdr strm123)) 2)
  (test/f
   (stream-pair?
    (stream-cdr
     (stream-cons (/ 1 0) stream-null))))

  (test/f (stream? (list 1 2 3)))
  (test* (stream-car (stream-cdr nats)) 1)
  (test* (stream-car evens) 0)
  (test* (stream-car (stream-cdr evens)) 2)
  (test* (stream-car (stream-cdr (stream-cdr evens))) 4))

;;;;;  SRFI-42   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test* (list-ec (: i 5) (* i i)) '(0 1 4 9 16))

;;;;;  SRFI-43   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test/t (vector= eq? '#(a b c d) '#(a b c d)))

;;;;;  SRFI-48   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test* (format "Hello, ~a" "World!") "Hello, World!")
(test* (format "Error, list is too short: ~s" '(one "two" 3)) "Error, list is too short: (one \"two\" 3)")
(test* (format "test me") "test me")
(test* (format "~a ~s ~a ~s" 'this 'is "a" "test") "this is a \"test\"")
(test* (format "#d~d #x~x #o~o #b~b~%" 32 32 32 32)"#d32 #x20 #o40 #b100000
")
(test* (format "~a ~? ~a" 'a "~s" '(new) 'test) "a new test")
(test* (format #f "~&1~&~&2~&~&~&3~%") 
"
1
2
3
")
(test* (format #f "~a ~? ~a ~%" 3 " ~s ~s " '(2 2) 3) "3  2 2  3 
")

(test* (format "~w" (let ( (c '(a b c)) ) (set-cdr! (cddr c) c) c)) "#1=(a b c . #1#)")
(test* (format "~8,2F" 32) "   32.00")
(test* (format "~8,3F" (sqrt -3.8)) "0.000+1.949i")
(test* (format "~6,3F" 1/3) " 0.333")
(test* (format "~4F" 12) "  12")
(test* (format "~8,3F" 123.3456) " 123.346")
(test*  (format "~6,3F" 123.3456) "123.346")
(test*  (format "~2,3F" 123.3456) "123.346")
(test* (format "~8,3F" "foo") "     foo")
(test* (format "~a~a~&" (list->string (list #\newline)) "")
"
")

;;;;;  SRFI-61   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test* (cond (1 number? => values) (else 8)) 1)

;;;;;  SRFI-67   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test/t  (boolean-compare #t #f))

;;;;;  SRFI-78   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test/t (check (+ 1 1) => 2))

(test-end)
