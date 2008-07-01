#!/usr/bin/env gosh
(use gauche.test)
(use srfi-1)
(use util.match)
(use gauche.sequence)

(define (get-timeofday) (receive (a b) (sys-gettimeofday) (cons a b)))

(define (foldr2 binop start l1 l2)
    (if (null? l1)
        start
        (binop (car l1) (car l2) (foldr2 binop start (cdr l1) (cdr l2)))))

(define (make-valuez . obj)
  `#(valuez ,obj ,(length obj)))

(define (valuez.values obj)
  (vector-ref obj 1))

(define (valuez? obj)
  (and (vector? obj) (eq? 'valuez (vector-ref obj 0))))

(define (vm/apply proc-c args)
  (let1 code-c  `#(FRAME
                   ,(+ (length proc-c) 5)
                  CONSTANT
                  ,args
                  PUSH
                  ,@proc-c
                  APPLY
                  HALT)
    (set-closure-body-code! vm-outer-closure code-c)
    (VM code-c  0 vm-outer-closure 0 vm-outer-closure vstack 0)))

(define (apply-proc . args)
  (let* ([proc (first args)]
         [args (cdr args)]
         [adjusted_args (append
                         (take args (- (length args) 1))
                         (car (drop args (- (length args) 1))))])
     (VM `#(FRAME
           7
           CONSTANT
           ,adjusted_args
           PUSH
           CONSTANT
           ,proc
           APPLY
           HALT)
        0
        '()
        0
        '()
        (make-vector 1000)
        0)))

;(define (VM codes pc a fp c stack sp)


(define (init-library-table)
  (set! vm-instances (make-hash-table 'eq?))
  (set! vm-libraries (make-hash-table 'eq?))
  (set! vm-name-space (make-hash-table 'eq?))
  (set! top-level-library (make-empty-library '(top level)))
  (vm-import (libname->symbol '(top level))))

(load "./free-vars-decl.scm")

;; pass3 の変更のために一時的に読み込むものを変更している
(load "./compiler-vm.scm")
;(load "./compiler-vm-pass3.scm")
(load "./free-vars.scm")

(define-syntax check-sexp->iform
  (syntax-rules ()
    ((_ sexp)
     (pretty-iform (pass1/sexp->iform (quote sexp) top-level-library () #f)))))

(define-syntax eqt
  (syntax-rules ()
    ((_ a b)
     (test* (quote b) a b))))

(define debug-mode         #f)
(define debug-max-sp       0)
(define debug-vm-run-time  0)
(define debug-compile-time 0)
(define optimize?          #t)

(cond [#f
(eqt '(1 2 3) (remove-tail '(1 2 3 4) even?))
(eqt '(1 2 3 4) (remove-tail '(1 2 3 4) odd?))

(define-syntax check-sexp->iform
  (syntax-rules ()
    ((_ sexp)
     (pretty-iform (pass1/sexp->iform (quote sexp) top-level-library () #f)))))

(test-start "vm")

[check-sexp->iform
 (let ([a 0])
   0)
 ]
(eqt "($LET ((a[0 0] ($CONST 0))\n       )\n  ($CONST 0))\n" (with-output-to-string (lambda () (check-sexp->iform (let ((a 0)) 0)))))


[check-sexp->iform
 (let ([a 0])
   a)
 ]
(eqt "($LET ((a[1 0] ($CONST 0))\n       )\n  ($LOCAL-REF a[1 0]))\n" (with-output-to-string (lambda () (check-sexp->iform (let ((a 0)) a)))))


[check-sexp->iform
 (let ([a 0])
   (let ([b 0])
     a))
 ]
(eqt "($LET ((a[1 0] ($CONST 0))\n       )\n  ($LET ((b[0 0] ($CONST 0))\n         )\n    ($LOCAL-REF a[1 0])))\n" (with-output-to-string (lambda () (check-sexp->iform (let ((a 0)) (let ((b 0)) a))))))


[check-sexp->iform
 (let ([a 0])
   (let ([b 0])
     b))
 ]
(eqt "($LET ((a[0 0] ($CONST 0))\n       )\n  ($LET ((b[1 0] ($CONST 0))\n         )\n    ($LOCAL-REF b[1 0])))\n" (with-output-to-string (lambda () (check-sexp->iform (let ((a 0)) (let ((b 0)) b))))))


[check-sexp->iform
 (let ([a 0])
   (let ([b 0])
     (let ([c b])
       c)))
 ]
(eqt "($LET ((a[0 0] ($CONST 0))\n       )\n  ($LET ((b[1 0] ($CONST 0))\n         )\n    ($LET ((c[1 0] ($LOCAL-REF b[1 0]))\n           )\n      ($LOCAL-REF c[1 0]))))\n" (with-output-to-string (lambda () (check-sexp->iform (let ((a 0)) (let ((b 0)) (let ((c b)) c)))))))


[check-sexp->iform
 (let ([a 0])
   (let ([b 0])
     (let ([c b])
       b)))
 ]
(eqt "($LET ((a[0 0] ($CONST 0))\n       )\n  ($LET ((b[2 0] ($CONST 0))\n         )\n    ($LET ((c[0 0] ($LOCAL-REF b[2 0]))\n           )\n      ($LOCAL-REF b[2 0]))))\n" (with-output-to-string (lambda () (check-sexp->iform (let ((a 0)) (let ((b 0)) (let ((c b)) b)))))))


[check-sexp->iform
 (let ([a 0])
   (set! a 1))
 ]
(eqt "($LET ((a[0 1] ($CONST 0))\n       )\n  ($LOCAL-ASSIGN a[0 1]\n    ($CONST 1)))\n" (with-output-to-string (lambda () (check-sexp->iform (let ((a 0)) (set! a 1))))))

[check-sexp->iform
 (let ([a 0])
   (let ([b 1])
     (set! b 1)))
 ]
(eqt "($LET ((a[0 0] ($CONST 0))\n       )\n  ($LET ((b[0 1] ($CONST 1))\n         )\n    ($LOCAL-ASSIGN b[0 1]\n      ($CONST 1))))\n" (with-output-to-string (lambda () (check-sexp->iform (let ((a 0)) (let ((b 1)) (set! b 1)))))))

[check-sexp->iform
 (let ([a 0])
   (let ([b 1])
     (set! a 1)))
 ]
(eqt "($LET ((a[0 1] ($CONST 0))\n       )\n  ($LET ((b[0 0] ($CONST 1))\n         )\n    ($LOCAL-ASSIGN a[0 1]\n      ($CONST 1))))\n" (with-output-to-string (lambda () (check-sexp->iform (let ((a 0)) (let ((b 1)) (set! a 1)))))))


[check-sexp->iform
 (lambda (x) x)
 ]
(eqt "($LAMBDA[anonymous (x[1 0]) ()]\n  ($LOCAL-REF x[1 0]))\n" (with-output-to-string (lambda () (check-sexp->iform (lambda (x) x)))))



[check-sexp->iform
 (lambda (x)
   x
   (lambda (y)
     y))
 ]
(eqt "($LAMBDA[anonymous (x[1 0]) ()]\n  ($SEQ\n    ($LOCAL-REF x[1 0])\n    ($LAMBDA[anonymous (y[1 0]) ()]\n      ($LOCAL-REF y[1 0]))))\n" (with-output-to-string (lambda () (check-sexp->iform (lambda (x) x (lambda (y) y))))))

[check-sexp->iform
 (lambda (x)
   x
   (lambda (y)
     (set! x y)))
 ]
(eqt "($LAMBDA[anonymous (x[1 1]) ()]\n  ($SEQ\n    ($LOCAL-REF x[1 1])\n    ($LAMBDA[anonymous (y[1 0]) ()]\n      ($LOCAL-ASSIGN x[1 1]\n        ($LOCAL-REF y[1 0])))))\n" (with-output-to-string (lambda () (check-sexp->iform (lambda (x) x (lambda (y) (set! x y)))))))


[check-sexp->iform
 (if 3 4 5)
 ]
(eqt "($IF ($CONST 3)\n  ($CONST 4)\n  ($CONST 5))\n" (with-output-to-string (lambda () (check-sexp->iform (if 3 4 5)))))


[check-sexp->iform
 (if 3 4)
 ]
(eqt "($IF ($CONST 3)\n  ($CONST 4)\n  ($UNDEF))\n" (with-output-to-string (lambda () (check-sexp->iform (if 3 4)))))


[check-sexp->iform
 (if 3
     (let ([a 0])
       a))
 ]
(eqt "($IF ($CONST 3)\n  ($LET ((a[1 0] ($CONST 0))\n         )\n    ($LOCAL-REF a[1 0]))\n  ($UNDEF))\n" (with-output-to-string (lambda () (check-sexp->iform (if 3 (let ((a 0)) a))))))


[check-sexp->iform
 (cons 1 2)
 ]
(eqt "($asm CONS\n  ($CONST 1)\n  ($CONST 2))\n" (with-output-to-string (lambda () (check-sexp->iform (cons 1 2)))))

[check-sexp->iform
 (begin 3)
 ]
(eqt "($CONST 3)\n" (with-output-to-string (lambda () (check-sexp->iform (begin 3)))))


[check-sexp->iform
 (begin 3 4)
 ]
(eqt "($SEQ\n  ($CONST 3)\n  ($CONST 4))\n" (with-output-to-string (lambda () (check-sexp->iform (begin 3 4)))))


[check-sexp->iform
 (define a 3)
 ]

(eqt "($DEFINE top level :a\n  ($CONST 3))\n" (with-output-to-string (lambda () (check-sexp->iform (define a 3)))))

[check-sexp->iform
 (define (func x) x)
 ]
(eqt "($DEFINE top level :func\n  ($LAMBDA[func (x[1 0]) ()]\n    ($LOCAL-REF x[1 0])))\n" (with-output-to-string (lambda () (check-sexp->iform (define (func x) x)))))


[check-sexp->iform
 (call/cc (lambda (c) c))
 ]
(eqt "($CALL-CC ($LAMBDA[anonymous (c[1 0]) ()]\n  ($LOCAL-REF c[1 0])))\n" (with-output-to-string (lambda () (check-sexp->iform (call/cc (lambda (c) c))))))

[check-sexp->iform
 '(a b)
 ]
(eqt "($CONST (a b))\n" (with-output-to-string (lambda () (check-sexp->iform '(a b)))))


[check-sexp->iform
 (+ 1 2 3)
 ]
(eqt "($asm NUMBER_ADD\n  ($asm NUMBER_ADD\n    ($CONST 1)\n    ($CONST 2))\n  ($CONST 3))\n" (with-output-to-string (lambda () (check-sexp->iform (+ 1 2 3)))))


[check-sexp->iform
 (= 1 2 3)
 ]
(eqt "($IF ($asm NUMBER_EQUAL\n       ($CONST 1)\n       ($CONST 2))\n  ($asm NUMBER_EQUAL\n    ($CONST 2)\n    ($CONST 3))\n  ($CONST #f))\n" (with-output-to-string (lambda () (check-sexp->iform (= 1 2 3)))))


[check-sexp->iform
 (car 3)
 ]
(eqt "($asm CAR\n  ($CONST 3))\n" (with-output-to-string (lambda () (check-sexp->iform (car 3)))))


[check-sexp->iform
 func
 ]
(eqt "($GLOBAL-REF top level  func)\n" (with-output-to-string (lambda () (check-sexp->iform func))))


[check-sexp->iform
 (func 3 4)
 ]
(eqt "($call [#f]($GLOBAL-REF top level  func)\n  ($CONST 3)\n  ($CONST 4))\n" (with-output-to-string (lambda () (check-sexp->iform (func 3 4)))))

[check-sexp->iform
 (lambda (a)
   (func a))
 ]
(eqt "($LAMBDA[anonymous (a[1 0]) ()]\n  ($call[tail] [#f]($GLOBAL-REF top level  func)\n    ($LOCAL-REF a[1 0])))\n" (with-output-to-string (lambda () (check-sexp->iform (lambda (a) (func a))))))

[check-sexp->iform
 (letrec ([a 1])
   a)
 ]
(eqt "($LET ((a[1 0] ($CONST 1))\n       )\n  ($LOCAL-REF a[1 0]))\n" (with-output-to-string (lambda () (check-sexp->iform (letrec ((a 1)) a)))))

;; refer-count of a should be 1.
[check-sexp->iform
 (letrec ([a 1]
          [b (lambda () a)])
   (b))
 ]
(eqt "($LET ((a[1 0] ($CONST 1))\n       (b[1 0] ($LAMBDA[anonymous () ()]\n                 ($LOCAL-REF a[1 0])))\n       )\n  ($call [#f]($LOCAL-REF b[1 0])))\n" (with-output-to-string (lambda () (check-sexp->iform (letrec ((a 1) (b (lambda () a))) (b))))))

[check-sexp->iform
 (letrec ([a (lambda (i) (if (= i 10) i (a (+ i 1))))])
   (a 0))
 ]
(eqt "($LET ((a[2 0] ($LAMBDA[anonymous (i[3 0]) ()]\n                 ($IF ($asm NUMBER_EQUAL\n                        ($LOCAL-REF i[3 0])\n                        ($CONST 10))\n                   ($LOCAL-REF i[3 0])\n                   ($call[tail] [#f]($LOCAL-REF a[2 0])\n                     ($asm NUMBER_ADD\n                       ($LOCAL-REF i[3 0])\n                       ($CONST 1))))))\n       )\n  ($call [#f]($LOCAL-REF a[2 0])\n    ($CONST 0)))\n" (with-output-to-string (lambda () (check-sexp->iform (letrec ((a (lambda (i) (if (= i 10) i (a (+ i 1)))))) (a 0))))))


;; letrec の tail-call
[check-sexp->iform
 (letrec ([a (lambda (i) (if (= i 10) 10 (a (+ i 1))))])
   (a 0))
 ]
(eqt "($LET ((a[2 0] ($LAMBDA[anonymous (i[2 0]) ()]\n                 ($IF ($asm NUMBER_EQUAL\n                        ($LOCAL-REF i[2 0])\n                        ($CONST 10))\n                   ($CONST 10)\n                   ($call[tail] [#f]($LOCAL-REF a[2 0])\n                     ($asm NUMBER_ADD\n                       ($LOCAL-REF i[2 0])\n                       ($CONST 1))))))\n       )\n  ($call [#f]($LOCAL-REF a[2 0])\n    ($CONST 0)))\n" (with-output-to-string (lambda () (check-sexp->iform (letrec ((a (lambda (i) (if (= i 10) 10 (a (+ i 1)))))) (a 0))))))

;; pass2 tests
(define-syntax check-optimize
  (syntax-rules ()
    ((_ sexp)
     (pretty-iform (pass2/optimize (pass1/sexp->iform (pass1/expand (quote sexp)) top-level-library () #f) '())))))

[check-optimize
 (let ([a 0])
   a)
 ]
(eqt "($CONST 0)\n" (with-output-to-string (lambda () (check-optimize (let ((a 0)) a)))))


[check-optimize
 (let1 a (lambda (x) (let1 b x (+ b 1)))
   (a 3))
 ]
(eqt "($CONST 4)\n" (with-output-to-string (lambda () (check-optimize (let1 a (lambda (x) (let1 b x (+ b 1))) (a 3))))))


[check-optimize
 (let1 a (lambda () 3)
   (a)
   (a))
 ]
(eqt "($SEQ\n  ($SEQ\n    ($CONST 3))\n  ($SEQ\n    ($CONST 3)))\n" (with-output-to-string (lambda () (check-optimize (let1 a (lambda () 3) (a) (a))))))

[check-optimize
 3
 ]


[check-optimize
 (letrec ([a (lambda (i) (if (= i 10) i (a (+ i 1))))])
   (print (a 0)))
 ]
(eqt "($call [#f]($GLOBAL-REF top level  print)\n  ($call [embed]($LAMBDA[anonymous (i[3 0]) dissolved]\n           ($label #0\n             ($IF ($asm NUMBER_EQUAL\n                    ($LOCAL-REF i[3 0])\n                    ($CONST 10))\n               ($LOCAL-REF i[3 0])\n               ($call[tail] [jump]($call [embed]($LAMBDA[anonymous (i[3 0]) dissolved]\n                                     label#0)\n                              ($CONST 0))\n                 ($asm NUMBER_ADD\n                   ($LOCAL-REF i[3 0])\n                   ($CONST 1))))))\n    ($CONST 0)))\n" (with-output-to-string (lambda () (check-optimize (letrec ((a (lambda (i) (if (= i 10) i (a (+ i 1)))))) (print (a 0)))))))


(eqt "($call [embed]($LAMBDA[anonymous (i[3 0]) dissolved]\n         ($label #0\n           ($IF ($asm NUMBER_EQUAL\n                  ($LOCAL-REF i[3 0])\n                  ($CONST 10))\n             ($LOCAL-REF i[3 0])\n             ($call[tail] [jump]($call [embed]($LAMBDA[anonymous (i[3 0]) dissolved]\n                                   label#0)\n                            ($CONST 0))\n               ($asm NUMBER_ADD\n                 ($LOCAL-REF i[3 0])\n                 ($CONST 1))))))\n  ($CONST 0))\n" (with-output-to-string (lambda () (check-optimize (letrec ((a (lambda (i) (if (= i 10) i (a (+ i 1)))))) (a 0))))))

(define-syntax check-compile-no-optimize
  (syntax-rules ()
    ((_ sexp)
     (pass3 (pass1/sexp->iform (quote sexp) top-level-library () #f) '() '() '() '() #f))))

(define-syntax check-compile-optimize
  (syntax-rules ()
    ((_ sexp)
     (pass4 (pass3 (pass2/optimize (pass1/sexp->iform (quote sexp) top-level-library () #f) '()) '() '() '() '() #f)))))

;; pass3 tests with optimize
[check-compile-optimize
 (letrec ([a (lambda (i) (if (= i 10) i (a (+ i 1))))])
   (a 0))
 ]
(eqt #(LET_FRAME CONSTANT 0 PUSH ENTER NOP REFER_LOCAL 0 PUSH CONSTANT 10 NUMBER_EQUAL TEST 4 REFER_LOCAL 0 LOCAL_JMP 15 REDUCE REFER_LOCAL 0 PUSH CONSTANT 1 NUMBER_ADD PUSH SHIFT 1 1 LOCAL_JMP -23 LEAVE 1) (check-compile-optimize (letrec ((a (lambda (i) (if (= i 10) i (a (+ i 1)))))) (a 0))))

[check-compile-optimize
 (vector? #(3))
 ]
(eqt #(CONSTANT #0=#(3) VECTOR_P) (check-compile-optimize (vector? #0#)))


[check-compile-optimize
 (letrec ([a (lambda (i) (if (= i 10) i (a (+ i 1))))])
   (print (a 0)))
 ]
(eqt #(FRAME 40 LET_FRAME CONSTANT 0 PUSH ENTER NOP REFER_LOCAL 0 PUSH CONSTANT 10 NUMBER_EQUAL TEST 4 REFER_LOCAL 0 LOCAL_JMP 15 REDUCE REFER_LOCAL 0 PUSH CONSTANT 1 NUMBER_ADD PUSH SHIFT 1 1 LOCAL_JMP -23 LEAVE 1 PUSH REFER_GLOBAL |top level | print CALL 1) (check-compile-optimize (letrec ((a (lambda (i) (if (= i 10) i (a (+ i 1)))))) (print (a 0)))))

[check-compile-optimize
 (letrec ([a (lambda (i) (if (= i 10) i (a (+ i 1))))])
   (begin (a 0)))
 ]
(eqt #(LET_FRAME CONSTANT 0 PUSH ENTER NOP REFER_LOCAL 0 PUSH CONSTANT 10 NUMBER_EQUAL TEST 4 REFER_LOCAL 0 LOCAL_JMP 15 REDUCE REFER_LOCAL 0 PUSH CONSTANT 1 NUMBER_ADD PUSH SHIFT 1 1 LOCAL_JMP -23 LEAVE 1) (check-compile-optimize (letrec ((a (lambda (i) (if (= i 10) i (a (+ i 1)))))) (begin (a 0)))))


;; pass3 tests without optimize
[check-compile-no-optimize
 3
 ]
(eqt '(CONSTANT 3) (check-compile-no-optimize 3))


[check-compile-no-optimize
 (lambda () 3)
 ]
(eqt '(CLOSURE 8 0 #f 0 CONSTANT 3 RETURN 0) (check-compile-no-optimize (lambda () 3)))


[check-compile-no-optimize
 (lambda (x) x)
 ]
(eqt '(CLOSURE 8 1 #f 0 REFER_LOCAL 0 RETURN 1) (check-compile-no-optimize (lambda (x) x)))

[check-compile-no-optimize
 (lambda (x) (lambda () x))
 ]
(eqt '(CLOSURE 18 1 #f 0 REFER_LOCAL 0 PUSH CLOSURE 8 0 #f 1 REFER_FREE 0 RETURN 0 RETURN 1) (check-compile-no-optimize (lambda (x) (lambda () x))))


[check-compile-no-optimize
 (lambda (x) (set! x 3))
 ]
(eqt '(CLOSURE 12 1 #f 0 BOX 0 CONSTANT 3 ASSIGN_LOCAL 0 RETURN 1) (check-compile-no-optimize (lambda (x) (set! x 3))))


[check-compile-no-optimize
 (begin 1 2)
 ]
(eqt '(CONSTANT 1 CONSTANT 2) (check-compile-no-optimize (begin 1 2)))


[check-compile-no-optimize
 a
 ]
(eqt '(REFER_GLOBAL |top level | a) (check-compile-no-optimize a))


[check-compile-no-optimize
 (if #f 3)
 ]
(eqt '(CONSTANT #f TEST 4 CONSTANT 3 LOCAL_JMP 3 UNDEF) (check-compile-no-optimize (if #f 3)))


[check-compile-no-optimize
 (+ 1 2)
 ]
(eqt '(CONSTANT 1 PUSH CONSTANT 2 NUMBER_ADD) (check-compile-no-optimize (+ 1 2)))


[check-compile-no-optimize
 (define a 3)
 ]
(eqt '(CONSTANT 3 DEFINE_GLOBAL |top level | a) (check-compile-no-optimize (define a 3)))


[check-compile-no-optimize
 (func 3)
 ]
(eqt '(FRAME 9 CONSTANT 3 PUSH REFER_GLOBAL |top level | func CALL 1) (check-compile-no-optimize (func 3)))


[check-compile-no-optimize
 (call/cc (lambda (c) c))
 ]
(eqt '(FRAME 15 MAKE_CONTINUATION 0 PUSH CLOSURE 8 1 #f 0 REFER_LOCAL 0 RETURN 1 CALL 1) (check-compile-no-optimize (call/cc (lambda (c) c))))


[check-compile-no-optimize
 (let ([a 0]) a)
 ]
(eqt '(LET_FRAME CONSTANT 0 PUSH ENTER REFER_LOCAL 0 LEAVE 1) (check-compile-no-optimize (let ((a 0)) a)))


[check-compile-no-optimize
 (let ([a 0]) (let ([b 1]) b))
 ]
(eqt '(LET_FRAME CONSTANT 0 PUSH ENTER LET_FRAME CONSTANT 1 PUSH ENTER REFER_LOCAL 0 LEAVE 1 LEAVE 1) (check-compile-no-optimize (let ((a 0)) (let ((b 1)) b))))


[check-compile-no-optimize
 (lambda () (func 3))
 ]
(eqt '(CLOSURE 17 0 #f 0 CONSTANT 3 PUSH REFER_GLOBAL |top level | func SHIFT 1 0 CALL 1 RETURN 0) (check-compile-no-optimize (lambda () (func 3))))


])
;;--------------------------------------------------------------------
;;
;;  VM
;;
(define-syntax index
  (syntax-rules ()
    ((_ stack sp i)
     (vector-ref stack (- (- sp i) 1)))))

(define-syntax push
  (syntax-rules ()
    ((_ stack sp x)
     (begin
       (vector-set! stack sp x)
       (+ sp 1)))))

(define-syntax index-set!
  (syntax-rules ()
    ((_ stack sp i v)
     (vector-set! stack (- sp i 1) v))))

(define (save-stack stack sp)
  (let ([v (make-vector sp)])
    (let copy ([i 0])
      (unless (= i sp)
        (vector-set! v i (vector-ref stack i))
        (copy (+ i 1))))
    v))

(define (pair-args->stack stack s offset args)
  (cond ((null? args) (index-set! stack s offset '()))
        (else
         (let loop ([i (- (length args) 1)] [args args])
           (unless (null? args)
             (index-set! stack s (+ i offset) (car args))
             (loop (- i 1) (cdr args)))))))

(define (restore-stack stack v)
  (let ([s (vector-length v)])
    (let copy ([i 0])
      (unless (= i s)
        (vector-set! stack i (vector-ref v i))
        (copy (+ i 1))))
    s))

(define-syntax shift-args-to-bottom
  (syntax-rules ()
    ((_ stack sp depth diff)
     (begin
;       (print "shift-args-to-bottom start" sp)
       (let nxtarg ([i (- depth 1)])
         (unless (< i 0)
           (index-set! stack sp (+ i diff) (index stack sp i))
           (nxtarg (- i 1))))
;       (print "shift-args-to-bottom end" (- sp diff))
;       (exit)
       (- sp diff)))))

;;--------------------------------------------------------------------
;;
;; Make Clousre.
;;
;; Arguments:
;;
;;   1. body-code
;;     Code vector of body.
;;
;;   2. body-pc
;;     Offset index of body in body-code.
;;
;;   3. arg-length
;;     Length of arguments.
;;
;;   4. optional-arg?
;;     This lambda has optional arguments?
;;       (lambda a ...)       => #t
;;       (lambda (a) ...)     => #f
;;       (lambda (a . b) ...) => #t
;;
;;   5. n
;;     Number of free variables.
;;
;;   6. stack
;;     Current stack.
;;
;;   7. sp
;;     Current stack pointer.
;;
(define-syntax make-closure
  (syntax-rules ()
    ((_ body-code body-pc arg-length optional-arg? n max-stack stack sp source-info)
     ;; we don't use source-info in vm.scm.
     (let ([v (make-vector (+ n 5))])
       (vector-set! v 0 body-code)
       (vector-set! v 1 body-pc)
       (vector-set! v 2 arg-length)
       (vector-set! v 3 optional-arg?)
       (vector-set! v 4 max-stack)
       (let f ([i 0])
         (unless (= i n)
           (vector-set! v (+ i 5) (index stack sp i))
           (f (+ i 1))))
       v))))

(define-syntax make-display
  (syntax-rules ()
    ((_ n stack sp)
     (let1 v (make-vector (+ n 5))
       (let f ([i 0])
         (unless (= i n)
           (vector-set! v (+ i 5) (index stack sp i))
           (f (+ i 1))))
       v))))

(define-syntax closure-body-code
  (syntax-rules ()
    ((_ c)
     (vector-ref c 0))))

(define (set-closure-body-code! closure c)
  (vector-set! closure 0 c))

(define-syntax closure-body-pc
  (syntax-rules ()
    ((_ c)
     (vector-ref c 1))))

(define-syntax closure-arg-length
  (syntax-rules ()
    ((_ c)
     (vector-ref c 2))))

(define-syntax closure-optional-arg?
  (syntax-rules ()
    ((_ c)
     (vector-ref c 3))))

(define-syntax closure-max-stack
  (syntax-rules ()
    ((_ c)
     (vector-ref c 4))))

(define-syntax index-closure
  (syntax-rules ()
    ((_ c n)
                                        ;(define (index-closure c n)
     (vector-ref c (+ n 5)))))

(define (make-continuation stack sp n)
  (make-closure
   `#(REFER_LOCAL 0 RESTORE_CONTINUATION ,(save-stack stack sp) RETURN ,n)
   0
   1
   #f
   0
   1
   stack
   sp
   #f))

(define (dump-stack stack sp)
  (let loop ([n 0])
    (if (>= n sp)
        '()
        (begin
          (print (format "    |~d: ~s" n (vector-ref stack n)))
          (loop (+ n 1))))))

;; (define (stack->pair-args stack sp num-args)
;;   (let loop ([n (- num-args 1)]
;;              [args '()])
;;     (if (>= n 0)
;;         (begin
;;           (print "sp=" n " val=" (if (string? (index stack sp n)) (index stack sp n) 'a ))
;;           (loop (- n 1) (cons (index stack sp n) args)))
;;         args)))

(define (stack->pair-args stack sp num-args)
  (let loop ([n (- num-args 1)])
    (if (>= n 0)
        (cons (index stack sp n) (loop (- n 1)) )
        '())))


;; (define (stack->pair-args stack sp num-args)
;;   (let loop ([n 0]
;;              [args '()])
;;     (if (< n num-args)
;;         (loop (+ n 1) (cons (index stack sp n) args))
;;         args)))

(define (shift-args-to-top stack sp depth diff)
;  (format #t "sp=~d depth=~d diff=~d\n" sp depth diff)
  (let loop ((i 0))
    (cond ((< i diff)
           (index-set! stack (- (+ sp diff) i) 0 (index stack sp i))
           (loop (+ i 1)))
          (else
           (+ sp diff)))))

(define (box v)
  (cons 'BOX v))

(define (box? v)
  (and (pair? v) (eq? (car v) 'BOX)))

(define (set-box! b v)
  (set-cdr! b v))

(define (unbox b)
  (cdr b))

(define (make-procedure body)
  `(procedure . ,body))

(define-syntax procedurep
  (syntax-rules ()
    ((_ proc)
     (and (pair? proc) (eqv? 'procedure (car proc))))))

(define (procedure-body proc)
  (cdr proc))

(define regmatch-proxy
  (lambda a
    (let ([match (car a)]
          [rest  (cdr a)])
      (cond [(and (pair? rest) (eq? 'after (car rest)))
             (apply rxmatch-after (cons match (cdr rest)))]
            [(and (pair? rest) (eq? 'before (car rest)))
             (apply rxmatch-before (cons match (cdr rest)))]
            [else
             (apply rxmatch-substring a)]))))

(define undef (if #f #f))

(define-macro (debug-case val . clauses)
  `(case ,val
     ,@(map (lambda (x) `(,(car x)
                         (let1 debug-clause (quote ,(car x))
                           ,@(cdr x))))
            clauses)))

(define-macro (check-vm-paranoia pred)
  `(unless ,pred
     (error "** vm check paranoia " (quote ,pred) " on " debug-clause)))

;; (debug-case val
;;   ((a)
;;     (print debug-clause))
;;   (else
;;    'else))

(define-macro (let-frame-size) 2)

(define valuez (make-vector 100))
(define num-valuez 1)

(define (VM codes pc a fp c stack sp)
  (letrec-syntax
      ([skip (syntax-rules ()
               ((_ n)
                (+ pc n 1)))]
       [val1 (syntax-rules ()
               ((_)
                (set! num-valuez 1)))]
       [next (syntax-rules ()
               ((_ n)
                (vector-ref codes (+ n pc))))]
       [apply-native-1arg (syntax-rules ()
                            ((_ proc)
                             (begin
                               (val1)
                               (VM codes (skip 0) (proc a) fp c stack sp))))]
       [apply-native-1arg-optional (syntax-rules ()
                                     ((_ proc)
                                      (begin
                                        (val1)
                                        (VM codes (skip 0) (if (null? a) (proc) (proc a)) fp c stack sp))))]
       [apply-native-2arg (syntax-rules ()
                            ((_ proc)
                             (begin
                               (val1)
                               (VM codes (skip 0) (proc (index stack sp 0) a) fp c stack (- sp 1)))))]
       [apply-native-2arg-ac (syntax-rules ()
                               ((_ proc ac)
                                (begin
                                  (val1)
                                (VM codes (skip 0) (proc (index stack sp 0) aac) fp c stack (- sp 1)))))]
       [apply-native-2arg-push (syntax-rules ()
                                 ((_ proc)
                                  (begin
                                    (val1)
                                  (VM codes (skip 0) (proc (index stack sp 0) a) fp c stack (push stack (- sp 1) (proc (index stack sp 0) a))))))]
       [apply-native-2arg-push(syntax-rules ()
                                 ((_ proc ac)
                                  (begin
                                    (val1)
                                  (VM codes (skip 0) (proc (index stack sp 0) ac) fp c stack (push stack (- sp 1) (proc (index stack sp 0) a))))))]
       [apply-native-3arg (syntax-rules ()
                            ((_ proc)
                             (begin
                               (val1)
                               (VM codes (skip 0) (proc (index stack sp 1) (index stack sp 0) a) fp c stack (- sp 2)))))]
       [apply-native-3arg-ac (syntax-rules ()
                               ((_ proc ac)
                                (begin
                                  (val1)
                                  (VM codes (skip 0) (proc (index stack sp 1) (index stack sp 0) ac) fp c stack (- sp 2)))))]
       [return (syntax-rules ()
                 ((_ n)
                  (let ([sp (- sp n)])
                    (VM (index stack sp 0)                 ;; code
                        (index stack sp 1)                 ;; pc
                        a
                        (index stack sp 2)                 ;; fp
                        (index stack sp 3)                 ;; c
                        stack
                        (- sp 4)))))])
  (define (refer-local n)
;;     (let1 v (index stack (+ fp n 1) 0)
;;       (print "REFER_LOCAL" n "=" (if (string? v) v 'none)))
    (index stack (+ fp n 1) 0))
;    (index stack fp n))
  (define (apply-body a args-num sp)
    (cond [(procedurep a)
           (check-vm-paranoia (number? args-num))
           (VM `#(RETURN ,args-num HALT) 0 (apply (procedure-body a) (stack->pair-args stack sp args-num)) fp c stack sp)]
          [(regexp? a)
           (check-vm-paranoia (number? args-num))
           (VM `#(RETURN ,args-num HALT) 0 (apply rxmatch (cons a(stack->pair-args stack sp args-num))) fp c stack sp)]
          [(regmatch? a)
           (check-vm-paranoia (number? args-num))
           (VM `#(RETURN ,args-num HALT) 0 (apply regmatch-proxy (cons a(stack->pair-args stack sp args-num))) fp c stack sp)]
          [else
           (when (>= (+ sp (closure-max-stack a)) (vector-length stack))
             (errorf "stack over flow sp=~d" sp))
           (let ([arg-length args-num]
                 [required-length (closure-arg-length a)])
             (cond [(closure-optional-arg? a)
                    (let ([extra-num-args (- arg-length (closure-arg-length a))])
                      ;; last arg is '()
                      (cond [(= -1 extra-num-args)
                             (let ((stack-pointer (shift-args-to-top stack sp arg-length 1)))
                               (index-set! stack
                                           stack-pointer
                                           0
                                           '())
                               (VM (closure-body-code a)
                                   (closure-body-pc a)
                                   a
                                   (- stack-pointer required-length)
                                   a
                                   stack
                                   stack-pointer
                                   ))]
                            [(>= extra-num-args 0)
                             (index-set! stack
                                         sp
                                         extra-num-args
                                         (stack->pair-args stack sp (+ extra-num-args 1)))
                             (let ([stack-pointer (- sp extra-num-args)])
                               (VM (closure-body-code a)
                                   (closure-body-pc a)
                                   a
                                   (- stack-pointer required-length)
                                   a
                                   stack
                                   stack-pointer
                                   ))]
                            [else
                             (error (format "closure ~a require ~d arguments got ~d" a (closure-arg-length a) arg-length))]
                            ))]
                   [else
                    (cond [(= arg-length required-length)
                           (VM (closure-body-code a)
                               (closure-body-pc a)
                               a
                               (- sp arg-length)
                               a
                               stack
                               sp
                               )]
                          [else
                           (error (format "[2]wrong number of arguments for #<closure> (required ~d, got ~d ~a)" required-length arg-length (debug-source-info a)))])]))]))
    (cond [debug-mode
           (if (> sp debug-max-sp)
               (set! debug-max-sp sp))
           ])
    ;;--------------------------------  HALT  ------------------------------
    (cond [#f a];(>= pc len) a]
          [else
           (let1 code (vector-ref codes pc)
             (if (not (number? fp)) (error "fp error"))
             (if (not (number? sp)) (error "sp error"))
             (if (not (number? pc)) (error "pc error"))
             (if (not (vector? codes)) (error "vector error"))
             (cond [#f ;; simple dump
                    ;(dump-stack stack sp)
                    (print "========================================" code)
                                        ;                    (format #t "~a(~a) sp=~a fp=~a" code (if (number? (next 1)) (next 1) "-") sp fp)
                   ; (if (number? a) (print " a=" a) (newline))
                    ]
                   [else
                    '()])
             (debug-case code
               ;;---------------------------- HALT -------------------------------
               [(HALT) a]
               [(VALUES)
                ;; values stack layout
                ;;   (value 'a 'b 'c 'd)
                ;;   ==>
                ;;   =====
                ;;     a
                ;;   =====
                ;;     b
                ;;   =====
                ;;     c    [a-reg] = d
                ;;   =====
                ;;
                ;; values are stored in [valuez vector] and [a-reg] like following.
                ;; #(b c d)
                ;; [a-reg] = a
                (let1 n-args (next 1)
                  (set! num-valuez n-args)
                  (let loop ([i (- n-args 1)]
                             [val a])
                    (if (>= 0 i)
                        (begin
;;                           ;; debug
;;                           (let dump ([j 0])
;;                             (if (>= j (- n-args 1))
;;                                 (format #t "areg=~a" val)
;;                                 (begin (format #t "valuez[~d]=~a\n" j (vector-ref valuez j))
;;                                        (dump (+ j 1)))))
                          (VM codes (skip 1) val fp c stack (- sp n-args -1))
                          )
                        (begin
;                          (format #t "set valuez[~d]=~a\n" (- i 1) val)
                          (vector-set! valuez (- i 1) val)
                          (loop (- i 1) (index stack sp (- n-args i 1)))))))]
               [(RECEIVE)
                (let ([reqargs (next 1)]
                      [optarg  (next 2)])
                  (cond
                   [(< num-valuez reqargs)
                    (error "received fewer values than expected")]
                   [(and (zero? optarg) (> num-valuez reqargs))
                    (error "received more values than expected")]
                   [else
                    (cond
                     ;; (receive (a b c) ...)
                     [(zero? optarg)
                      (when (> reqargs 0)
                        (let loop ([i      0]
                                   [new-sp (push stack sp a)])
                          (if (>= i (- reqargs 1))
                              '()
                              (loop (+ i 1) (push stack new-sp (vector-ref valuez i))))))]
                     ;; (receive a ...)
                     [(zero? reqargs)
                      (let loop ([ret `(,a)]
                                 [i   0])
                        (if (>= i (- num-valuez 1))
                            (push stack sp ret)
                            (loop (append ret (list (vector-ref valuez i)))
                                  (+ i 1))))]
                     ;; (receive (a b . c) ...)
                     [else
                      (let loop ([i      0]
                                 [new-sp (push stack sp a)]
                                 [ret    '()])
                          (cond
                           [(< i (- reqargs 1)) ;; push a, b
                            (loop (+ i 1) (push stack new-sp (vector-ref valuez i)) ret)]
                           [(< i (- num-valuez 1))
                            (loop (+ i 1) new-sp (append ret (list (vector-ref valuez i))))]
                           [else
                            (push stack new-sp ret)]))])
                      (VM codes
                          (skip 2)
                          a
                          fp
                          c
                          stack
                          (+ sp reqargs optarg))]))]
               ;;---------------------------- CLOSURE ----------------------------
               [(CLOSURE)
                (check-vm-paranoia (number? (skip (next 1))))
                (check-vm-paranoia (number? (next 2)))
                (val1)
                (VM codes
                    (skip (next 1))
                    (make-closure codes (skip 6) (next 2) (next 3) (next 4) (next 5) stack sp (next 6))
                    fp
                    c
                    stack
                    (- sp (next 4))
                    )]
               ;;---------------------------- FRAME ------------------------------
               [(FRAME)
                (VM codes (skip 1) a fp c stack
                    (push stack (push stack (push stack (push stack sp c) fp) (skip (next 1))) codes))]
               [(PUSH_FRAME)
                (VM codes (skip 1) a fp c stack
                    (push stack (push stack (push stack (push stack (push stack sp a) c) fp) (skip (next 1))) codes))]

               ;;---------------------------- DISPLAY ----------------------------
               ;;
               ;;  Create display for free variable access.
               ;;  display has same structure as closure.
               ;;
               [(DISPLAY)
                (VM codes
                    (skip 1)
                    a
                    fp
                    (make-display (next 1) stack sp)
                    stack
                    (- sp (next 1))
                    )]
               ;;---------------------------- LET_FRAME---------------------------
               ;;
               ;;  Create frame for let.
               ;;  The following structure is an over view of the Frame structure.
               ;;
               ;;  When ENTER, we update fp.
               ;;
               ;;  ======================
               ;;           fp
               ;;  ======================
               ;;           c
               ;;  ======================
               ;;
               [(LET_FRAME)
                (VM codes (skip 0) a fp c stack
                    (push stack (push stack sp fp) c))]
               ;;---------------------------- PUSH ---------------------------
               [(PUSH)
                (VM codes (skip 0) a fp c stack (push stack sp a))]
               ;;---------------------------- CONSTANT_PUSH ------------------
               [(CONSTANT_PUSH)
;                (print "CONSTANT_PUSH:"  (next 1))
                (VM codes (skip 1) (next 1) fp c stack (push stack sp (next 1)))]
               ;;---------------------------- CONSTANT_PUSH ------------------
               [(PUSH_CONSTANT)
;                (print "PUSH_CONSTANT " (next 1))
                (VM codes (skip 1) (next 1) fp c stack (push stack sp a))]
               ;;---------------------------- ENTER ------------------------------
               [(ENTER)
                (VM codes (skip 1) a (- sp (next 1)) c stack sp)]
               ;;---------------------------- ENTER ------------------------------
               [(PUSH_ENTER)
                (let1 sp (push stack sp a)
                  (VM codes (skip 1) a (- sp (next 1)) c stack sp))]
               ;;---------------------------- LEAVE ------------------------------
               ;;
               ;;  Remove "let frame" from stack.
               ;;  We change fp and display pointer.
               ;;
               [(LEAVE)
                (let ([sp (- sp (next 1))])
                  (check-vm-paranoia (number? (index stack sp 1)))
                  (check-vm-paranoia (vector? (index stack sp 0)))
                  (VM codes
                      (skip 1)
                      a
                      (index stack sp 1) ;; fp
                      (index stack sp 0) ;; display
                      stack
                      (- sp 2) ;; size of "let frame"
                      ))]
               [(LEAVE1)
                (let ([sp (- sp 1)])
                  (VM codes
                      (skip 0)
                      a
                      (index stack sp 1) ;; fp
                      (index stack sp 0) ;; display
                      stack
                      (- sp 2) ;; size of "let frame"
                      ))]
               ;;---------------------------- REFER_LOCAL ----------------------
               [(REFER_LOCAL)
                (check-vm-paranoia (number? (next 1)))
                (val1)
                (VM codes (skip 1) (refer-local (next 1)) fp c stack sp)]
               ;;---------------------------- REFER_LOCAL0 ----------------------
               [(REFER_LOCAL0)
                (val1)
                (VM codes (skip 0) (refer-local 0) fp c stack sp)]
               [(REFER_LOCAL1)
                (val1)
                (VM codes (skip 0) (refer-local 1) fp c stack sp)]
               [(REFER_LOCAL2)
                (val1)
                (VM codes (skip 0) (refer-local 2) fp c stack sp)]
               [(REFER_LOCAL3)
                (val1)
                (VM codes (skip 0) (refer-local 3) fp c stack sp)]
               [(REFER_LOCAL0_EQV_TEST)
                (val1)
                (if (eqv? (index stack sp 0) (refer-local 0))
                    (VM codes (skip 1) a fp c stack (- sp 1))
                    (VM codes (skip (next 1)) a fp c stack (- sp 1)))]
                [(REFER_LOCAL0_PUSH)
                 (val1)
                 (VM codes (skip 0) a fp c stack (push stack sp (refer-local 0)))]

;;                 [(REFER_LOCAL0_PUSH_DISPLAY)
;;                  (let1 sp (push stack sp (index stack fp 0))
;;                  (VM codes (skip 1) a fp (make-display (next 1) stack sp) stack (- sp (next 1))))]
;;                ;;---------------------------- REFER_LOCAL1 ---------------------
;;                [(REFER_LOCAL1)
;;                 (VM codes (skip 0) (index stack fp 1) fp c stack sp)]
;;                ;;---------------------------- REFER_LOCAL2 ---------------------
;;                [(REFER_LOCAL2)
;;                 (VM codes (skip 0) (index stack fp 2) fp c stack sp)]
;;                ;;---------------------------- REFER_LOCAL_PUSH -----------------
               [(REFER_LOCAL_PUSH)
                (val1)
                (VM codes (skip 1) a fp c stack (push stack sp (refer-local (next 1))))]
;;                ;;---------------------------- REFER_LOCAL0_PUSH ----------------
;;                [(REFER_LOCAL0_PUSH)
;;                 (VM codes (skip 0) a fp c stack (push stack sp (index stack fp 0)))]
               [(REFER_LOCAL0_PUSH_CONSTANT)
                (val1)
;                (print "REFER_LOCAL0_PUSH_CONSTANT " (next 1))
                (VM codes (skip 1) (next 1) fp c stack (push stack sp (refer-local 0)))]
               [(REFER_LOCAL1_PUSH_CONSTANT)
                (val1)
                (VM codes (skip 1) (next 1) fp c stack (push stack sp (refer-local 1)))]
;;                ;;---------------------------- REFER_LOCAL1_PUSH ----------------
               [(REFER_LOCAL1_PUSH)
                (val1)
                (VM codes (skip 0) a fp c stack (push stack sp (refer-local 1)))]
               [(REFER_LOCAL2_PUSH)
                (val1)
                (VM codes (skip 0) a fp c stack (push stack sp (refer-local 2)))]

;;                ;;---------------------------- REFER_LOCAL2_PUSH ----------------
;;                [(REFER_LOCAL2_PUSH)
;;                 (VM codes (skip 0) a fp c stack (push stack sp (index stack fp 2)))]
               ;;---------------------------- REFER_FREE -----------------------
               [(REFER_FREE)
                (val1)
                (check-vm-paranoia (vector? c))
                (check-vm-paranoia (number? (next 1)))
;                (print "refer free" (index-closure c (next 1)))
                (VM codes (skip 1) (index-closure c (next 1)) fp c stack sp)]
               [(REFER_FREE0)
                (val1)
                (VM codes (skip 0) (index-closure c 0) fp c stack sp)]
               [(REFER_FREE1)
                (val1)
                (VM codes (skip 0) (index-closure c 1) fp c stack sp)]
               [(REFER_FREE2)
                (val1)
                (VM codes (skip 0) (index-closure c 2) fp c stack sp)]
               [(REFER_FREE3)
                (val1)
                (VM codes (skip 0) (index-closure c 3) fp c stack sp)]
               [(PUSH_REFER_FREE0)
                (val1)
                (VM codes (skip 0) (index-closure c 0) fp c stack (push stack sp (index-closure c 0)))]
               ;;---------------------------- REFER_FREE_PUSH ------------------
               [(REFER_FREE_PUSH)
                (val1)
                (VM codes (skip 1) a fp c stack (push stack sp (index-closure c (next 1))))]
               [(REFER_FREE0_PUSH)
                (val1)
                (VM codes (skip 0) a fp c stack (push stack sp (index-closure c 0)))]
               [(REFER_FREE1_PUSH)
                (val1)
                (VM codes (skip 0) a fp c stack (push stack sp (index-closure c 1)))]
               [(REFER_FREE2_PUSH)
                (val1)
                (VM codes (skip 0) a fp c stack (push stack sp (index-closure c 2)))]

               ;;---------------------------- NOP ------------------------------
               [(NOP)
                (VM codes (skip 0) a fp c stack sp)]
               ;;---------------------------- REDUCE ---------------------------
               ;; reduce sp to fp
               [(REDUCE)
                (VM codes (skip 1) a fp c stack (+ fp (next 1)))]
               ;;---------------------------- CALL ------------------------------
               [(CALL)
                (val1)
                (apply-body a (next 1) sp)]
               [(CALL1)
                (val1)
                (apply-body a 1 sp)]
               [(CALL2)
                (val1)
                (apply-body a 2 sp)]
               [(CALL3)
                (val1)
                (apply-body a 3 sp)]
               [(PUSH_REFER_GLOBAL_CALL1)
                (val1)
                (let* ([lib (next 1)]
                       [new-sp (push stack sp a)]
                       [v (refer-global lib)])
                  (apply-body v 1 new-sp))]
               [(REFER_GLOBAL_CALL1)
                (val1)
                (apply-body (refer-global (next 1)) 1 sp)]
               [(REFER_GLOBAL_CALL)
                (val1)
                (apply-body (refer-global (next 1)) (next 2) sp)]
               ;;---------------------------- APPLY ----------------------------
               [(APPLY) ;; (apply proc args)
                (val1)
                (let1 args (index stack sp 0)
                  (cond
                   [(null? args) ;; (aplly proc '()). proc takes no argument.
                    (VM `#(CALL 0 HALT) 0 a fp c stack (- sp 1))]
                   [(valuez? args) ;; (call-with-values ...)
                    (let* ([args (valuez.values args)]
                           [len (length args)]
                           [shift-len (if (> len 1) (- len 1) 0)]
                           [new-sp (shift-args-to-top stack sp 0 shift-len)])
                      (if (and (not (procedurep a))
                               (not (regexp? a))
                               (not (regmatch? a))
                               (not (closure-optional-arg? a))
                               (not (= len (closure-arg-length a))))
                          (errorf "Values received ~a values than expected" (if (> len (closure-arg-length a)) "more" "fewer")))
                      (pair-args->stack stack new-sp 0 args)
                      (VM `#(CALL, len HALT) 0 a fp c stack new-sp))]
                   [else
                    (let* ([len (length args)]
                           [shift-len (if (> len 1) (- len 1) 0)]
                           [new-sp (+ sp shift-len)]); 正しい?(begin (format #t "sp=~d shift-len=~d\n" sp shift-len)(shift-args-to-top stack sp 0 shift-len))])
                      (pair-args->stack stack new-sp 0 args)
                      (VM `#(CALL, len HALT) 0 a fp c stack new-sp))]))]
               ;;---------------------------- LIST -----------------------------
               [(LIST)
                (val1)
                (let1 n (next 1)
                  (VM codes (skip 1)
                      (let loop ([i 0]
                                 [ret '()])
                        (if (= n i)
                            ret
                            (loop (+ i 1) (cons (index stack sp i) ret))))
                      fp c stack (- sp n)))]
               ;;---------------------------- RETURN -----------------------------
               [(RETURN)
                (return (next 1))]
;;                 (let ([sp (- sp (next 1))])
;;                   (check-vm-paranoia (vector? (index stack sp 0)))
;;                   (check-vm-paranoia (number? (index stack sp 1)))
;;                   (check-vm-paranoia (number? (index stack sp 2)))
;;                   (check-vm-paranoia (vector? (index stack sp 3)))
;;                   (VM (index stack sp 0)                 ;; code
;;                       (index stack sp 1)                 ;; pc
;;                       a
;;                       (index stack sp 2)                 ;; fp
;;                       (index stack sp 3)                 ;; c
;;                       stack
;;                       (- sp 4)))]
               [(RETURN1)
                (return 1)]
               [(RETURN2)
                (return 2)]
               [(RETURN3)
                (return 3)]

               ;;---------------------------- SHIFT ------------------------------
               [(SHIFT)
                (VM codes (skip 2) a fp c stack (shift-args-to-bottom stack sp (next 1) (next 2)))]
               [(SHIFT_CALL)
                (let1 sp (shift-args-to-bottom stack sp (next 1) (next 2))
                  (apply-body a (next 3) sp))]
               ;;---------------------------- MAKE_CONTINUATION ------------------
               [(MAKE_CONTINUATION)
                (val1)
                (VM codes (skip 1) (make-continuation stack sp (next 1)) fp c stack sp)]
               ;;---------------------------- RESTORE_CONTINUATION ---------------
               [(RESTORE_CONTINUATION)
                (VM codes (skip 1) a fp c stack (restore-stack stack (next 1)))]
               ;;---------------------------- CONSTANT ---------------------------
               [(CONSTANT)
                (val1)
                (VM codes (skip 1) (next 1) fp c stack sp)]
               ;;---------------------------- TEST  ------------------------------
               [(TEST)
                (val1)
                (if a
                    (VM codes (skip 1) a fp c stack sp)
                    (VM codes (skip (next 1)) a fp c stack sp))]
               [(NUMBER_LE_TEST)
                (val1)
                (let1 val (<= (index stack sp 0) a)
                (if val
                    (VM codes (skip 1) val fp c stack (- sp 1))
                    (VM codes (skip (next 1)) val fp c stack (- sp 1))))]
               [(NOT_TEST)
                (val1)
                (let1 val (not a)
                (if val
                    (VM codes (skip 1) val fp c stack sp)
                    (VM codes (skip (next 1)) val fp c stack sp)))]


;;                ;;---------------------------- BRANCH_NULLP  ----------------------
;;                [(BRANCH_NULLP)
;;                 (if (null? a)
;;                     (VM codes (skip 1) a fp c stack sp)
;;                     (VM codes (skip (+ (next 1) 1)) a fp c stack sp))]
;;                ;;---------------------------- BRANCH_PAIRP  ----------------------
;;                [(BRANCH_PAIRP)
;;                 (if (pair? a)
;;                     (VM codes (skip 1) a fp c stack sp)
;;                     (VM codes (skip (+ (next 1) 1)) a fp c stack sp))]
;;                ;;---------------------------- BRANCH_EQ  -------------------------
;;                [(BRANCH_EQ)
;;                 (if (eq? (index stack sp 0) a)
;;                     (VM codes (skip 1) a fp c stack (- sp 1))
;;                     (VM codes (skip (+ (next 1) 1)) a fp c stack (- sp 1)))]
;;                ;;---------------------------- BRANCH_NUMBER_EQUAL  ---------------
;;                [(BRANCH_NUMBER_EQUAL)
;;                 (if (= (index stack sp 0) a)
;;                     (VM codes (skip 1) a fp c stack (- sp 1))
;;                     (VM codes (skip (+ (next 1) 1)) a fp c stack (- sp 1)))]
;;                ;;---------------------------- BRANCH_NUMBER_GT  ------------------
;;                [(BRANCH_NUMBER_GT)
;;                 (if (> (index stack sp 0) a)
;;                     (VM codes (skip 1) a fp c stack (- sp 1))
;;                     (VM codes (skip (+ (next 1) 1)) a fp c stack (- sp 1)))]
;;                ;;---------------------------- BRANCH_NUMBER_GE  ------------------
;;                [(BRANCH_NUMBER_GE)
;;                 (if (>= (index stack sp 0) a)
;;                     (VM codes (skip 1) a fp c stack (- sp 1))
;;                     (VM codes (skip (+ (next 1) 1)) a fp c stack (- sp 1)))]
;;                ;;---------------------------- BRANCH_NUMBER_LT  ------------------
;;                [(BRANCH_NUMBER_LT)
;;                 (if (< (index stack sp 0) a)
;;                     (VM codes (skip 1) a fp c stack (- sp 1))
;;                     (VM codes (skip (+ (next 1) 1)) a fp c stack (- sp 1)))]
;;                ;;---------------------------- BRANCH_NUMBER_LE  ------------------
;;                [(BRANCH_NUMBER_LE)
;;                 (if (<= (index stack sp 0) a)
;;                     (VM codes (skip 1) a fp c stack (- sp 1))
;;                     (VM codes (skip (+ (next 1) 1)) a fp c stack (- sp 1)))]
               ;;---------------------------- LOCAL_JMP  -------------------------
               [(LOCAL_JMP)
                (VM codes (+ pc (next 1) 1) a fp c stack sp)]
               ;;---------------------------- BOX  -------------------------------
               [(BOX)
                (index-set! stack sp (next 1) (box (index stack sp (next 1))))
                (VM codes (skip 1) a fp c stack sp)]
               ;;---------------------------- INDIRECT  --------------------------
               [(INDIRECT)
                (val1)
                (VM codes (skip 0) (unbox a) fp c stack sp)]
               [(REFER_FREE0_INDIRECT)
                (val1)
                (VM codes (skip 0) (unbox (index-closure c 0)) fp c stack sp)]
               [(REFER_FREE1_INDIRECT)
                (val1)
                (VM codes (skip 0) (unbox (index-closure c 1)) fp c stack sp)]
               ;;---------------------------- ASSIGN_BIND  -----------------------
               [(ASSIGN_LOCAL)
                (set-box! (refer-local (next 1)) a)
                (VM codes (skip 1) a fp c stack sp)]
               ;;---------------------------- ASSIGN_FREE  -----------------------
               [(ASSIGN_FREE)
                (set-box! (index-closure c (next 1)) a)
                (VM codes (skip 1) a fp c stack sp)]
               ;;---------------------------- DEFINE_GLOBAL  ---------------------
               [(DEFINE_GLOBAL)
                (define-global (next 1) a)
                (VM codes (skip 1) a fp c stack sp)]
               ;;---------------------------- LIBRARY --------------------------
               [(LIBRARY)
                (hashtable-set! vm-libraries (next 1) (next 2))
                (VM codes (skip 2) a fp c stack sp)]
               ;;---------------------------- IMPORT -----------------------------
               [(IMPORT)
                (cond
                 [(fetch-instance (next 1))
                  (VM `#(RETURN 0 HALT) 0 a fp c stack sp)]
                 [else
                  (vm-import (next 1))
                  (let1 lib (hash-table-get vm-libraries (next 1))
;                    ($library.set-macro! lib (make-hash-table 'eq?))
                    (unless ($library.compiled-body lib)
                      (compile-library-body! lib))
                    (VM ($library.compiled-body lib) 0 a fp c stack sp))])]
               ;;---------------------------- REFER_GLOBAL  ----------------------
               [(REFER_GLOBAL)
                (val1)
                (VM codes (skip 1) (refer-global (next 1)) fp c stack sp)]
               ;;---------------------------- ASSIGN_GLOBAL  ---------------------
               [(ASSIGN_GLOBAL)
                (assign-global (next 1) a)
                (VM codes (skip 1) a fp c stack sp)]
               ;;---------------------------- Arithmetic  ------------------------
               [(NUMBER_EQUAL) (apply-native-2arg =)]
               [(NUMBER_GE)    (apply-native-2arg >=)]
               [(NUMBER_LE)    (apply-native-2arg <=)]
               [(NUMBER_GT)    (apply-native-2arg >)]
               [(NUMBER_LT)    (apply-native-2arg <)]
               [(NUMBER_ADD)   (apply-native-2arg +)]
               [(NUMBER_DIV)   (apply-native-2arg /)]
               [(NUMBER_SUB)   (apply-native-2arg -)]
               [(NUMBER_SUB_PUSH) (apply-native-2arg-push -)]
               [(NUMBER_ADD_PUSH) (apply-native-2arg-push +)]
               [(REFER_LOCAL0_NUMBER_ADD_PUSH) (apply-native-2arg-push-a + (refer-local 0))]
               [(NUMBER_MUL)   (apply-native-2arg *)]
               ;;---------------------------- Pair  ------------------------------
               [(PAIR_P)  (apply-native-1arg pair?)]
               [(NULL_P)  (apply-native-1arg null?)]
               [(CONS)    (apply-native-2arg cons)]
               [(CAR)     (apply-native-1arg car)]
               [(CDR)     (apply-native-1arg cdr)]
               [(CAAR)    (apply-native-1arg caar)]
               [(CADR)    (apply-native-1arg cadr)]
               [(CDAR)    (apply-native-1arg cdar)]
               [(CDDR)    (apply-native-1arg cddr)]
               [(SET_CAR) (apply-native-2arg set-car!)]
               [(SET_CDR) (apply-native-2arg set-cdr!)]
;               [(APPEND)  (apply-native-2arg append)]
               [(CAR_PUSH)  (VM codes (skip 0) a fp c stack (push stack sp (car a)))]
               [(CDR_PUSH)  (VM codes (skip 0) a fp c stack (push stack sp (cdr a)))]
               ;;---------------------------- Vector  ----------------------------
               [(VECTOR_P)      (apply-native-1arg vector?)]
               [(MAKE_VECTOR)   (apply-native-2arg make-vector)]
               [(VECTOR_LENGTH) (apply-native-1arg vector-length)]
               [(VECTOR_REF)    (apply-native-2arg vector-ref)]
               [(VECTOR_SET)    (apply-native-3arg vector-set!)]
               [(REFER_LOCAL0_VECTOR_SET (apply-native-3arg-ac (index fp 0) vector-ref))]
               [(REFER_LOCAL0_VECTOR_REF (apply-native-2arg-ac (index fp 0) vector-ref))]
               ;;---------------------------- Port  ------------------------------
               [(READ)            (apply-native-1arg-optional read)]
               [(READ_CHAR)       (apply-native-1arg-optional read-char)]
               [(NOT)             (apply-native-1arg not)]
               [(SYMBOL_P)        (apply-native-1arg symbol?)]
               [(EQ)              (apply-native-2arg eq?)]
               [(EQV)             (apply-native-2arg eqv?)]
               [(EQUAL)           (apply-native-2arg equal?)]
               ;;---------------------------- UNDEF ------------------------------
               [(UNDEF)
                (val1)
                (VM codes (skip 0) undef fp c stack sp)]
               [else
                (error "unknown instruction on vm:" code)]))])))



(define (define-global lib-id val)
  (if (hash-table-get vm-name-space lib-id #f)
      '();      (errorf "~a defined twice" lib-id)
      (hashtable-set! vm-name-space lib-id val)))

(define (refer-global lib-id)
  (aif (hash-table-get vm-name-space lib-id #f)
       it
       (errorf "unbound variable ~a" lib-id)))

(define (assign-global lib-id val)
  (if (eq? (hash-table-get vm-name-space lib-id 'notfound) 'notfound)
      (errorf "can not set! to unbound variable ~a" lib-id)
      (hashtable-set! vm-name-space lib-id val)))
       

(define (vm-import lib)
  (if (fetch-instance lib)
      '() ; already imported
      (hashtable-set! vm-instances lib (make-hash-table 'eq?))))

;; Store bound variables as (libname . (name . val)).
;; Not depend on compiler.
(define vm-instances (make-hash-table 'eq?))

;; Store (libname . $library).
;; Not depend on compiler.
(define vm-libraries (make-hash-table 'eq?))

;; Not depend on compiler.
(define vm-name-space (make-hash-table 'eq?))

(define (vm-lookup sym)
  (hash-table-get vm-name-space sym #f))

(define (fetch-instance lib)
  (hash-table-get vm-instances lib #f))

(define vstack '())
(define vm-outer-closure '())
(define command-line-args '())
(define (vm-init args)
  (init-library-table)
  (set! vstack (make-vector 10000))
  (define-global (merge-libname-sym (libname->symbol '(top level)) '*command-line-args*) args)
  (let loop ([i 0]
             [sp  (- (length *free-vars*) 1)])
    (cond [(>= sp 0)
           (push vstack sp (make-procedure (second (list-ref *free-vars* i))))
           (loop (+ i 1) (- sp 1))]
          [else
           (set! vm-outer-closure (make-closure '() 0 0 #f (length *free-vars*) 0 vstack (length *free-vars*) #f))])))



;; for eval whole buffer
(vm-init '())

(define (time-diff s sm e em)
  (- (+ (* e 1000000) em) (+ (* s 1000000) sm)))

(define (load-file file)
  (with-input-from-file file
    (lambda ()
      (let loop ([obj (read)])
        (cond [(eof-object? obj) '()]
              [else
               (evaluate obj)
               (loop (read))])))))

(define (compile-string s)
  (with-input-from-string s
    (lambda ()
      (compile (read)))))

;; return compiled code as list. label is not fixed up yet.
;; (define (compile-partial sexp . lib)
;;   (pass3 (pass2/optimize (pass1/sexp->iform #?= (pass1/expand sexp) (if (null? lib) top-level-library (car lib)) '() #f) '()) '() *primitive-procs-lvars* '() '() #f))



(define (print-insn v start end indent)
  (define (ref i)
    (vector-ref v (+ start i)))
  (define (next len)
    (print-insn v (+ start len) end indent))
  (define (print-space n)
    (let loop ([i 0])
      (if (>= i n)
          '()
          (begin (display " ") (loop (+ i 1))))))
  (cond
   [(>= end start)
    (case (ref 0)
      [(SHIFT)
       (print-space indent)
       (print (format "SHIFT(~d, ~d)" (ref 1) (ref 2)))
       (next 3)]
      [(PUSH)
       (print-space indent)
       (print "PUSH")
       (next 1)]
      [(INDIRECT)
       (print-space indent)
       (print "INDIRECT")
       (next 1)]
      [(NUMBER_GE)
       (print-space indent)
       (print "NUMBER_GE")
       (next 1)]
      [(REFER_LOCAL)
       (print-space indent)
       (print (format "REFER_LOCAL(~a, ~a)" (ref 1) (ref 2)))
       (next 3)]
      [(ASSIGN_LOCAL)
       (print-space indent)
       (print (format "ASSIGN_LOCAL(~a, ~a)" (ref 1) (ref 2)))
       (next 3)]
      [(CLOSURE)
       (print-space indent)
       (print
        (format "CLOSURE(arg-length => ~d, optional-arg? => ~a, free-variables => ~d)" (ref 2) (ref 3) (ref 4)))
       (print-insn v (+ start 5) (+ start (ref 1)) (+ 2 indent))
       (next (+ 1 (ref 1)))]
      [(NUMBER_ADD)
       (print-space indent)
       (print "NUMBER_ADD")
       (next 1)]
      [(NUMBER_EQUAL)
       (print-space indent)
       (print "NUMBER_EQUAL")
       (next 1)]
      [(ENTER)
       (print-space indent)
       (print (format "ENTER(~a)" (ref 1)))
       (print-insn v (+ start 2) end (+ indent 2))]
      [(LEAVE)
       (print-space (- indent 2))
       (print "LEAVE")
       (print-insn v (+ start 1) end (- indent 2))]
      [else
       (print-space indent)
       (print (format "~a(~a)" (ref 0) (ref 1)))
       (next 2)])]))

(define (pretty-print v)
  (print "\n\n\n================================================")
  (print-insn v 0 (- (vector-length v) 1) 2)
  (print "================================================\n\n\n")
  )

(define (compile-file file)
  (with-input-from-file file
    (lambda ()
      (let loop ((obj (read)))
        (cond ((eof-object? obj) '())
              (else
               (print (compile obj))
               (display "\n")
               (loop (read))))))))

(define *free-lvars* ($map1 (lambda (p) ($lvar p '() 0 0)) ($map1 car *free-vars*)))


(if debug-mode
    (define (evaluate code)
      (receive (t1 tm1) (sys-gettimeofday)
        (let1 code-c ((if optimize? compile compile-no-optimize) code)
          (receive (t2 tm2) (sys-gettimeofday)
            (set-closure-body-code! vm-outer-closure code-c)
            (VM code-c  0 vm-outer-closure 0 vm-outer-closure vstack 0)
            (receive (t3 tm3) (sys-gettimeofday)
              (set! debug-compile-time (+ debug-compile-time (time-diff t1 tm1 t2 tm2)))
              (set! debug-vm-run-time  (+ debug-vm-run-time  (time-diff t2 tm2 t3 tm3))))))))
    (define (evaluate code)
      (let1 code-c ((if optimize? compile compile-no-optimize) code)
;      (let1 code-c ((if optimize? compile compile-no-optimize) code)
;        (print "====> code-c=>" code-c)
        (set-closure-body-code! vm-outer-closure code-c)
        (VM code-c  0 vm-outer-closure 0 vm-outer-closure vstack 0))))

(cond [#f
;; tail call 1
(compile-no-optimize '((lambda () ((lambda () 3)))))
(eqt #(FRAME 24 CLOSURE 20 0 #f 0 CLOSURE 8 0 #f 0 CONSTANT 3 RETURN 0 SHIFT 0 0 CALL 0 RETURN 0 CALL 0) (compile-no-optimize '((lambda () ((lambda () 3))))))

(compile '((lambda () ((lambda () 3)))))
(eqt #(CONSTANT 3) (compile '((lambda () ((lambda () 3))))))


;; tail call 2
(compile-no-optimize '((lambda () ((lambda (x) x) 3))))
(eqt #(FRAME 27 CLOSURE 23 0 #f 0 CONSTANT 3 PUSH CLOSURE 8 1 #f 0 REFER_LOCAL 0 RETURN 1 SHIFT 1 0 CALL 1 RETURN 0 CALL 0) (compile-no-optimize '((lambda () ((lambda (x) x) 3)))))

(compile '((lambda () ((lambda (x) x) 3))))
(eqt #(CONSTANT 3) (compile '((lambda () ((lambda (x) x) 3)))))


;; tail call 3
(compile-no-optimize '((lambda (y) ((lambda (x) x) 3)) 4))
(eqt #(FRAME 30 CONSTANT 4 PUSH CLOSURE 23 1 #f 0 CONSTANT 3 PUSH CLOSURE 8 1 #f 0 REFER_LOCAL 0 RETURN 1 SHIFT 1 1 CALL 1 RETURN 1 CALL 1) (compile-no-optimize '((lambda (y) ((lambda (x) x) 3)) 4)))

(compile '((lambda (y) ((lambda (x) x) 3)) 4))
(eqt #(CONSTANT 3) (compile '((lambda (y) ((lambda (x) x) 3)) 4)))


;; tail call 4
(compile-no-optimize '((lambda () (let1 a 1 ((lambda () 3))))))
(eqt #(FRAME 31 CLOSURE 27 0 #f 0 LET_FRAME CONSTANT 1 PUSH ENTER CLOSURE 8 0 #f 0 CONSTANT 3 RETURN 0 SHIFT 0 3 CALL 0 LEAVE 1 RETURN 0 CALL 0) (compile-no-optimize '((lambda () (let1 a 1 ((lambda () 3)))))))

(compile '((lambda () (let1 a 1 ((lambda () 3))))))
(eqt #(CONSTANT 3) (compile '((lambda () (let1 a 1 ((lambda () 3)))))))


;; tail call5
(compile '((lambda () (let1 b 2 (let1 a 1 ((lambda () 3)))))))
(eqt #(CONSTANT 3) (compile '((lambda () (let1 b 2 (let1 a 1 ((lambda () 3))))))))


(compile-no-optimize '((lambda () (let1 b 2 (let1 a 1 ((lambda () 3)))))))
(eqt #(FRAME 38 CLOSURE 34 0 #f 0 LET_FRAME CONSTANT 2 PUSH ENTER LET_FRAME CONSTANT 1 PUSH ENTER CLOSURE 8 0 #f 0 CONSTANT 3 RETURN 0 SHIFT 0 6 CALL 0 LEAVE 1 LEAVE 1 RETURN 0 CALL 0) (compile-no-optimize '((lambda () (let1 b 2 (let1 a 1 ((lambda () 3))))))))


;; tail call6
(compile '((lambda () (if 3 ((lambda () 3))))))
(eqt #(CONSTANT 3 TEST 4 CONSTANT 3 LOCAL_JMP 3 UNDEF) (compile '((lambda () (if 3 ((lambda () 3)))))))


(compile-no-optimize '((lambda () (if 3 ((lambda () 3))))))
(eqt #(FRAME 31 CLOSURE 27 0 #f 0 CONSTANT 3 TEST 16 CLOSURE 8 0 #f 0 CONSTANT 3 RETURN 0 SHIFT 0 0 CALL 0 LOCAL_JMP 3 UNDEF RETURN 0 CALL 0) (compile-no-optimize '((lambda () (if 3 ((lambda () 3)))))))


;; tail call7 ** not tail call ***
(compile '((lambda () (if ((lambda () 3)) 4 5))))
(eqt #(CONSTANT 3 TEST 4 CONSTANT 4 LOCAL_JMP 4 CONSTANT 5) (compile '((lambda () (if ((lambda () 3)) 4 5)))))

(compile-no-optimize '((lambda () (if ((lambda () 3)) 4 5))))
(eqt #(FRAME 31 CLOSURE 27 0 #f 0 FRAME 12 CLOSURE 8 0 #f 0 CONSTANT 3 RETURN 0 CALL 0 TEST 4 CONSTANT 4 LOCAL_JMP 4 CONSTANT 5 RETURN 0 CALL 0) (compile-no-optimize '((lambda () (if ((lambda () 3)) 4 5)))))


(compile-no-optimize '((lambda (a) a) 101))
(eqt #(FRAME 15 CONSTANT 101 PUSH CLOSURE 8 1 #f 0 REFER_LOCAL 0 RETURN 1 CALL 1) (compile-no-optimize '((lambda (a) a) 101)))

(compile '((lambda (a) a) 101))
(eqt #(CONSTANT 101) (compile '((lambda (a) a) 101)))


(compile '((lambda (a) (lambda () a)) 10))
(eqt #(CLOSURE 8 0 #f 0 CONSTANT 10 RETURN 0) (compile '((lambda (a) (lambda () a)) 10)))

(compile-no-optimize '((lambda (a) (lambda () a)) 10))
(eqt #(FRAME 25 CONSTANT 10 PUSH CLOSURE 18 1 #f 0 REFER_LOCAL 0 PUSH CLOSURE 8 0 #f 1 REFER_FREE 0 RETURN 0 RETURN 1 CALL 1) (compile-no-optimize '((lambda (a) (lambda () a)) 10)))


(compile '((lambda (a) ((lambda () (set! a 101)))) '()))
(eqt #(LET_FRAME CONSTANT () PUSH BOX 0 ENTER CONSTANT 101 ASSIGN_LOCAL 0 LEAVE 1) (compile '((lambda (a) ((lambda () (set! a 101)))) '())))

(compile-no-optimize '((lambda (a) ((lambda () (set! a 101)))) '()))
(eqt #(FRAME 34 CONSTANT () PUSH CLOSURE 27 1 #f 0 BOX 0 REFER_LOCAL 0 PUSH CLOSURE 10 0 #f 1 CONSTANT 101 ASSIGN_FREE 0 RETURN 0 SHIFT 0 1 CALL 0 RETURN 1 CALL 1) (compile-no-optimize '((lambda (a) ((lambda () (set! a 101)))) '())))


(compile '((lambda (a) (set! a 12) a) 2))
(eqt #(LET_FRAME CONSTANT 2 PUSH BOX 0 ENTER CONSTANT 12 ASSIGN_LOCAL 0 REFER_LOCAL 0 INDIRECT LEAVE 1) (compile '((lambda (a) (set! a 12) a) 2)))

(compile-no-optimize '((lambda (a) (set! a 12) a) 2))
(eqt #(FRAME 22 CONSTANT 2 PUSH CLOSURE 15 1 #f 0 BOX 0 CONSTANT 12 ASSIGN_LOCAL 0 REFER_LOCAL 0 INDIRECT RETURN 1 CALL 1) (compile-no-optimize '((lambda (a) (set! a 12) a) 2)))

])
(define (vm-test)
  (with-input-from-file "./test-data.scm"
    (lambda ()
      (let loop1 ([obj (read)])
        (cond
         [(eof-object? obj) '()]
         [(and (pair? obj) (>= (length obj) 2))
          (cond [(eq? 'lib (first obj))
                 (init-library-table) ;; found 'lib prefix, then init library table.
                 (let loop2 ([lst (cddr obj)])
                   (cond
                    [(null? (cdr lst))
                     (test* (cdr obj) (if (eq? (second obj) 'error) *test-error* (second obj)) (evaluate (car lst)))
                     (loop1 (read))]
                    [else
                     (guard (e
                             (#t (test* (car lst) (if (eq? (second obj) 'error) *test-error* '()) (raise e))))
                            (evaluate (car lst)))
                     (loop2 (cdr lst))]))]
                [(eq? 'mosh-only(first obj))
                     (loop1 (read))]
                [else
                 (let loop2 ([lst (cdr obj)])
                   (cond
                    [(null? (cdr lst))
                     (test* (cdr obj) (if (eq? (first obj) 'error) *test-error* (first obj)) (evaluate (car lst)))
                     (loop1 (read))]
                    [else
                     (evaluate (car lst))
                     (loop2 (cdr lst))]))])]
         [else
          (error "invalid test form")])))))

(define (dump-vm-debug-info)
  (define (dump key value)
    (print (format "  ~a : ~a" key value)))
  (cond [debug-mode
         (print "\n====   debug informtion    ====")
         (dump "max sp" debug-max-sp)
         (dump "compile time" debug-compile-time)
         (dump "vm run time" debug-vm-run-time)
         ]))


(define (map-pair f l)
  (cond
   [(null? l)
    '()]
   [(pair? l)
    (cons (f (car l)) (map-pair f (cdr l)))]
   [else
    l]))

(use gauche.sequence)
;; (define (compile-file file . num?)
;;   (define *vm-instructions*
;;     (with-input-from-file "./instruction.scm"
;;       (lambda ()
;;         (let loop ([obj (read)]
;;                    [ret '()])
;;           (if (eof-object? obj)
;;               (reverse ret)
;;               (match obj
;;                 [('define-insn name n)
;;                  (loop (read) (cons name ret))]))))))
;;   (define (insn->num-insn lst)
;;     (map-pair (lambda (x)
;;            (if (pair? x)
;;                (insn->num-insn x)
;;                (receive (index val) (find-with-index (lambda (insn) (eq? insn x)) *vm-instructions*)
;;                  (if index
;;                      (if num? (list '*insn* index) x)
;;                      x))))
;;            lst))
;;   (with-input-from-file file
;;     (lambda ()
;;       (let loop ([obj (read)]
;;                  [ret '()])
;;         (cond [(eof-object? obj)
;;                (list->vector (insn->num-insn (vector->list (pass4 ret))))]
;;               [else
;;                (loop (read) (append ret (compile-partial obj)))])))))

(define (fetch-instructions)
  (with-input-from-file "./instruction.scm"
    (lambda ()
      (let loop ([obj (read)])
        (cond
         [(eof-object? obj) '()]
         [else
          (match obj
            [('define-insn name n)
             (cons (cons name n) (loop (read)))])])))))

(define (insn-sym->insn-num insn-table syms)
  ;; For (PUSH 3) in #(CONSTANT (PUSH 3) ...).
  ;; We convert all instructions as *compiler-insn*.
  (define (insn-sym->insn-num-direct syms)
    (let loop ([syms syms])
      (cond [(null? syms)
             '()]
            [else
             (receive (i val) (find-with-index (lambda (insn) (eq? (first insn) (car syms))) insn-table)
               (cons (if i
                         `(*compiler-insn* ,i)
                         (car syms))
                     (loop (cdr syms))))])))
  (let loop ([index 0]
             [next-insn-index 0]
             [syms syms])
    (cond [(null? syms)
           '()]
          ;; special case. compiler has list which has instruction like '(0 UNDEF).
          ;; so we convert it into '(0 (*compiler-insn* n))
          ;; N.B. we ignore dotted pair.
          [(list? (car syms))
           (cons (insn-sym->insn-num-direct (car syms))
                 (loop (+ index 1)
                       next-insn-index
                       (cdr syms)))]
          [else
           (receive (i val) (find-with-index (lambda (insn) (eq? (first insn) (car syms))) insn-table)
             (cons (if i
                       (if (= index next-insn-index)
                           `(*insn* ,i)
                           `(*compiler-insn* ,i))
                       (car syms))
                   (loop (+ index 1) (if (= index next-insn-index)
                                         (if val (+ next-insn-index (cdr val) 1) (errorf "instruction.scm offset wrong on ~a" (car syms)))
                                         next-insn-index) (cdr syms))))])))

; not used but usable.
(define (acons-diff after before)
  (append-map (lambda (x) (if (assq (car x) before) '() (list x))) after))

(define (assq-multi alist keys)
  (append-map (lambda (keys) (list (assq keys alist))) keys))

;; Generate pre-compiled code.
;; (1)normal form
;;    normal forms are compiled by (compile-partial ...).
;;    It compiles form into list of instruction like '(CONST 3 ...), on this phase labels is not fixed up yet.
;; (2)macro form
;;    Normally compiled macro forms are stored ($library ...)'s macro.
;;    ($library ...)'s macro is alist like '((macro-name . compiled-macro-body) (macro-name2 . compiled-macro-body2)).
;;    Note that macro information is stored in compiler(means dynamic), on the hand pre-compiled-code is static.
;;    So you should bring this information to VM.
;;    For this purpose, we generate the code ($library.set-macro! top-level-library macros-alist) and put them on the tail of library.
;; (3)code size
;;    If you pre-compile all of the macro that top-level-library has.
;;    The size of compiler.cpp becomes bigger and difficult to compiler with g++.

(define (compile-file file . for-vm-cpp?)
  (with-input-from-file file
    (lambda ()
      (let loop ([obj (read)]
                 [ret '()])
        (cond [(eof-object? obj)
;               (let* ([allowed-macro '(acond guard receive)] ;; allowed macro!
               (let* ([allowed-macro '(define-simple-struct do acond guard receive defmacro match-let1 gentemp match match-lambda match-lambda* match-let match-let* match-letrec match-define defstruct define-structure define-const-structure)] ;; allowed macro!
;                      [v (map (lambda (x) (if x (cons (car x) (insn-sym->insn-num (fetch-instructions) (cdr x)))) '())
                      [v (map (lambda (x) (cons (car x) (insn-sym->insn-num (fetch-instructions) (cdr x))))
                              (assq-multi ($library.macro top-level-library) allowed-macro))]
                      [c (compile-partial `($library.set-macro! top-level-library (quote ,v)))])
               (if (and (pair? for-vm-cpp?) (car for-vm-cpp?))
                   (list->vector (insn-sym->insn-num (fetch-instructions) (vector->list (pass4 (append ret c)))))
                   (pass4 ret)))]
              [else
               (loop (read) (append ret (compile-partial obj)))])))))

(define-macro (do . sexp)
  (match sexp
    [(((var init step ...) ...)
         (test expr ...)
       command ...)
     `(letrec
       ((loop
         (lambda (,@var)
           (if ,test
               (begin
                 #f ; avoid empty begin
                 ,@expr)
               (begin
                 ,@command
                 (loop ,@(map (lambda (v s) `(do "step" ,v ,@s)) var step)))))))
        (loop ,@init))]
    [("step" x)
     x]
    [("step" x y)
     y]
    [else
     (syntax-error "malformed do on mosh")]))



(define (main args)
  (set! *command-line-args* (cdr args))
  (cond
   ;; test
   [(= (length args) 1)
    (vm-init '())
    (load-file "./library.scm")
    (load-file "./match.scm")
    (vm-test)
    (set! optimize? (not optimize?))
    (vm-init '())
    (load-file "./library.scm")
    (load-file "./match.scm")

    (vm-test)
    (test-end)

    ]
   ;; compile string
   [(and (= (length args) 3) (string=? (second args) "compile"))
;    (pretty-print (list->vector (compile-string (third args))))
    (print (compile-string (third args)))]
   ;;  compile a file
   [(and (= (length args) 3) (string=? (second args) "compile-file"))
    (load-file "./library.scm")
    (load-file "./match.scm")
    (write (compile-file (third args) #t))
;;     (let ([compiled (list->vector (compile-file (third args) #t))]
;;           [v (map (lambda (x) (cons (car x) (insn-sym->insn-num (fetch-instructions) (cdr x)))) `(,(assq 'kar ($library.macro top-level-library))))])
;;       (write (list->vector (append compiled (vector->list (compile `($library.set-macro! top-level-library (quote ,v)) #t)))))
        ]

;    (print (compile `($library.set-macro top-level-library (quote ,($library.macro top-level-library)))))]
;    (write `($library.set-macro! top-level-library ,($library.macro top-level-library)))]
   ;;  execute script
   [else
    (vm-init (cdr args))
;    (load-file "./instruction.scm")
    (load-file "./library.scm")
    (load-file "./match.scm")
    (load-file (second args))
    (dump-vm-debug-info)]
   )
  0)
