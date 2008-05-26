;; don't edit start
(#t #t)
;; don't edit end

;; test start
(#t (and))
(3 3)
(4 4)
(#t (if #f #f #t))
(3 ((lambda (a) 3) 4))
(7 ((lambda (a) (if 3 7 5)) 6))
(3 ((lambda () 3)))
(101 ((lambda (a) a) 101))
(102 (((lambda () (lambda () 102)))))
(102 (((lambda () (lambda (a) 102))) 101))
(103 (((lambda () (lambda (a) a))) 103))
(10 (((lambda (a) (lambda () a)) 10)))
(12 ((lambda (a) (set! a 12) a) 2))
(101  ((lambda (a) ((lambda () (set! a 101)))) '()))
;(2 ((lambda (g) ((lambda (f) (f 2)) (lambda (a) (g a)))) (lambda (x) x)))
(4 (call/cc (lambda (c) (c 4))))
(3 ((lambda (cont)
   (if (call/cc (lambda (c) (set! cont c)))
       (cont #f)
       3)) '()))
(102 ((lambda (cont)
   (if (call/cc (lambda (c) (set! cont c)))
       ((lambda ()
          ((lambda ()
             (cont #f)
             ))
          ))
       102)) '()))
(5 ((lambda () 3 4 5)))
(#t (number? 3))
(#f (number? 'a))
(#f (number? 'a))
(4  (+ 4))
(7  (+ 4 3))
(17 (+ 4 3 10))
(4 (+ 1 1 1 1))
(5 (- 10 5))
(3 (- 10 5 2))
((a . b) (cons 'a 'b))
(2 (car (cons 2 3)))
(3 (cdr (cons 2  3)))
(3 (cadr (cons 2 (cons 3 '()))))
(#t (= 3 3))
(#f (= 3 4))
;;
;; let1 using ENTER/LEAVE
;;
(3 (let ([a 3]) a))
(1 (let ([a 3] [b 1]) b))
(3 (let ([a 3] [b 1]) a))
(1 (let ([a 3] [b 1]) a b))
(3 (let1 a 3 a))
(4 (let1 a 3 (let1 b 4 b)))
(3 (let1 a 3 (let1 b 4 a)))
(7 (let1 a 3 (let1 b 4 (+ a b))))
(12 (let1 a 3 (let1 b 4 (let1 c 5 (+ a b c)))))
(12 (let ([a 3] [b 4]) (let1 c 5 (+ a b c))))
(13 (let ([a 3] [b 4]) (+ (let1 c 5 (+ a b c)) 1)))
(4 (let1 a 3 (let1 a 4 a)))
(5 (let1 a 3 (set! a (+ a 1)) (+ a 1)))
(3 (let1 a 3 (let1 b 4 (set! b a) b)))
(2 (let ([a 2] [b 3]) a))
;(3 (let1 a 3
;     (let1 b (lambda () a)
;       (b))))
;(3 (let ([a 3]) (let ([b (lambda () a)]) (b))))
(2 (let ([a 0]
         [b 1]
         [c 2])
     c))
(4
 (let1 a 1
   (let1 b 2
     (let1 c a
       (+ a b c)))))
(3 (let ((a 3)) a))
(7 (let ((a 3) (b 4)) (+ a b)))
(4 (let* ((a 3) (b (+ a 1))) b))
(4 (let1 a 3
     (let1 b 4
       (let1 c (lambda () b)
         (set! a c)))
     (a)))
(1
 (let ([a 0] [b 1])
   (let ([c (lambda () b)])
     (c))))
(1
 (let ([a 0] [b 1])
   ((lambda ()
      b))))
(3
 (let ([a 0] [b 1])
   (let ((c (lambda () (set! b 3) b)))
     (c))))
(3
 (let ([a 0] [b 1])
   (let1 c (lambda () (set! b 3) b)
     (c))))
(100
 (let1 a 100
   (let1 c (let1 d (lambda () a) d)
     (c))))
(1000 (let ([a '()])
        (let ([G68 (lambda (i) (if (>= i 1000) i (a (+ i 1))))])
          (set! a G68)
          (a 0))))
(1000 (letrec ((a (lambda (i)
           (if (>= i 1000)
               i
               (a (+ i 1))))))
        (a 0)))
(1 (letrec ([a 1]
            [b (lambda () a)])
     (b)))
((#t . #f) (letrec ((even?
                     (lambda (n)
                       (if (= 0 n)
                           #t
                           (odd? (- n 1)))))
                    (odd?
                     (lambda (n)
                       (if (= 0 n)
                           #f
                           (even? (- n 1))))))
             (cons (even? 88) (odd? 88))))
(10 (letrec ([a (lambda (i) (if (= i 10) i (a (+ i 1))))])
      (a 0)))
(1000 ((lambda (a) (set! a 1000) a) '()))
(20 ((lambda (a)
         (set! a (lambda (i)
                   (if (= i 20)
                       i
                       (a (+ i 1)))))
         (a 0)
         )
       '())
)
(3 (or #f 3 4))
(3 (define a 3))
(3 a)
(#f (= 3 4))
(#t (= 3 3 3))
(#f (= 3 4 5))
(101 (((lambda (a) (lambda () a)) 101)))
(102 (((lambda (a) (lambda (b) (+ a b))) 101) 1))
(#t (null? '()))
(#f (null? 3))
((1 . 2) (cons 1 2))
((1 2) (cons 1 (cons 2 '())))
(3 (begin 1 2 3))
(4 ((lambda () (set! a 4) a)))
;; tail call
(3 ((lambda () ((lambda () 3)))))
(3 ((lambda () ((lambda (x) x) 3))))
(3 ((lambda (y) ((lambda (x) x) 3)) 4))
(3 ((lambda () (let1 a 1 ((lambda () 3))))))
(3 ((lambda () (let1 b 2 (let1 a 1 ((lambda () 3)))))))
(3 ((lambda () (if 3 ((lambda () 3))))))
;; ** not tail call ***
(4 ((lambda () (if ((lambda () 3)) 4 5))))
(10 (let loop ([i 0])
      (if (= i 10)
          i
          (let1 a 1
            (let1 b 0
              (loop (+ i a b))))))) ;let1 at tail context
(10 (let loop ([i 0])
      (if (= i 10)
          i
          (let1 a 1
            (let1 b 0
              (loop (+ i a b))))))) ;let1 at tail context
(6 ((lambda () (define d (lambda (x y z) (+ x y z))) (d 1 2 3))))
(3 ((lambda ()
     (define b (lambda () 3))
     (b))))
(3 (apply car '((3 2))))
((1 2 3) ((lambda a a) 1 2 3))
((2 3) ((lambda (a . b) b) 1 2 3))
((2 3 4) ((lambda (a . b) b) 1 2 3 4))
((3 4) ((lambda (a b . c) c) 1 2 3 4))
((4) ((lambda (a b c . d ) d) 1 2 3 4))
(() ((lambda (a b c . d ) d) 1 2 3))
(() ((lambda a a)))
((1) ((lambda a a) 1))
(34 (when #t 1 2 34))
(#f (not 3))
(48 (unless #f 1 2 48))
(5 (and 3 4 5))
;; check side effect of "and" and "or" .
(1 (let1 a 0 (and (set! a (+ a 1))) a))
(1 (let1 a 0 (or (set! a (+ a 1))) a))
(#f (and 3 #f 5))
(3 (or 3 4 5))
(#f (or #f #f #f))
(#t (> 4 3))
(#t (> 4 3 2))
(#f (> 4 3 1 2))
(#t (>= 3 3 3))
(#t (>= 4 3 3))
(#t (>= 4 3))
(#t (< 1 2))
(#t (< 1 2 3))
(#f (< 1 5 3))
(#t (<= 1 2))
(#t (<= 1 2 3))
(#t (<= 1 3 3))
(#f (<= 1 5 3))
(#t (eq? #t #t))
(#f (eq? #t #f))
(#t (eq? 'a 'a))
(#f (eq? 'a 'b))
(#t (pair? (cons 1 2)))
(#f (pair? 3))
(#t (symbol? 'a))
(#f (symbol? 3))
(3 (cond (#f 1)
         (#t 3)))
(3 (cond (#f 1)
         (#f 2)
         (else 3)))
(3 (cond (#t 3)
         (#f 2)
         (else 1)))
(1 (cond ((cons 1 2) => car)
         (#f 2)
         (else 3)))
(3 (apply (lambda (a) a) '(3)))
(7 (apply (lambda (a b) (+ a b)) '(5 2)))
(8 (apply (lambda (a b c) (+ a b c)) '(5 2 1)))
;; quasiquote
((0 b c) (let ([a 0]) `(,a b c)))
(((1 2 3)  b c) (let ([a '(1 2 3)]) `(,a b c)))
((1 2 3  b c) (let ([a '(1 2 3)]) `(,@a b c)))
((list a 'a) (let ([name 'a]) `(list ,name ',name)))
((list 3 4) `(list ,(+ 1 2) 4))
((1 1 2 3) (let ([a '(1 2 3)]) `(1 . ,a)))
((1 2 3) (let ([a '(1 2 3)]) `,a))
((1 2 3) (let ([a '(1 2 3)]) `(,@a)))
((0 1 2 3) (let ([a '(1 2 3)]) `(0 ,@a)))
((0 (1 2 3) 4) (let ([a '(1 2 3)]) `(0 ,a 4)))
((1 2 3 4) (let ([a '(1 2 3)]) `(,@a 4)))
(((1 2 3) 4) (let ([a '(1 2 3)]) `((,@a) 4)))
((((1 2 3)) 4) (let ([a '(1 2 3)]) `((,a) 4)))
(b `b)
((1 2 3) (list 1 2 3))
(3 (aif (+ 1 2) it #f))
(3 (string-length "abc"))
(3 (string-length "あいう"))
(abc (string->symbol "abc"))
("123" (number->string 123))
((1 2 3 4) (begin (define (proc1 . a) a) (proc1 1 2 3 4)))
((2 3) ((lambda (a . b) b) 1 2 3))
(1 ((lambda (a . b) a) 1 2 3 4 5))
((2 3 4 5) ((lambda (a . b) b) 1 2 3 4 5))
(() ((lambda (a b c d . e) e) 1 2 3 4))
(1 ((lambda (a b c d . e) a) 1 2 3 4))
(2 ((lambda (a b c d . e) b) 1 2 3 4))
(3 ((lambda (a b c d . e) c) 1 2 3 4))
(4 ((lambda (a b c d . e) d) 1 2 3 4))
((1 2 3 4) (append '(1 2) '(3 4)))

(3 (begin (define x 3) x))
((1 2 3) (begin (define (hoge . a) a) (hoge 1 2 3)))
((2 3) (begin (define (hige a . b) b) (hige 1 2 3)))
((3 2) (apply (lambda a a) '(3 2)))

(#t (equal? '(1 2 (3)) '(1 2 (3))))
(1 (let ((a 3)) 3 2 1))
("   " (make-string 3))
("ccc" (make-string 3 #\c))
(3 (apply  car '((3))))
(3 (apply (lambda (a) (car a)) '((3))))
(3 (apply (lambda (a . b) (+ a (car b))) '(1 2)))
("123456" (string-append "12" "345" "6"))
(3 (find (lambda (e) (= e 3)) (list 1 2 3)))
(#t (string? "hige"))
(("key" "value") (assoc "key" '(("key" "value"))))
(("12" "34" "56") (string-split "12\n34\n56" #\newline))
(#\2 (let ([p (open-string-input-port "12345")])
          (read-char p)
          (read-char p)))
(#t (eof-object? (let ([p (open-string-input-port "1")]) (read-char p) (read-char p))))
(123 (let ([p (open-string-input-port "123 456")]) (read p)))
(#f (rxmatch #/123/ "12"))
(#t (if (rxmatch #/123/ "123") #t #f))
(#t (regexp? #/abc/))
(#f (regexp? "abc"))
(1 (rxmatch-start (rxmatch #/\d+/ "a345a")))
(2 (rxmatch-start (rxmatch #/\d+/ "ab345a")))
(3 (rxmatch-start (rxmatch #/\d+/ "abあ345a")))
(1 (rxmatch-start (rxmatch #/(\d+)(a)/ "a345a") 1))
(4 (rxmatch-start (rxmatch #/(\d+)(a)/ "a345a") 2))
(4 (rxmatch-end  (rxmatch #/\d+/ "a345a")))
(4 (rxmatch-end  (rxmatch #/(\d+)(a)/ "a345a") 1))
(5 (rxmatch-end  (rxmatch #/(\d+)(a)/ "a345a") 2))
(#f (rxmatch-end (rxmatch #/\d+/ "aaaa")))
(#f (#/123/ "12"))
(1 (rxmatch-start (#/\d+/ "a345a")))
(2 (rxmatch-start (#/\d+/ "ab345a")))
(3 (rxmatch-start (#/\d+/ "abあ345a")))
(1 (rxmatch-start (#/(\d+)(a)/ "a345a") 1))
(4 (rxmatch-start (#/(\d+)(a)/ "a345a") 2))
("def" (rxmatch-after (#/abc/ "123abcdef")))
("あいうえ" (rxmatch-after (#/abc/ "123abcあいうえ")))
("あいうえ" (rxmatch-after (#/かきく/ "123かきくあいうえ")))
("123" (rxmatch-before (#/abc/ "123abcdef")))
("abc" ((#/abc/ "123abcdef") 0))
("abc" ((#/abc/ "123abcdef")))
("def" ((#/abc/ "123abcdef") 'after))
("123" ((#/abc/ "123abcdef") 'before))
(#f (rxmatch #/123/ "12"))

(#f (#/123/ "12"))
(#t (if (#/^abc/ "abc") #t #f))
("abc" (regexp->string #/abc/))
(#f (rxmatch-start (#/\d+/ "aaaa")))
(4 (rxmatch-end (#/\d+/ "a345a")))
(4 (rxmatch-end (#/(\d+)(a)/ "a345a") 1))
(5 (rxmatch-end (#/(\d+)(a)/ "a345a") 2))
(#f (rxmatch-end (#/\d+/ "aaaa")))
(#f (rxmatch #/123/ "12"))
(#t (if (rxmatch #/123/ "123") #t #f))
(#f (#/123/ "12"))
(#t (if (#/^abc/ "abc") #t #f))
(#t (regexp? #/abc/))
(#f (regexp? "abc"))
("abc" (regexp->string #/abc/))
(1 (rxmatch-start (#/\d+/ "a345a")))
("345" (rxmatch-substring (#/\d+/ "a345a")))
("345" (rxmatch-substring (#/(\d+)(a)/ "a345a") 1))
("a" (rxmatch-substring (#/(\d+)(a)/ "a345a") 2))
(#f (rxmatch-substring (#/\d+/ "aaaa")))
("あ" ((#/あ/ "あ")))
(#/abc/ (string->regexp "abc"))
(#f (let1 m (#/^#([^(^)^\s]+)(?:\(([^\)]+)\))?/ "#comment")
   (m 2)
))
(a (begin (let ((xxx 'a)) (case xxx ((b) 'b) ((a) 'a)))))
(3 (begin (let ((xxy 'a)) (case xxy ((b) 'b) ((c) 'c) (else 3)))))
(-1 ((lambda (a) (call/cc (lambda (c) (c -1) 4))) 2))
(6 (* 2 3))
(24 (* 2 3 4))
(4 (call/cc (lambda (c) (c 4))))
(123 (string->number "123"))
(123 (let ([p (open-string-input-port "123 456")]) (read p)))
(#\1 (let ([p (open-string-input-port "123 456")]) (read-char p)))
((4 3 2 1) (reverse '(1 2 3 4)))
(("wiki" "cmd") (string-split "wiki&cmd" #\&))
("cbc" (begin (define str1 (make-string 3 #\c)) (string-set! str1 1 #\b) str1))
(-1 ((lambda (a) (call/cc (lambda (c) (c -1)))) 2))
; order of arguments evaluation.
(2 (let* ([a 0]
      [b (lambda (x y) a)])
  (b (begin (set! a 1)) (begin (set! a 2)))))
(#\a #\a)
(#f (eof-object? 3))
(101 101)
;; internal define
(4 (define val 3) (define (func8) (define val 4) val) (func8))
((1 . 3) ((lambda () (define p (cons 1 2)) (set-cdr! p 3) p)))
((3 . 2) ((lambda () (define q (cons 1 2)) (set-car! q 3) q)))
(#t (begin (display "test done") #t))
(3 (vector-length (make-vector 3)))
(101 (let loop ((i 0))
       (if (= i 100)
           (+ i 1)
           (loop (+ i 1)))))
(3 ((lambda (cont)
   (if (call/cc (lambda (c) (set! cont c)))
       (cont #f)
       3)) '()))
(102 ((lambda (cont)
   (if (call/cc (lambda (c) (set! cont c)))
       ((lambda ()
          ((lambda ()
             (cont #f)
             ))
          ))
       102)) '()))
(2 (let ((a 0))
     (cond (#t (set! a (+ a 1)) (set! a (+ a 1)) a))))
(#t (char? #\あ))
(#f (eq? (list 'a) (list 'a)))
(#t (let ((x (list 'a))) (eq? x x)))
(("ABC123" "DEF123") (map1 (lambda (s) (string-append s "123")) '("ABC" "DEF")))
(("ABC123" "DEF123") (map (lambda (s) (string-append s "123")) '("ABC" "DEF")))
(("ABCGHI" "DEFJKL") (map (lambda (a  b) (string-append a b)) '("ABC" "DEF") '("GHI" "JKL")))
((4 10 18) (map (lambda (a b) (* a b)) '(1 2 3) '(4 5 6)))
(10000 (let1 a '()
        (let1 G68 (lambda (i) (if (>= i 10000) i (a (+ i 1))))
          (set! a G68)
          (a 0))))
;; quasiquote test from R5RS start
((list 3 4) `(list ,(+ 1 2) 4))
((list a (quote a)) (let ((name 'a)) `(list ,name ',name)))
((a 3 4 5 6 b) `(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b))
(#t (vector? #(3)))
(3 (begin (define (proc-01) 3) (proc-01)))
(3 (begin (define (add3 a b) (+ a b)) (add3 1 2)))
(3 (begin (define add2 (lambda (a b) (+ a b))) (add2 1 2)))
(3 (begin (define z (make-vector 2)) (vector-set! z 0 1) (vector-set! z 1 2) (make-vector 3) (null? 3) (vector-set! z 1 3) (vector-ref z 1)))

(3 (begin (define (proc-2) (define (rec) 3) (rec)) (proc-2)))
;; internal define
(4 (begin (define (func2) (define val 4) val) (func2)))
;; values
(#t (if (values 1 2 3) #t #f))
(6 (call-with-values (lambda () (values 1 2 3)) (lambda (a b c) (+ a b c))))
(6 (receive (a b c) (values 1 2 3) (+ a b c)))
;(error (call-with-values (lambda () (values 1 2)) (lambda (a b c) (+ a b c))))
("higepon" (receive (port proc) (open-string-output-port)
             (display "hige" port)
             (display "pon" port)
             (proc)))
("\"string\"" (call-with-string-output-port (lambda (port) (write "string" port))))
("123ABC456" (regexp-replace #/abc/ "123abc456" "ABC"))
;; ;;--------------------------------------------------------------------
;; ;;
;; ;; library
;; ;;
[lib "hello higepon\n"
(library (higepon)
         (export hello)
         (import)
         (define hello "hello higepon\n"))
(import (higepon))
hello
]

[lib "hello higepon\n"
(library (higepon2)
         (export hello)
         (import)
         (define hello "hello higepon\n")
         (define goodbye "goodbye higepon\n"))
(import (higepon2))
hello
]

[lib "goodbye higepon\n"
(library (higepon3)
         (export hello goodbye)
         (import)
         (define hello "hello higepon\n")
         (define goodbye "goodbye higepon\n"))
(import (higepon3))
hello
goodbye
]

[lib "hello higepon\n"
(library (higepon4)
         (export hello goodbye)
         (import)
         (define hello "hello higepon\n")
         (define goodbye "goodbye higepon\n"))
(import (only (higepon4) hello))
hello
]


[lib "goodbye higepon\n"
(library (higepon5)
         (export hello goodbye)
         (import)
         (define hello "hello higepon\n")
         (define goodbye "goodbye higepon\n"))
(import (except (higepon5) hello))
goodbye
]


[lib "hello higepon\n"
(library (higepon6)
         (export hello goodbye)
         (import)
         (define hello "hello higepon\n")
         (define goodbye "goodbye higepon\n"))
(import (rename (higepon6) (hello hige:hello)))
hige:hello
]

[lib "goodbye higepon\n"
(library (higepon7)
         (export hello goodbye)
         (import)
         (define hello "hello higepon\n")
         (define goodbye "goodbye higepon\n"))
(import (prefix (higepon7) higepon.))
higepon.goodbye
]


[lib "lulululuhello higepon\n"
(library (higepon8)
         (export hello goodbye)
         (import)
         (define hello "hello higepon\n")
         (define goodbye "goodbye higepon\n"))
(library (ipod (3))
         (export play)
         (import)
         (define play "lulululu"))
(import (prefix (higepon8) higepon.)
        (ipod))
(string-append play higepon.hello)
]


;; we now ignore version
[lib "goodbye higepon\n"
(library (higepon9 (7))
         (export hello goodbye)
         (import)
         (define hello "hello higepon\n")
         (define goodbye "goodbye higepon\n"))
(import (prefix (higepon9 (6)) higepon.))
higepon.goodbye
]


[lib "goodbye higepon\n"
(library (higepon10)
         (export hello goodbye)
         (import)
         (define hello "hello higepon\n")
         (define goodbye "goodbye higepon\n"))
(import (prefix (higepon10) higepon.))
higepon.goodbye
]


[lib "goodbye higepon\nhi higepon\ncall\n"
(library (higepon11)
         (export hi
                 hello
                 (rename (goodbye bye) (call c)))
         (import)
         (define hello   "hello higepon\n")
         (define hi      "hi higepon\n")
         (define goodbye "goodbye higepon\n")
         (define call    "call\n"))
(import (higepon11))
(string-append bye hi c)
]


[lib "lulululuhello higepon\n"
(library (ipod0 (3))
         (export play)
         (import)
         (define play "lulululu"))
(library (higepon12)
         (export hello goodby play)
         (import (ipod0))
         (define hello "hello higepon\n")
         (define goodbye "goodbye higepon\n"))
(import (higepon12))
(string-append play hello)
]


;; (ipod) library imported twice.
;; Body of ipod must not be instatiated twice!
[lib "hello higepon\n"
(library (ipod2 (3))
         (export play)
         (import)
         (define play "lulululu"))
(library (higepon13)
         (export hello goodby play)
         (import (ipod2))
         (define hello "hello higepon\n")
         (define goodbye "goodbye higepon\n"))
(library (hagepon)
         (export hello)
         (import (ipod2)
                 (higepon13)))

(import (hagepon))
(string-append hello)
]

[lib "lulululuhello higepon\n"
(library (ipod4 (3))
         (export play)
         (import)
         (define play "lulululu"))
(library (higepon14)
         (export hello goodby play)
         (import (for (ipod4) run))
         (define hello "hello higepon\n")
         (define goodbye "goodbye higepon\n"))
(import (higepon14))
(string-append play hello)
]


[lib 5
(library (my-macro-lib)
         (export add3)
         (import)
         (define-macro (add3 v) `(+ 3 ,v)))

(library (test)
         (export val)
         (import (my-macro-lib))
         (define val (add3 2)))

(import (test))
val
]

;; from R6RS
;;; cons
((a) (cons 'a '()))
(((a) b c d) (cons '(a) '(b c d)))
(("a" b c) (cons "a" '(b c)))
((a . 3) (cons 'a 3))
(((a b) . c) (cons '(a b) 'c))

;;; car
(a (car '(a b c)))
((a) (car '((a) b c d)))
(1 (car '(1 . 2)))
;(&assertion exception (car '()))

;;; cdr
((b c d) (cdr '((a) b c d)))
(2 (cdr '(1 . 2)))
;(&assertion exception (cdr '()))

;;; reverse
((c b a) (reverse '(a b c)))
(((e (f)) d (b c) a) (reverse '(a (b c) d (e (f)))))

;;; equal?
(#t (equal? 'a 'a))
(#t (equal? '(a) '(a)))
(#t (equal? '(a (b) c) '(a (b) c)))
(#t (equal? "abc" "abc"))
(#t (equal? 2 2))
(#t (equal? (make-vector 5 'a) (make-vector 5 'a)))

;;; eq?
(#t (eq? 'a 'a))
(#f (eq? '(a) '(a))) ;; unspecified on R6RS
(#f (eq? (list 'a) (list 'a)))
(#f (eq? "a" "a"))   ;; unspecified on R6RS
(#f (eq? "" ""))     ;; unspecified on R6RS
(#t (eq? '() '()))   ;; unspecified on R6RS
(#t (eq? 2 2))       ;; unspecified on R6RS
(#t (eq? #\A #\A))   ;; unspecified on R6RS
(#t (eq? car car))
(#t (let ((n (+ 2 3)))
      (eq? n n)))    ;; unspecified on R6RS
(#t (let ((x '(a)))
      (eq? x x)))
(#t (let ((x '#()))
      (eq? x x)))    ;; unspecified on R6RS
(#t (let ((p (lambda (x) x)))
      (eq? p p)))    ;; unspecified on R6RS

;; -
(-1 (- 3 4))
(-6 (- 3 4 5))
(-3 (- 3))

;; cond
(greater (cond ((> 3 2) 'greater)
                ((< 3 2) 'less)))
(equal (cond ((> 3 3) 'greater)
              ((< 3 3) 'less)
              (else 'equal)))
(2 (cond ('(1 2 3) => cadr)
      (else #f)))

;; do
(#(0 1 2 3 4) (do ((vec (make-vector 5))
                   (i 0 (+ i 1)))
                  ((= i 5) vec)
                (vector-set! vec i i)))
(25 (let ((x '(1 3 5 7 9)))
      (do ((x x (cdr x))
           (sum 0 (+ sum (car x))))
          ((null? x) sum))))

;; vector-set!
(#(0 ("Sue" "Sue") "Anna")
 (let ((vec (vector 0 '(2 2 2 2) "Anna")))
   (vector-set! vec 1 '("Sue" "Sue"))
   vec))

;; vector-ref
(8 (vector-ref '#(1 1 2 3 5 8 13 21) 5))

;; or
(#t (or (= 2 2) (> 2 1)))
(#t (or (= 2 2) (< 2 1)))
(#f (or #f #f #f))
((b c) (or '(b c) (/ 3 0)))

;; not
(#f (not #t))
(#f (not 3))
(#f (not (list 3)))
(#t (not #f))
(#f (not '()))
(#f (not (list)))
(#f (not 'nil))

;; let
(6 (let ((x 2) (y 3))
     (* x y)))
(35 (let ((x 2) (y 3))
      (let ((x 7)
            (z (+ x y)))
        (* z x))))

;; let*
(70 (let ((x 2) (y 3))
      (let* ((x 7)
             (z (+ x y)))
        (* z x))))
;; eqv?
(#t (eqv? 'a 'a))
(#f (eqv? 'a 'b))
(#t (eqv? 2 2))
(#t (eqv? '() '()))
(#t (eqv? 100000000 100000000))
(#f (eqv? (cons 1 2) (cons 1 2)))
(#f (eqv? (lambda () 1)
          (lambda () 2)))
(#f (eqv? #f 'nil))

;; expansion of cond
(#t (define (foo n) (let loop ((n n)) (cond ((null? n)) (else (loop (cdr n)))))) (and (foo '()) (foo '(1 2 3))))


(3 (digit->integer #\3 10))
(0 (+))
(1 (*))

;; Exceptions
[mosh-only "no-error"
  (with-exception-handler
    (lambda (e)
      "error")
    (lambda () "no-error"))]
[mosh-only "error"
  (with-exception-handler
    (lambda (e)
      e)
    (lambda () (raise "error") 5))]
[mosh-only "error-is-string"
  (guard (con
          [(string? con)
           "error-is-string"]
          [else
           "error-is-not-string"])
         (raise "raise"))]
[mosh-only 3
  (guard (con
          [(string? con)
           "error-is-string"]
          [else
           "error-is-not-string"])
         3)]
[mosh-only "catched at parent"
 (guard (e
         [(symbol? e)
          "catched at parent"]
         [else
          "error"])
   (guard (con
           [(string? con)
            "error-is-string"]) ;; no else clause
          (raise 'symbol-error)))]
[mosh-only 7
  (guard (con
          [con
           4]
          [else
           "error-is-not-string"])
         (+ 3 (raise-continuable "warn continuation")))]
[mosh-only #t
  (vector-type?
   (make-vector-type 'test
                     #f
                     '()
                     '(#f #f)
                     #f))]
[mosh-only 1234
  (vector-type-data
   (make-vector-type 'test
                     #f
                     1234
                     '(#f #f)
                     #f))]
[mosh-only #t
  (let* ([vt (make-vector-type 'test #f #f '(#f #f) #f)]
         [constructor (typed-vector-constructor vt)]
         [pred?       (vector-type-predicate vt)])
    (pred? (constructor 3 4)))]
[mosh-only #t
  (let* ([parent (make-vector-type 'test-parent #f #f '(#f #f) #f)]
         [vt (make-vector-type 'test parent #f '(#f #f) #f)]
         [constructor (typed-vector-constructor vt)]
         [pred?       (vector-type-predicate parent)])
    (pred? (constructor 3 4)))]
[mosh-only 12
  (let* ([vt (make-vector-type 'test #f #f '(#f #f) #f)]
         [get-0 (typed-vector-accessor vt 0)]
         [get-1 (typed-vector-accessor vt 1)]
         [constructor (typed-vector-constructor vt)]
         [v (constructor 3 9)])
    (+ (get-0 v) (get-1 v)))]
[mosh-only 16
  (let* ([vt (make-vector-type 'test #f #f '(#f #f) #f)]
         [get-0 (typed-vector-accessor vt 0)]
         [get-1 (typed-vector-accessor vt 1)]
         [set-0 (typed-vector-mutator vt 0)]
         [constructor (typed-vector-constructor vt)]
         [v (constructor 3 9)])
    (set-0 v 7)
    (+ (get-0 v) (get-1 v)))]
[mosh-only #t
  (let* ([vt (make-vector-type 'test #f #f '(#f #f) #f)]
         [constructor (typed-vector-constructor vt)]
         [v (constructor 3 9)])
    (typed-vector? v))]
[mosh-only #t
  (let* ([vt (make-vector-type 'test #f #f '(#f #f) #f)]
         [constructor (typed-vector-constructor vt)]
         [v (constructor 3 9)])
    (eq? vt (typed-vector-type v)))]

[6
  (apply (lambda (a b c) (+ a b c)) 1 2 '(3))]
[6
  (apply (lambda (a b c) (+ a b c)) '(1 2 3))]
[6
  (apply (lambda (a b c) (+ a b c)) 1 '(2 3))]
[3 (/ 6 2)]
[3 (mod 23 10)]
[#t (even? 2)]
[#f (even? 3)]
[#f (for-all even? '(3 1 4 1 5 9))]
[#f (for-all even? '(3 1 4 1 5 9 . 2))]
[#t (for-all even? '(2 4 14))]
;[14 (for-all (lambda (n) (and (even? n) n))
;             '(2 4 14))]
[#t (for-all (lambda (a b) (< a b)) '(1 2 3) '(2 3 4))]
[#f (for-all (lambda (a b) (< a b)) '(1 2 4) '(2 3 4))]

[mosh-only 0 (/ 2)]
[mosh-only 0 (/ 3)]
;[mosh-only 0 (/ 0)] -> division by zero
[mosh-only 3 (mod 123 10)]
[mosh-only 3 (mod 123 -10)]
[mosh-only 7 (mod -123 10)]
[mosh-only 7 (mod -123 -10)]
[mosh-only 12 (div 123 10)]
[mosh-only -12(div 123 -10)]
[mosh-only -13 (div -123 10)]
[mosh-only 13 (div -123 -10)]
[#\c (string-ref "abc" 2)]
[#t (list? '(a b c))]
[#t (list? '())]
[#f (list? '(a . b))]

;; match
[(0 1 2 3 4 5)
 (match '(0 (1 2) (3 4 5))
   ((a (b c) (d e f))
    (list a b c d e f)))]
[(number 123)
 (match 123
   ((? string? x) (list 'string x))
   ((? number? x) (list 'number x)))]
["normal let, vars=(a c) exprs=(b d)"
 (define let-analyzer
   (match-lambda
    (('let (? symbol?)
           ((var expr) ...)
       body ...)
     (format "named let, vars=~s exprs=~s" var expr))
    (('let ((var expr) ...)
       body ...)
     (format "normal let, vars=~s exprs=~s" var expr))
    (_
     (format "malformed let"))))
(let-analyzer '(let ((a b) (c d)) e f g))]
["named let, vars=(x y) exprs=((f a b) (f c d))"
 (let-analyzer '(let foo ((x (f a b)) (y (f c d))) e f g))]
["malformed let"
 (let-analyzer '(let (a) b c d))]
[42
 (match '(the answer is 42)
   (`(the answer is ,value) value)
   (else #f))]
[#f
 (match '(the answer was 42)
   (`(the answer is ,value) value)
   (else #f))]
[d
 (match '(a b c d)
   ((the answer is value) value)
   (else #f))]
["base=mosh suffix=scm"
 (match "mosh.scm"
  ((? string? (= #/(.*)\.([^.]+)$/ m))
   (format "base=~a suffix=~a" (m 1) (m 2))))]
[2
 (do ((i 0) (j 0)) ((zero? j) (set! i 1) (set! i 2) i))]


;; ["syntax error: malformed when"
;;  (print (guard (con
;;          [con con]
;;          [else ""])
;;         (when #t)))]






;
;; List utilities
;; [mosh-only #f
;;   (for-all number? '(3 1 4 1 5 9))]
;; (10 (digit->integer #\A 16))
;; ;(error (cdr 3))
;; (#f (digit->integer #\Z 16))

;; (#\あ (with-input-from-string "あいう" (lambda () (read-char))))
;; (+ (with-input-from-string "+" (lambda () (read))))

;; ("/usr/bin"
;;  (find (lambda (s) (string=? "/usr/bin" s)) (string-split (sys-getenv "PATH") #\:)))
;; ;; let1 local env
;("f" (number->string 15 16))

;; ;; UTF-8
;; (227 (let ([in (open-string-input-port "あ")]) (read-byte in)))
;; (#t (= (let ([in (open-string-input-port "#")]) (read-byte in)) (char->integer #\#)))
;; (#\# (integer->char 35))
;; ("apple" (format #f "apple"))
;; ("apple is sweet" (format #f "apple is ~a" "sweet"))
;; ("apple is sweet and red" (format #f "apple is ~a and ~a" "sweet" "red"))
;; ("there are 3 apples" (format #f "there are ~d apples" 3))
;; ("apple" (format "apple"))
;; ("apple is sweet" (format "apple is ~a" "sweet"))
;; ("apple is sweet and red" (format "apple is ~a and ~a" "sweet" "red"))
;; ("there are 3 apples" (format "there are ~d apples" 3))
;; ("apple is sweet" (call-with-output-string (lambda (out) (format out "apple is ~a" "sweet") "dummy")))
;; ;; Bignum
;; (#t (number? 22222222222222222222222222222222222222222222))
;; ;; Bignum & Integer
;; (18446744073709551617 (+ 1 18446744073709551616))
;; (1208925819614629174706175 (- 1208925819614629174706176 1))
;; (381904502119539814806379823104 (* 309485009821345068724781056 1234))
;; (#t (< 1 18446744073709551616))
;; (#f (> 1 18446744073709551616))
;; (#t (<= 1 18446744073709551616))
;; (#f (>= 1 18446744073709551616))
;; ;; Bignum & Bignum
;; (84179922671405858697435414528 (+ 4951760157141521099596496896 79228162514264337593543950336 4294967296))
;; (-74276402357122816498242420736 (- 4951760157141521099596496896 79228162514264337593543950336 4294967296))
;; (121932631356500531347203169112635269 (* 123456789123456789 987654321987654321))
;; (-121932631356500531347203169112635269 (* -123456789123456789 987654321987654321))
;; (#t (= 121932631356500531347203169112635269 121932631356500531347203169112635269 121932631356500531347203169112635269 121932631356500531347203169112635269))
;; (#t (> 1111111111111111111111111111111111111111 3333333333333333333333333))
;; (#f (> -1111111111111111111111111111111111111111 3333333333333333333333333))
;; (#f (> -1111111111111111111111111111111111111111 -3333333333333333333333333))
;; (#t (< -1111111111111111111111111111111111111111 -3333333333333333333333333))
;; (#t (>= 1111111111111111111111111111111111111111 3333333333333333333333333))
;; (#f (>= -1111111111111111111111111111111111111111 3333333333333333333333333))
;; (#f (>= -1111111111111111111111111111111111111111 -3333333333333333333333333))
;; (#t (<= -1111111111111111111111111111111111111111 -3333333333333333333333333))
;; ;; Integer => Bignum
;; (10000000000000000000000000000000000000000
;;  (let loop ([i 0] [ret 1])
;;    (if (= 40 i)
;;        ret
;;        (loop (+ 1 i) (* ret 10)))))
;; (1243592236 (+ 1242944740 647496))
;; (1243592227 (+ 1242944740 647487))
;; (9 (- 1243592236 1243592227))
;; ;; write
;; ("\"string\"" (call-with-output-string (lambda (port) (write "string" port))))
;; ("\"str\\\"in\\\"ing\"" (call-with-output-string (lambda (port) (write "str\"in\"ing" port))))
;; ("\"str\\\"i\\\\\\\"s\\\\\\\"n\\\"ing\"" (call-with-output-string (lambda (port) (write "str\"i\\\"s\\\"n\"ing" port))))
;; ("#\\あ" (call-with-output-string (lambda (port) (write #\あ port))))
;; ("\"string\"" (format "~s" "string"))
;; ("\"str\\\"in\\\"ing\"" (format "~s" "str\"in\"ing"))
;; ("#\\あ" (format "~s" #\あ))
;; ("\"str\\nin\\ning\"" (call-with-output-string (lambda (port) (write "str\nin\ning" port))))
;; ("#\\newline" (call-with-output-string (lambda (port) (write #\newline port))))
;; ("#\\あ" (call-with-output-string (lambda (port) (write #\あ port))))
;; ;; write unquote
;; (",a" (call-with-output-string (lambda (port) (write '(unquote a) port))))
;; (",a" (call-with-output-string (lambda (port) (write ',a port))))
;; ("(a unquote b)" (call-with-output-string (lambda (port) (write '(a unquote b) port))))
;; ;; write unquote-splicing
;; (",@a" (call-with-output-string (lambda (port) (write '(unquote-splicing a) port))))
;; (",@a" (call-with-output-string (lambda (port) (write ',@a port))))
;; ;; write quasiquote
;; ("`a" (call-with-output-string (lambda (port) (write '(quasiquote a) port))))
;; ("`a" (call-with-output-string (lambda (port) (write '`a port))))
;; ("(a quasiquote b)" (call-with-output-string (lambda (port) (write '(a quasiquote b) port))))
;; ;; write quote
;; ("'a" (call-with-output-string (lambda (port) (write '(quote a) port))))
;; ("'a" (call-with-output-string (lambda (port) (write ''a port))))
;; ("(a quote b)" (call-with-output-string (lambda (port) (write '(a quote b) port))))
;; ;; display
;; ("string" (call-with-output-string (lambda (port) (display "string" port))))
;; ("str\"in\"ing" (call-with-output-string (lambda (port) (display "str\"in\"ing" port))))
;; ("あ" (call-with-output-string (lambda (port) (display #\あ port))))
;; ("string" (format "~a" "string"))
;; ("str\"in\"ing" (format "~a" "str\"in\"ing"))
;; ("あ" (format "~a" #\あ))
; ("\"string\"" (call-with-output-string (lambda (port) (write "string" port))))

;; ("." (find (lambda (e) (equal? "." e)) (sys-readdir "/home/taro/vm/")))
;; (#t (file-exists? "/home/taro/vm/test-data.scm"))
;; (#f (file-exists? "/home/taro/vm/dummy.scm"))
;; ("abc" (call-with-output-string (lambda (out) (display #\a out) (display "bc" out))))
;; ("あbいc" (call-with-output-string (lambda (out) (display #\あ out) (display "bいc" out))))
;; ("(a unquote-splicing b)" (call-with-output-string (lambda (port) (write '(a unquote-splicing b) port))))
;; (#\あ (let ([port (open-string-input-port "あいう")]
;;             [org (current-input-port)])
;;         (set-current-input-port! port)
;;         (let ([ret (read-char)])
;;           (set-current-input-port! org)
;;           ret)))
;; (#\あ (let ([port (open-string-input-port "あいう")])
;;         (read-char port)))
;; (#\; (let ((port (open-input-file "/home/taro/vm/test-data.scm")))
;;            (read-char port)))
;; (3 (letrec ([a (lambda () b)]
;;             [b 3])
;;      4
;;      (a)))





;; ("abc" (list->string '(#\a #\b #\c)))
;; (";; don't edit start" (let ((port (open-input-file "/home/taro/vm/test-data.scm")))
;;            (read-line port)))
;; ("123" (let ([p (open-string-input-port "123\n456")]) (read-line p)))
;; ("Hello" (let ([port (open-output-file "/home/taro/vm/tmp.log")])
;;            (display "Hello\n" port)
;;            (close-output-port port)
;;            (let ((port (open-input-file "/home/taro/vm/tmp.log")))
;;              (read-line port))))



;; ((#t #t) (let ((port (open-input-file "/home/taro/vm/test-data.scm")))
;;            (read port)))
;; (1 (begin (define (afunc a . b) (if (= a 0) (apply afunc '(1 2 3)) a)) (afunc 0)))
;; ((2 3) (begin (define (bfunc a . b) (if (= a 0) (apply bfunc '(1 2 3)) b)) (bfunc 0)))
;; (3 (define val2 3) (define (func3) (define val2 4) val2) (func3) val2)

;; (9 (begin (define pon 3) (for-each (lambda (s) (set! pon (+ pon s))) '(1 2 3)) pon))
;; ;; Bignum as string
;; ("4294967296" (with-output-to-string (lambda () (display 4294967296))))                                       ; 0x100000000
;; ("18446744073709551616" (with-output-to-string (lambda () (display 18446744073709551616))))                   ; 0x10000000000000000
;; ("1208925819614629174706176" (with-output-to-string (lambda () (display 1208925819614629174706176))))         ; 0x100000000000000000000
;; ("309485009821345068724781056" (with-output-to-string (lambda () (display 309485009821345068724781056))))     ; 0x10000000000000000000000
;; ("4951760157141521099596496896" (with-output-to-string (lambda () (display 4951760157141521099596496896))))   ; 0x100000000000000000000000
;; ("79228162514264337593543950335" (with-output-to-string (lambda () (display 79228162514264337593543950335)))) ; 0xFFFFFFFFFFFFFFFFFFFFFFFF
;; ("79228162514264337593543950336" (with-output-to-string (lambda () (display 79228162514264337593543950336)))) ; 0x1000000000000000000000000
;; ("-79228162514264337593543950336" (with-output-to-string (lambda () (display -79228162514264337593543950336))))
;; ((a 3 4 5 6 b) `(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b))
;; (3 (apply (lambda () 3) '()))
;; (3 (acond [(+ 1 2)
;;           it]))
;; (else (acond [#f #f]
;;           [#t 'else]))

;; (error (car 3))

;; quasiquote test from R5RS start
;; ((a `(b ,x ,'y d) e) (let ((name1 'x)
;;                            (name2 'y))
;;                        `(a `(b ,,name1 ,',name2 d) e)))
;; ((list 3 4) (quasiquote (list (unquote (+ 1 2)) 4)))
;; ((a `(b ,(+ 1 2) ,(foo 4 d) e) f) `(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f))
;; (((foo 13) . cons) `((foo ,(+ 10 3)) ,@(cdr '(c)) . ,(car '(cons))))
;; (#(10 5 8 32 18 8) `#(10 5 ,((lambda (a) (* a 2)) 4) ,@(map (lambda (a) (* a 2)) '(16 9)) 8))
;; ;; quasiquote test from R5RS end

;; ((0 1 2 3 4 5)
;;  (match '(0 (1 2) (3 4 5))
;;    ((a (b c) (d e f))
;;    (list a b c d e f))))
;; ((number 123) (match 123
;;                 ((? string? x) (list 'string x))
;;                 ((? number? x) (list 'number x))))
;; (42 (match '(the answer is 42)
;;          (`(the answer is ,value) value)
;;          (else #f)))
;; ((3 2)
;; (map (match-lambda
;;          (('add num1 num2)
;;           (+ num1 num2))
;;        (('sub num1 num2)
;;         (- num1 num2))) '((add 1 2) (sub 4 2))))

;; ;; (((a b c) (0 1 2))
;; ;; (match-let (
;; ;;              (((ca . cd) ...)   '((a . 0) (b . 1) (c . 2)))
;; ;;            )
;; ;;   (list ca cd)))

;; ((0 1 2 3 4 5)
;;  (match '(0 (1 2) (3 4 5))
;;    ((a (b c) (d e f))
;;     (list a b c d e f))))

;; (((a c) (b d) (foo bar baz))
;;  (match-let1 ('let ((var val) ...) body ...)
;;      '(let ((a b) (c d)) foo bar baz)
;;    (list var val body)))






;; ;; error "library not imported"
;; [lib error
;; (library (higepon)
;;          (export hello)
;;          (import)
;;          (define hello "hello higepon\n"))
;; hello
;; ]

;; ;; error "symbol is not exported"
;; [lib error
;; (library (higepon)
;;          (export hello)
;;          (import)
;;          (define hello "hello higepon\n")
;;          (define goodbye "goodbye higepon\n"))
;; (import (higepon))
;; goodbye
;; ]


;; [lib error
;; (library (higepon)
;;          (export hello goodbye)
;;          (import)
;;          (define hello "hello higepon\n")
;;          (define goodbye "goodbye higepon\n"))
;; (import (only (higepon) hello))
;; goodbye
;; ]




;; [lib error
;; (library (higepon)
;;          (export hello goodbye)
;;          (import)
;;          (define hello "hello higepon\n")
;;          (define goodbye "goodbye higepon\n"))
;; (import (rename (higepon) (hello hige:hello)))
;; hello
;; ]


;; [lib "hello higepon\n"
;; (library (higepon)
;;          (export hello goodbye)
;;          (import)
;;          (define hello "hello higepon\n")
;;          (define goodbye "goodbye higepon\n"))
;; (import (prefix (higepon) higepon.))
;; higepon.hello
;; ]


;; [lib error
;; (library (higepon)
;;          (export hello goodbye)
;;          (import)
;;          (define hello "hello higepon\n")
;;          (define goodbye "goodbye higepon\n"))
;; (import (prefix (higepon) higepon.))
;; hello
;; ]


;; ;; macro is not exported, so error.
;; [lib error
;; (library (my-macro-lib)
;;          (export)
;;          (import)
;;          (define-macro (add3 v) `(+ 3 ,v)))
;; (library (test)
;;          (export val)
;;          (import (my-macro-lib))
;;          (define val (add3 2)))
;; (import (test))
;; val
;; ]
