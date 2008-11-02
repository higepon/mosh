;; don't edit start
(#t #t)
(mosh-only ("all-tests.scm"  12) (source-info '(3)))
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
((1 2 3) (receive a (call/cc (lambda (c) (c 1 2 3))) a))
((1 2) (receive a (call/cc (lambda (c) (c 1 2))) a))
((1) (receive a (call/cc (lambda (c) (c 1))) a))
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
(5 (call-with-values (lambda () (values 4 5))
                  (lambda (a b) b)))
(6 (call-with-values (lambda () (values 1 2 3)) (lambda (a b c) (+ a b c))))
((1 2 3)  (call-with-values (lambda () (values 1 2 3)) list))
(1235 (call-with-values (lambda () 1) (lambda (x) (+ x 1234))))

(mosh-only 6 (receive (a b c) (values 1 2 3) (+ a b c)))
(mosh-only (x y) (receive z (values 'x 'y) z))
(mosh-only (y z) (receive (a . b) (values 'x 'y 'z) b))
(mosh-only x (receive (a . b) (values 'x 'y 'z) a))
(mosh-only (1 2 3) (receive x (apply values '(1 2 3)) x))
(mosh-only (1 . 2) (call-with-values (lambda () (values 1 2)) cons))
;(error (call-with-values (lambda () (values 1 2)) (lambda (a b c) (+ a b c))))
(mosh-only "higepon" (receive (port proc) (open-string-output-port)
             (display "hige" port)
             (display "pon" port)
             (proc)))
("\"string\"" (call-with-string-output-port (lambda (port) (write "string" port))))
("123ABC456" (regexp-replace #/abc/ "123abc456" "ABC"))
;; ;;--------------------------------------------------------------------
;; ;;
;; ;; library
;; ;;
;; [lib "hello higepon\n"
;; (library (higepon)
;;          (export hello)
;;          (import)
;;          (define hello "hello higepon\n"))
;; (import (higepon))
;; hello
;; ]

;; [lib "hello higepon\n"
;; (library (higepon2)
;;          (export hello)
;;          (import)
;;          (define hello "hello higepon\n")
;;          (define goodbye "goodbye higepon\n"))
;; (import (higepon2))
;; hello
;; ]

;; [lib "goodbye higepon\n"
;; (library (higepon3)
;;          (export hello goodbye)
;;          (import)
;;          (define hello "hello higepon\n")
;;          (define goodbye "goodbye higepon\n"))
;; (import (higepon3))
;; hello
;; goodbye
;; ]

;; [lib "hello higepon\n"
;; (library (higepon4)
;;          (export hello goodbye)
;;          (import)
;;          (define hello "hello higepon\n")
;;          (define goodbye "goodbye higepon\n"))
;; (import (only (higepon4) hello))
;; hello
;; ]


;; [lib "goodbye higepon\n"
;; (library (higepon5)
;;          (export hello goodbye)
;;          (import)
;;          (define hello "hello higepon\n")
;;          (define goodbye "goodbye higepon\n"))
;; (import (except (higepon5) hello))
;; goodbye
;; ]


;; [lib "hello higepon\n"
;; (library (higepon6)
;;          (export hello goodbye)
;;          (import)
;;          (define hello "hello higepon\n")
;;          (define goodbye "goodbye higepon\n"))
;; (import (rename (higepon6) (hello hige:hello)))
;; hige:hello
;; ]

;; [lib "goodbye higepon\n"
;; (library (higepon7)
;;          (export hello goodbye)
;;          (import)
;;          (define hello "hello higepon\n")
;;          (define goodbye "goodbye higepon\n"))
;; (import (prefix (higepon7) higepon.))
;; higepon.goodbye
;; ]


;; [lib "lulululuhello higepon\n"
;; (library (higepon8)
;;          (export hello goodbye)
;;          (import)
;;          (define hello "hello higepon\n")
;;          (define goodbye "goodbye higepon\n"))
;; (library (ipod (3))
;;          (export play)
;;          (import)
;;          (define play "lulululu"))
;; (import (prefix (higepon8) higepon.)
;;         (ipod))
;; (string-append play higepon.hello)
;; ]


;; ;; we now ignore version
;; [lib "goodbye higepon\n"
;; (library (higepon9 (7))
;;          (export hello goodbye)
;;          (import)
;;          (define hello "hello higepon\n")
;;          (define goodbye "goodbye higepon\n"))
;; (import (prefix (higepon9 (6)) higepon.))
;; higepon.goodbye
;; ]


;; [lib "goodbye higepon\n"
;; (library (higepon10)
;;          (export hello goodbye)
;;          (import)
;;          (define hello "hello higepon\n")
;;          (define goodbye "goodbye higepon\n"))
;; (import (prefix (higepon10) higepon.))
;; higepon.goodbye
;; ]


;; [lib "goodbye higepon\nhi higepon\ncall\n"
;; (library (higepon11)
;;          (export hi
;;                  hello
;;                  (rename (goodbye bye) (call c)))
;;          (import)
;;          (define hello   "hello higepon\n")
;;          (define hi      "hi higepon\n")
;;          (define goodbye "goodbye higepon\n")
;;          (define call    "call\n"))
;; (import (higepon11))
;; (string-append bye hi c)
;; ]


;; [lib "lulululuhello higepon\n"
;; (library (ipod0 (3))
;;          (export play)
;;          (import)
;;          (define play "lulululu"))
;; (library (higepon12)
;;          (export hello goodby play)
;;          (import (ipod0))
;;          (define hello "hello higepon\n")
;;          (define goodbye "goodbye higepon\n"))
;; (import (higepon12))
;; (string-append play hello)
;; ]


;; ;; (ipod) library imported twice.
;; ;; Body of ipod must not be instatiated twice!
;; [lib "hello higepon\n"
;; (library (ipod2 (3))
;;          (export play)
;;          (import)
;;          (define play "lulululu"))
;; (library (higepon13)
;;          (export hello goodby play)
;;          (import (ipod2))
;;          (define hello "hello higepon\n")
;;          (define goodbye "goodbye higepon\n"))
;; (library (hagepon)
;;          (export hello)
;;          (import (ipod2)
;;                  (higepon13)))

;; (import (hagepon))
;; (string-append hello)
;; ]

;; [lib "lulululuhello higepon\n"
;; (library (ipod4 (3))
;;          (export play)
;;          (import)
;;          (define play "lulululu"))
;; (library (higepon14)
;;          (export hello goodby play)
;;          (import (for (ipod4) run))
;;          (define hello "hello higepon\n")
;;          (define goodbye "goodbye higepon\n"))
;; (import (higepon14))
;; (string-append play hello)
;; ]


;; [lib 5
;; (library (my-macro-lib)
;;          (export add3)
;;          (import)
;;          (define-macro (add3 v) `(+ 3 ,v)))

;; (library (test)
;;          (export val)
;;          (import (my-macro-lib))
;;          (define val (add3 2)))

;; (import (test))
;; val
;; ]

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
(mosh-only #f (eq? (list 'a) (list 'a)))
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
(mosh-only #(0 1 2 3 4) (do ((vec (make-vector 5))
                   (i 0 (+ i 1)))
                  ((= i 5) vec)
                (vector-set! vec i i)))
(mosh-only 25 (let ((x '(1 3 5 7 9)))
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
(mosh-only #f (not (list 3)))
(#t (not #f))
(#f (not '()))
(mosh-only #f (not (list)))
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
;; [mosh-only "no-error"
;;   (with-exception-handler
;;     (lambda (e)
;;       "error")
;;     (lambda () "no-error"))]
;; [mosh-only "error"
;;   (with-exception-handler
;;     (lambda (e)
;;       e)
;;     (lambda () (raise "error") 5))]
;; [mosh-only "error-is-string"
;;   (guard (con
;;           [(string? con)
;;            "error-is-string"]
;;           [else
;;            "error-is-not-string"])
;;          (raise "raise"))]
;; [mosh-only 3
;;   (guard (con
;;           [(string? con)
;;            "error-is-string"]
;;           [else
;;            "error-is-not-string"])
;;          3)]
;; [mosh-only "catched at parent"
;;  (guard (e
;;          [(symbol? e)
;;           "catched at parent"]
;;          [else
;;           "error"])
;;    (guard (con
;;            [(string? con)
;;             "error-is-string"]) ;; no else clause
;;           (raise 'symbol-error)))]
;; [mosh-only 7
;;   (guard (con
;;           [con
;;            4]
;;           [else
;;            "error-is-not-string"])
;;          (+ 3 (raise-continuable "warn continuation")))]
;; [mosh-only #t
;;   (vector-type?
;;    (make-vector-type 'test
;;                      #f
;;                      '()
;;                      '(#f #f)
;;                      #f))]
;; [mosh-only 1234
;;   (vector-type-data
;;    (make-vector-type 'test
;;                      #f
;;                      1234
;;                      '(#f #f)
;;                      #f))]
;; [mosh-only #t
;;   (let* ([vt (make-vector-type 'test #f #f '(#f #f) #f)]
;;          [constructor (typed-vector-constructor vt)]
;;          [pred?       (vector-type-predicate vt)])
;;     (pred? (constructor 3 4)))]
;; [mosh-only #t
;;   (let* ([parent (make-vector-type 'test-parent #f #f '(#f #f) #f)]
;;          [vt (make-vector-type 'test parent #f '(#f #f) #f)]
;;          [constructor (typed-vector-constructor vt)]
;;          [pred?       (vector-type-predicate parent)])
;;     (pred? (constructor 3 4)))]
;; [mosh-only 12
;;   (let* ([vt (make-vector-type 'test #f #f '(#f #f) #f)]
;;          [get-0 (typed-vector-accessor vt 0)]
;;          [get-1 (typed-vector-accessor vt 1)]
;;          [constructor (typed-vector-constructor vt)]
;;          [v (constructor 3 9)])
;;     (+ (get-0 v) (get-1 v)))]
;; [mosh-only 16
;;   (let* ([vt (make-vector-type 'test #f #f '(#f #f) #f)]
;;          [get-0 (typed-vector-accessor vt 0)]
;;          [get-1 (typed-vector-accessor vt 1)]
;;          [set-0 (typed-vector-mutator vt 0)]
;;          [constructor (typed-vector-constructor vt)]
;;          [v (constructor 3 9)])
;;     (set-0 v 7)
;;     (+ (get-0 v) (get-1 v)))]
;; [mosh-only #t
;;   (let* ([vt (make-vector-type 'test #f #f '(#f #f) #f)]
;;          [constructor (typed-vector-constructor vt)]
;;          [v (constructor 3 9)])
;;     (typed-vector? v))]
;; [mosh-only #t
;;   (let* ([vt (make-vector-type 'test #f #f '(#f #f) #f)]
;;          [constructor (typed-vector-constructor vt)]
;;          [v (constructor 3 9)])
;;     (eq? vt (typed-vector-type v)))]

[6
  (apply (lambda (a b c) (+ a b c)) 1 2 '(3))]
[6
  (apply (lambda (a b c) (+ a b c)) '(1 2 3))]
[6
  (apply (lambda (a b c) (+ a b c)) 1 '(2 3))]
[(2)
 (apply (lambda (x y) (apply y '((3 2)))) `(,car ,cdr))]
[3 (/ 6 2)]
[mosh-only 3 (mod 23 10)]
[mosh-only #t (even? 2)]
[mosh-only #f (even? 3)]
[mosh-only #f (for-all even? '(3 1 4 1 5 9))]
[mosh-only #f (for-all even? '(3 1 4 1 5 9 . 2))]
[mosh-only #t (for-all even? '(2 4 14))]
;[14 (for-all (lambda (n) (and (even? n) n))
;             '(2 4 14))]
[mosh-only #t (for-all (lambda (a b) (< a b)) '(1 2 3) '(2 3 4))]
[mosh-only #f (for-all (lambda (a b) (< a b)) '(1 2 4) '(2 3 4))]

;; rational
[mosh-only 1 (+ (/ 2) (/ 4) (/ 4))]
[mosh-only 0 (- (/ 1 2) (/ 1 4) (/ 1 4))]
[mosh-only #t (= (/ 3 2) (+ (/ 1 2) 1))]
[mosh-only #t (= (/ 5 2) (+ 1 (/ 1 2) 1))]
[mosh-only #t (= (/ 3 2) (- 3 (/ 1 2) 1))]
[mosh-only 3 (* (/ 3 2) 2)]
[mosh-only 3 (* 2 (/ 3 2))]
[mosh-only 3 (* (/ 4 2) (/ 3 2))]
[mosh-only 2 (/ (/ 2 2) (/ 1 2))]
[mosh-only 2 (/ (/ 4 2) 1)]
[error (/ 1 0)]
[error (/ 1 (* 0 (/ 1 2)))]
[mosh-only #t (> 1 (/ 1 2))]
[mosh-only #f (> (/ 1 2) 1)]
[mosh-only #t (> 1 (/ 1 2))]
[mosh-only #t (> (/ 1 2) (/ 1 3))]
[mosh-only #t (<= (/ 1 2) 1)]
[mosh-only #t (>= 1 (/ 1 2))]
[mosh-only #t (>= (/ 1 2) (/ 1 3))]
[mosh-only #t (< (/ 1 2) 1)]
[mosh-only #f (< 1 (/ 1 2))]
[mosh-only #f (< (/ 1 2) (/ 1 3))]
[mosh-only #t (<= (/ 1 2) 1)]
[mosh-only #f (<= 1 (/ 1 2))]
[mosh-only #f (<= (/ 1 2) (/ 1 3))]
[mosh-only #t (= (/ 2 2) 1)]
[mosh-only #t (= 1 (/ 2 2))]
[mosh-only #t (= (/ 1 2) (/ 2 4))]
[mosh-only #t (>= (/ 1 2) (inexact (/ 1 3)))]
[mosh-only #t (> (/ 3 2) (+ (inexact (/ 1 3)) (inexact (/ 1 3)) (inexact (/ 1 3))) (/ 99 100))]
[mosh-only #t (> 1 (/ (inexact 98) 100) (/ 97 100))]
[mosh-only #t  (rational? 3)]
[mosh-only #t  (rational? (/ 1 4))]
;[mosh-only 1/2 (/ 2)]
;[mosh-only 1/3 (/ 3)]
;[mosh-only 0 (/ 0)] -> division by zero

[mosh-only #t  (number? 3)]
[mosh-only #t  (number? (/ 1 4))]

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
["abc" "a\
        b\
        c"]
;; match
[mosh-only (0 1 2 3 4 5)
 (match '(0 (1 2) (3 4 5))
   ((a (b c) (d e f))
    (list a b c d e f)))]
[mosh-only (number 123)
 (match 123
   ((? string? x) (list 'string x))
   ((? number? x) (list 'number x)))]
[mosh-only "normal let, vars=(a c) exprs=(b d)"
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
[mosh-only "named let, vars=(x y) exprs=((f a b) (f c d))"
 (let-analyzer '(let foo ((x (f a b)) (y (f c d))) e f g))]
[mosh-only "malformed let"
 (let-analyzer '(let (a) b c d))]
[mosh-only 42
 (match '(the answer is 42)
   (`(the answer is ,value) value)
   (else #f))]
[mosh-only #f
 (match '(the answer was 42)
   (`(the answer is ,value) value)
   (else #f))]
[mosh-only d
 (match '(a b c d)
   ((the answer is value) value)
   (else #f))]
[mosh-only "base=mosh suffix=scm"
 (match "mosh.scm"
  ((? string? (= #/(.*)\.([^.]+)$/ m))
   (format "base=~a suffix=~a" (m 1) (m 2))))]
;; do
[mosh-only 2
 (do ((i 0) (j 0)) ((zero? j) (set! i 1) (set! i 2) i))]

;; case
[composite
 (case (* 2 3)
   ((2 3 5 7) 'prime)
   ((1 4 6 8 9) 'composite))]
[consonant
 (case (car '(c d))
   ((a e i o u) 'vowel)
   ((w y) 'semivowel)
   (else 'consonant))]
[0 (case 1 ((2 1) 0))]
[0 (case 2 ((2 1) 0))]

;; procedure?
[mosh-only
 #t (procedure? car)]
[mosh-only
 #f (procedure? 'car)]
[mosh-only
 #t (procedure? (lambda (x) (* x x)))]
[mosh-only
 #f (procedure? '(lambda (x) (* x x)))]
[#t (char>=? #\b #\a)]
[mosh-only #t (char>=? #\c #\b #\a)]
[#t (char>=? #\b #\b)]
[#f (char>=? #\b #\c)]
[#t (char>? #\b #\a)]
[#f (char>? #\b #\b)]
[#f (char>? #\b #\c)]
[#t (char<=? #\a #\b)]
[#t (char<=? #\b #\b)]
[#f (char<=? #\c #\b)]
[#t (char<? #\a #\b)]
[#f (char<? #\b #\b)]
[#f (char<? #\c #\b)]
[mosh-only (1 2 3 . 4) (cons* 1 2 3 4)]
[mosh-only 1 (cons* 1)]
[mosh-only (1 . 3) (receive (x y) (car+cdr '(1 . 3)) (cons x y))]
[1 (append 1)]
[(1 . 2) (append '(1) 2)]
[(1  2 . 3) (append '(1 2) 3)]
[(1  2  3) (append '(1 2) '(3))]
[(1  2  3 . 4) (append '(1 2) '(3) 4)]
[(1  2  3 . 4) (append '(1 2) '(3) 4)]
[1 (append '() 1)]
[(1) (append '(1) '())]
[1 (append! 1)]
[(1 . 2) (append! '(1) 2)]
[(1  2 . 3) (append! '(1 2) 3)]
[(1  2  3) (append! '(1 2) '(3))]
[(1  2  3 . 4) (append! '(1 2) '(3) 4)]
[(1  2  3 . 4) (append! '(1 2) '(3) 4)]
[1 (append! '() 1)]
[(1) (append! '(1) '())]

[mosh-only (a b) (take '(a b c d e)  2)]
[mosh-only  (c d e) (drop '(a b c d e)  2)]
[mosh-only (1 2) (take '(1 2 3 . d) 2)]
[mosh-only (3 . d) (drop '(1 2 3 . d) 2)]
[mosh-only (1 2 3)(take '(1 2 3 . d) 3)]
[mosh-only d (drop '(1 2 3 . d) 3)]
[mosh-only (d e) (take-right '(a b c d e) 2)]
[mosh-only (a b c) (drop-right '(a b c d e) 2)]
[mosh-only (2 3 . d) (take-right '(1 2 3 . d) 2)]
[mosh-only (1) (drop-right '(1 2 3 . d) 2)]
[mosh-only d (take-right '(1 2 3 . d) 0)]
[mosh-only (1 2 3) (drop-right '(1 2 3 . d) 0)]

[mosh-only (a b c) (xcons '(b c) 'a)]
[mosh-only (c c c c) (make-list 4 'c)]
[mosh-only (0 1 2 3) (list-tabulate 4 values)]
[mosh-only (1 2 3 4) (list-copy '(1 2 3 4))]
[mosh-only z (let1 lst (circular-list 'z 'q)
      (and (eq? (first lst) 'z) (eq? (second lst) 'q) (third lst) 'z))]
[mosh-only #t (proper-list? '())]
[mosh-only #t (proper-list? '(1 2 3))]
[mosh-only #f (proper-list? '(1 . 3))]
[mosh-only #t (dotted-list? '(1 2 . 3))]
[mosh-only #f (dotted-list? '(1 2 3))]
[mosh-only (c) (last-pair '(a b c))]
[mosh-only c (last '(a b c))]
[mosh-only #t (not-pair? 3)]
[mosh-only #f (not-pair? '(1 2))]
[mosh-only #t (list= eq?)]
[mosh-only #t (list= eq? '(a))]
[mosh-only #t (list= eq? '(a) '(a))]
[mosh-only #f (list= eq? '(a) '(a b))]
[mosh-only ((a b c) d e f g h) (receive (x y) (split-at '(a b c d e f g h) 3) (cons x y))]
["123" (string #\1 #\2 #\3)]
["taro&amp;hanako" (regexp-replace-all #/&/ "taro&hanako" "&amp;")]
[mosh-only 2 (let1 ht (make-hashtable (lambda (x) 2) ;; always 2
                                       (lambda (a b) #t))
               (hashtable-set! ht 1 1)
               (hashtable-set! ht 2 2)
               (hashtable-ref ht 1))]
[mosh-only "apple" (let1 ht (make-hashtable string-hash
                                             string=?)
                     (hashtable-set! ht "my" "apple")
                     (hashtable-set! ht "our" "water")
                     (hashtable-ref ht "my"))]
[mosh-only #t (hashtable? (make-hashtable string-hash string=?))]
[mosh-only #t (hashtable? (make-eq-hashtable))]
[mosh-only #f (hashtable? '(a . b))]
[mosh-only 2 (let1 ht (make-hashtable string-hash string=?)
                 (hashtable-set! ht "my" "apple")
                 (hashtable-set! ht "our" "water")
                 (hashtable-size ht))]
[mosh-only 2 (let1 ht (make-eq-hashtable)
                 (hashtable-set! ht "my" "apple")
                 (hashtable-set! ht "my" "apple")
                 (hashtable-size ht))]
[mosh-only #f (let1 ht (make-eq-hashtable)
                (hashtable-set! ht 1 "one")
                (hashtable-delete! ht 1)
                (hashtable-ref ht 1 #f))]
[mosh-only #f (let1 ht (make-hashtable string-hash string=?)
                (hashtable-set! ht "one" 1)
                (hashtable-delete! ht "one")
                (hashtable-ref ht "one" #f))]
[mosh-only #f (let1 ht (make-eq-hashtable)
                (hashtable-set! ht 1 "one")
                (hashtable-contains? ht 2))]
[mosh-only #t (let1 ht (make-eq-hashtable)
                (hashtable-set! ht 1 "one")
                (hashtable-contains? ht 1))]
[mosh-only #f (let1 ht (make-hashtable string-hash string=?)
                (hashtable-set! ht "one" 1)
                (hashtable-contains? ht "two"))]
[mosh-only #t (let1 ht (make-hashtable string-hash string=?)
                (hashtable-set! ht "one" 1)
                (hashtable-contains? ht "one"))]
[mosh-only "!one!!hige!" (let1 ht (make-hashtable string-hash string=?)
                           (hashtable-set! ht "one" "one")
                           (hashtable-update! ht "one" (lambda (x) (string-append "!" x "!")) "hige")
                           (hashtable-update! ht "two" (lambda (x) (string-append "!" x "!")) "hige")
                           (string-append (hashtable-ref ht "one") (hashtable-ref ht "two")))]
[mosh-only #t (let1 ht (make-eq-hashtable)
                (hashtable-set! ht 1 "one")
                (let1 ht-copy (hashtable-copy ht)
                  (and (string=? (hashtable-ref ht-copy 1) "one") (not (hashtable-mutable? ht-copy)))))]
[mosh-only #t (let1 ht (make-eq-hashtable)
                (hashtable-set! ht 1 "one")
                (let1 ht-copy (hashtable-copy ht #t)
                  (and (string=? (hashtable-ref ht-copy 1) "one") (hashtable-mutable? ht-copy))))]
[mosh-only #t (let1 ht (make-hashtable string-hash string=?)
                (hashtable-set! ht "one" "one")
                (let1 ht-copy (hashtable-copy ht)
                  (and (string=? (hashtable-ref ht-copy "one") "one") (not (hashtable-mutable? ht-copy)))))]
[mosh-only #t (let1 ht (make-hashtable string-hash string=?)
                (hashtable-set! ht "one" "one")
                (let1 ht-copy (hashtable-copy ht #t)
                  (and (string=? (hashtable-ref ht-copy "one") "one") (hashtable-mutable? ht-copy))))]
[mosh-only 0 (let1 ht (make-eq-hashtable)
               (hashtable-set! ht 1 "one")
               (hashtable-set! ht 2 "two")
               (hashtable-clear! ht)
                (hashtable-size ht))]
[mosh-only 0 (let1 ht (make-hashtable string-hash string=?)
               (hashtable-set! ht "one" 1)
               (hashtable-set! ht "two" 2)
               (hashtable-clear! ht)
                (hashtable-size ht))]
[mosh-only (1 2) (let1 ht (make-eq-hashtable)
                    (hashtable-set! ht 1 "one")
                    (hashtable-set! ht 2 "two")
                    (vector->list (hashtable-keys ht)))]
[mosh-only #t (let1 ht (make-hashtable string-hash string=?)
                            (hashtable-set! ht "one" 1)
                            (hashtable-set! ht "two" 2)
                            (let1 keys (vector->list (hashtable-keys ht))
                              (and (member "one" keys)
                                   (member "two" keys)
                                   (= 2 (length keys)))))]
[mosh-only ("two" "one" 2 1)
           (let1 ht (make-hashtable string-hash string=?)
             (hashtable-set! ht "one" 1)
             (hashtable-set! ht "two" 2)
             (receive (keys vals) (hashtable-entries ht)
               (append (vector->list keys)
                       (vector->list vals))))]
[mosh-only #t (equal? eq? (hashtable-equivalence-function (make-eq-hashtable)))]
[mosh-only #f (hashtable-hash-function (make-eq-hashtable))]

[mosh-only #t (equal? string=? (hashtable-equivalence-function (make-hashtable string-hash string=?)))]
[mosh-only #t (equal? string-hash (hashtable-hash-function (make-hashtable string-hash string=?)))]
[mosh-only #t (= (string-ci-hash "abc") (string-ci-hash "AbC"))]
[mosh-only #t (= (symbol-hash 'abc) (symbol-hash 'abc))]
[mosh-only #f (= (symbol-hash 'abc) (symbol-hash 'aBc))]
[mosh-only #t (= (equal-hash '(a b c)) (equal-hash '(a b c)))]
[mosh-only #t (equal? eqv? (hashtable-equivalence-function (make-eqv-hashtable)))]

;; record procedual layer
[mosh-only #t (let* ([:point (make-record-type-descriptor 'point #f #f #f #f
                                                          '#((mutable x) (mutable y)))]
                     [:point-cd (make-record-constructor-descriptor :point #f #f)]
                     [make-point (record-constructor :point-cd)]
                     [point? (record-predicate :point)]
                     [point-x (record-accessor :point 0)]
                     [point-y (record-accessor :point 1)]
                     [point-x-set! (record-mutator :point 0)]
                     [point-y-set! (record-mutator :point 1)]
                     [p1 (make-point 1 2)])
                (and (point? p1)
                     (record? p1)
                     (= (point-x p1) 1)
                     (= (point-y p1) 2)
                     (point-x-set! p1 5)
                     (= (point-x p1) 5)))]
[mosh-only #t (let* ([:point (make-record-type-descriptor 'point #f #f #f #f
                                                          '#((mutable x) (mutable y)))]
                     [:point2 (make-record-type-descriptor 'point2 :point #f #f #f
                                                           '#((mutable x) (mutable y)))]
                     [make-point2 (record-constructor (make-record-constructor-descriptor :point2 #f #f))]
                     [point? (record-predicate :point)]
                     [point-x (record-accessor :point 0)]
                     [point-y (record-accessor :point 1)]
                     [point-x-set! (record-mutator :point 0)]
                     [point-y-set! (record-mutator :point 1)]
                     [point2? (record-predicate :point2)]
                     [point2-xx (record-accessor :point2 0)]
                     [point2-yy (record-accessor :point2 1)]
                     [point2-xx-set! (record-mutator :point2 0)]
                     [point2-yy-set! (record-mutator :point2 1)]
                     [p2 (make-point2 1 2 3 4)])
                (and (point? p2)
                     (point2? p2)
                     (record? p2)
                     (= (point-x p2) 1)
                     (= (point-y p2) 2)
                     (= (point2-xx p2) 3)
                     (= (point2-yy p2) 4)
                     (point-x-set! p2 5)
                     (= (point-x p2) 5)
                     (point-y-set! p2 6)
                     (= (point-y p2) 6)
                     (point2-xx-set! p2 7)
                     (= (point2-xx p2) 7)
                     (point2-yy-set! p2 8)
                     (= (point2-yy p2) 8)
))]
[mosh-only #t
           (let* ([:point (make-record-type-descriptor 'point #f #f #f #f
                                                       '#((mutable x) (mutable y)))]
                  [:point-cd/abs (make-record-constructor-descriptor
                                  :point #f
                                  (lambda (new)
                                    (lambda (x y)
                                      (new (abs x) (abs y)))))]
                  [point-x (record-accessor :point 0)]
                  [point-y (record-accessor :point 1)]
                  [make-point/abs
                   (record-constructor :point-cd/abs)])
             (and
              (= (point-x (make-point/abs -1 -2)) 1)
              (= (point-y (make-point/abs -1 -2)) 2))
)]
[mosh-only #t
           (let* ([color->rgb (lambda (c) (cons 'rgb c))]
                  [:point (make-record-type-descriptor 'point #f #f #f #f
                                                       '#((mutable x) (mutable y)))]
                  [:point-cd (make-record-constructor-descriptor :point #f #f)]
                  [:point-cd/abs (make-record-constructor-descriptor
                                  :point #f
                                  (lambda (new)
                                    (lambda (x y)
                                      (new (abs x) (abs y)))))]
                  [point-x (record-accessor :point 0)]
                  [:cpoint (make-record-type-descriptor
                            'cpoint :point
                            #f #f #f
                            '#((mutable rgb)))]
                  [cpoint-rgb
                   (record-accessor :cpoint 0)]
                  [make-cpoint(record-constructor
                               (make-record-constructor-descriptor
                                :cpoint :point-cd
                                (lambda (p)
                                  (lambda (x y c)
                                    ((p x y) (color->rgb c))))))]
                  [make-cpoint/abs (record-constructor
                                    (make-record-constructor-descriptor
                                     :cpoint :point-cd/abs
                                     (lambda (p)
                                       (lambda (x y c)
                                         ((p x y) (color->rgb c))))))])
             (and (equal? (cpoint-rgb (make-cpoint -1 -3 'red)) '(rgb . red))
                  (= (point-x (make-cpoint -1 -3 'red)) -1)
                  (= (point-x (make-cpoint/abs -1 -3 'red)) 1))
)]
;; opaque
[mosh-only #f (let* ([:point (make-record-type-descriptor 'point #f #f #f #t
                                                          '#((mutable x) (mutable y)))]
                     [:point-cd (make-record-constructor-descriptor :point #f #f)]
                     [make-point (record-constructor :point-cd)]
                     [p (make-point 1 2)])
                (record? p))]
[mosh-only #t (let* ([:point (make-record-type-descriptor 'point #f #f #f #f ; #t raise assertion
                                                          '#((mutable x) (mutable y)))]
                     [:point-cd (make-record-constructor-descriptor :point #f #f)]
                     [make-point (record-constructor :point-cd)]
                     [p (make-point 1 2)])
                (eqv? :point (record-rtd p)))]
[mosh-only point (let* ([:point (make-record-type-descriptor 'point #f #f #f #f ; #t raise assertion
                                                              '#((mutable x) (mutable y)))]
                         [:point-cd (make-record-constructor-descriptor :point #f #f)]
                         [make-point (record-constructor :point-cd)]
                         [p (make-point 1 2)])
                   (record-type-name (record-rtd p)))]
[mosh-only point (let* ([:point (make-record-type-descriptor 'point #f #f #f #f
                                                             '#((mutable x) (mutable y)))]
                        [:point2 (make-record-type-descriptor 'point2 :point #f #f #f
                                                              '#((mutable x) (mutable y)))])
                   (record-type-name (record-type-parent :point2)))]
[mosh-only #t (let* ([:point (make-record-type-descriptor 'point #f 'point-uid #f #f
                                                             '#((mutable x) (mutable y)))]
                        [:point2 (make-record-type-descriptor 'point #f 'point-uid #f #f
                                                              '#((mutable x) (mutable y)))])
                   (eq? :point :point2))]
[mosh-only #t (let* ([:point2 (make-record-type-descriptor 'point2 #f #f #f #f
                                                           '#((mutable x) (mutable y)))])
                   (record-type-generative? :point2))]
[mosh-only #f (let* ([:point (make-record-type-descriptor 'point #f 'point-uid #f #f
                                                             '#((mutable x) (mutable y)))])
                   (record-type-generative? :point))]
[mosh-only #(x y) (let* ([:point (make-record-type-descriptor 'point #f 'point-uid #f #f
                                                             '#((mutable x) (mutable y)))])
                (record-type-field-names :point))]
[mosh-only #t (let* ([:point (make-record-type-descriptor 'point #f 'point-uid #f #f
                                                             '#((mutable x) (mutable y)))])
                (record-field-mutable? :point 1))]

;; dynamic-wind start
[mosh-only (connect talk1 disconnect connect talk2 disconnect)
           (let ((path '())
                 (c '()))
             (let ((add (lambda (s) (set! path (cons s path)))))
               (dynamic-wind
                   (lambda () (add 'connect))
                   (lambda ()
                     (add (call-with-current-continuation
                           (lambda (c0) (set! c c0) 'talk1))))
                   (lambda () (add 'disconnect)))
               (if (< (length path) 4)
                   (c 'talk2)
                   (reverse path))))]
[mosh-only (3 connect talk1 disconnect connect talk2 disconnect 1) ;; from Gauche
  (let* ([c '()]
         [dynwind-test1
          (lambda ()
            (let ((path '()))
              (let ((add (lambda (s) (set! path (cons s path)))))
                (dynamic-wind
                    (lambda () (add 'connect))
                    (lambda ()
                      (add (call-with-current-continuation
                            (lambda (c0) (set! c c0) 'talk1))))
                    (lambda () (add 'disconnect)))
                (if (< (length path) 4)
                    (c 'talk2)
                    (reverse path)))))]
          [dynwind-test2
           (lambda ()
             (let ((path '()))
               (dynamic-wind
                   (lambda () (set! path (cons 1 path)))
                   (lambda () (set! path (append (dynwind-test1) path)))
                   (lambda () (set! path (cons 3 path))))
               path))])
    (dynwind-test2))]
[mosh-only (a b c d e f g b c d e f g h)
             (let ((x '())
                   (c #f))
               (dynamic-wind
                   (lambda () (set! x (cons 'a x)))
                   (lambda ()
                     (dynamic-wind
                         (lambda () (set! x (cons 'b x)))
                         (lambda ()
                           (dynamic-wind
                               (lambda () (set! x (cons 'c x)))
                               (lambda () (set! c (call/cc (lambda (x) x))))
                               (lambda () (set! x (cons 'd x)))))
                         (lambda () (set! x (cons 'e x))))
                     (dynamic-wind
                         (lambda () (set! x (cons 'f x)))
                         (lambda () (when c (c #f)))
                         (lambda () (set! x (cons 'g x)))))
                   (lambda () (set! x (cons 'h x))))
               (reverse x))]
[mosh-only (a b c) ;; multiple values from Gauche
      ((lambda ()
        (receive x
            (dynamic-wind (lambda () #f)
                          (lambda () (values 'a 'b 'c))
                          (lambda () #f))
          x)))]
[mosh-only () ;; multiple values from Gauche
    ((lambda ()
      (receive x
        (dynamic-wind (lambda () #f)
            (lambda () (values))
            (lambda () #f))
      x)))]
[mosh-only 3 (eval 3 '())]
[#t (equal? '(#(1 2 3)  . #(one two three)) '(#(1 2 3)  . #(one two three)))]

[error (call/cc (lambda (cont)
                  (with-exception-handler
                   (lambda (c)
                     (cont 'error))
                   (lambda () (car 3)))))]

[error (car 3)]
[error (cdr 3)]
[error (cddr 3)]
[error (cdar 3)]
[error (cadr 3)]
[error (caar 3)]
[error (values 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 )]
[error (apply values '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))]
[error (apply (lambda (s) (display s)) '(1 2))]
[error (number? 2 2)]
[error (make-record-constructor-descriptor)]
[error (hashtable-clear! 1 1 1)]
[error (record-predicate 3)]
[error (record-constructor 3)]
[error (make-record-type-descriptor 'point 3 'uid #f #f #f)]
[error (let* ([:point (make-record-type-descriptor 'point #f #f #f #f
                                                '#((mutable x) (mutable y)))]
           [:point-cd (make-record-constructor-descriptor :point #f #f)]
           [make-point (record-constructor :point-cd)]
           [point-x (record-accessor :point 3)])
      #f)]
[error (let* ([:point (make-record-type-descriptor 'point #f #f #f #f
                                                '#((immutable x) (mutable y)))]
           [:point-cd (make-record-constructor-descriptor :point #f #f)]
           [make-point (record-constructor :point-cd)]
           [point-x (record-mutator :point 0)])
      #f)]
[error (let* ([:point (make-record-type-descriptor 'point #f #f #f #f
                                               '#((immutable x) (mutable y)))]
          [:point2 (make-record-type-descriptor 'point2 #f #f #f #f
                                                '#((immutable x) (mutable y)))]
          [:point-cd (make-record-constructor-descriptor :point #f #f)]
          [:point2-cd (make-record-constructor-descriptor :point2 #f #f)]
          [make-point (record-constructor :point-cd)]
          [point2-x (record-accessor :point2 0)])
            (point2-x (make-point 1 2)))
          ]
[error (let* ([:point (make-record-type-descriptor 'point #f #f #t #t
                                               '#((immutable x) (mutable y)))]
          [:point-cd (make-record-constructor-descriptor :point #f #f)]
          [make-point (record-constructor :point-cd)])
     (record-rtd (make-point 2 21)))]

[error (apply read-char (current-input-port))] ;; the argument should be a list.
[error (rxmatch-start (rxmatch #/\d+/ "a345a") 5)]
;[error (and #/(?<hage >.*)/ #t)] ;; invalid regexp group name
[error (string-ref "hige" 5)]
[error (open-file-input-port "not-exist-path/////xxx")]
[error (open-file-output-port "not-exist-path/////xxx")]
[error (format "~a ~a" 1)]
[error  my-unbound-variable]
[error  (my-unbound-variable)]
[error (vector-ref 'v 3)]
[error (receive (a) (values 1 2))]
[error (receive (a b) (values 1))]
[error (3)]
;[error (set! my-unbound-variable 3)]
[error (/ 3 0)]
[error (error "our" "message")]
[#t (let ([x (list 'a 'b 'c 'a)]
         [y (list 'a 'b 'c 'a 'b 'c 'a)])
     (set-cdr! (list-tail x 2) x)
     (set-cdr! (list-tail y 5) y)
     (equal? x y))] ;; circular equal?
(#t (apply = '(3 3)))
(#f (apply = '(3 4)))
(#t (apply = '(3 3 3 3)))
(#f (apply = '(3 3 3 4)))
(#f (apply = '(4 3 3 3)))
(error (apply = '(4 'z 3 3)))
(#t (apply > '(4 3)))
(#t (apply > '(4 3 2)))
(#f (apply > '(4 3 1 2)))
(#t (apply >= '(3 3 3)))
(#t (apply >= '(4 3 3)))
(#t (apply >= '(4 3)))
(#t (apply < '(1 2)))
(#t (apply < '(1 2 3)))
(#f (apply < '(1 5 3)))
(#t (apply <= '(1 2)))
(#t (apply <= '(1 2 3)))
(#t (apply <= '(1 3 3)))
(#f (apply <= '(1 5 3)))
(error (+ 1 2 'a))
(error (/ 1 0))
(4  (apply + '(4)))
(7  (apply + '(4 3)))
(17 (apply + '(4 3 10)))
(4 (apply + '(1 1 1 1)))
(5 (apply - '(10 5)))
(3 (apply - '(10 5 2)))
(6 (apply * '(2 3)))
(24 (apply * '(2 3 4)))
(3 (apply / '(6 2)))
(error (apply / '(6 0)))
((0 1 2 #f) (let* ((e (make-enumeration '(red green blue)))
                   (i (enum-set-indexer e)))
              (list (i 'red) (i 'green) (i 'blue) (i 'yellow))))

((red green blue) (enum-set->list (make-enumeration '(red green blue))))

((red blue) (let* ((e (make-enumeration '(red green blue)))
                   (c (enum-set-constructor e)))
              (enum-set->list (c '(blue red)))))

((#t #f #t #t #f #t) (let* ((e (make-enumeration '(red green blue)))
                            (c (enum-set-constructor e)))
                       (list
                        (enum-set-member? 'blue (c '(red blue)))
                        (enum-set-member? 'green (c '(red blue)))
                        (enum-set-subset? (c '(red blue)) e)
                        (enum-set-subset? (c '(red blue)) (c '(blue red)))
                        (enum-set-subset? (c '(red blue)) (c '(red)))
                        (enum-set=? (c '(red blue)) (c '(blue red))))))

(error (guard [c (#t 'error)]
          (let* ((e (make-enumeration '(red green blue)))
                 (c (enum-set-constructor e)))
            (c '(pink)))))

(((red green blue)
  (red green blue)
  1 #t #f #t #t #f #t #t #f #f #t #t #f #f #t)
  (let* ((e (make-enumeration '(red green blue)))
         (r ((enum-set-constructor e) '(red))))
    (list
     (enum-set->list (enum-set-universe e))
     (enum-set->list (enum-set-universe r))
     ((enum-set-indexer
          ((enum-set-constructor e) '(red)))
         'green)
     (enum-set-member? 'red e)
     (enum-set-member? 'black e)
     (enum-set-subset? e e)
     (enum-set-subset? r e)
     (enum-set-subset? e r)
     (enum-set-subset? e (make-enumeration '(blue green red)))
     (enum-set-subset? e (make-enumeration '(blue green red black)))
     (enum-set-subset? (make-enumeration '(blue green red black)) e)
     (enum-set-subset? ((enum-set-constructor
                         (make-enumeration '(blue green red black)))
                        '(red)) e)
     (enum-set-subset? ((enum-set-constructor
                         (make-enumeration '(green red)))
                        '(red))
                       e)
     (enum-set=? e e)
     (enum-set=? r e)
     (enum-set=? e r)
     (enum-set=? e (make-enumeration '(blue green red))))))
((#t #f #t #t #f #t) (let* ((e (make-enumeration '(red green blue)))
                             (c (enum-set-constructor e)))
                        (list
                         (enum-set-member? 'blue (c '(red blue)))
                         (enum-set-member? 'green (c '(red blue)))
                         (enum-set-subset? (c '(red blue)) e)
                         (enum-set-subset? (c '(red blue)) (c '(blue red)))
                         (enum-set-subset? (c '(red blue)) (c '(red)))
                         (enum-set=? (c '(red blue)) (c '(blue red))))))

((red blue) (let* ((e (make-enumeration '(red green blue)))
                   (c (enum-set-constructor e)))
              (enum-set->list (c '(blue red)))))

(((red blue) (red) (green)) (let* ((e (make-enumeration '(red green blue)))
          (c (enum-set-constructor e)))
     (list
      (enum-set->list
       (enum-set-union (c '(blue)) (c '(red))))
      (enum-set->list
       (enum-set-intersection (c '(red green))
                              (c '(red blue))))
      (enum-set->list
       (enum-set-difference (c '(red green))
                            (c '(red blue)))))))

((green blue) (let* ((e (make-enumeration '(red green blue)))
          (c (enum-set-constructor e)))
     (enum-set->list
      (enum-set-complement (c '(red))))))

((red black) (let ((e1 (make-enumeration
              '(red green blue black)))
         (e2 (make-enumeration
              '(red black white))))
     (enum-set->list
      (enum-set-projection e1 e2))))

;; reader
(error (call-with-string-input-port
        "("
        (lambda (in)
          (read in))))

(error (call-with-string-input-port
        "("
        (lambda (in)
          (apply read (list in)))))
(#t (eq? #\A #\x41))
(mosh-only 3 (bytevector-length (make-bytevector 3)))
(mosh-only #vu8(3 3 3 3) (make-bytevector 4 3))
(mosh-only #t (bytevector=? #vu8(3 3 3) #vu8(3 3 3)))
(mosh-only #f (bytevector=? #vu8(3 4 3) #vu8(3 3 3)))
(mosh-only #vu8(3 3 3 3) (let ([bytevector (make-bytevector 4)])
                     (bytevector-fill! bytevector 3)
                     bytevector))
(#vu8(1 2 3 1 2 3 4 8)
     (let ((b #vu8(1 2 3 4 5 6 7 8)))
       (bytevector-copy! b 0 b 3 4)
       b))
(#vu8(1 2 3 4) (bytevector-copy #vu8(1 2 3 4)))
(255 (bytevector-u8-ref #vu8(255) 0))
(255 (bytevector-u8-ref (make-bytevector 1 -1) 0))
(255 (bytevector-u8-ref (make-bytevector 1 255) 0))
(255 (let ([bytevector (make-bytevector 1)])
       (bytevector-fill! bytevector -1)
       (bytevector-u8-ref  bytevector 0)))
(-1 (bytevector-s8-ref #vu8(255) 0))
(-1 (bytevector-s8-ref (make-bytevector 1 -1) 0))
(-1 (bytevector-s8-ref (make-bytevector 1 255) 0))
(-1 (let ([bytevector (make-bytevector 1)])
       (bytevector-fill! bytevector -1)
       (bytevector-s8-ref  bytevector 0)))
(255 (let ([b (make-bytevector 1)])
       (bytevector-u8-set! b 0 255)
       (bytevector-u8-ref b 0)))
(255 (let ([b (make-bytevector 1)])
       (bytevector-s8-set! b 0 -1)
       (bytevector-u8-ref b 0)))
(error (bytevector-u8-ref 1 (make-bytevector 1)))
(error (bytevector-s8-ref 1 (make-bytevector 1)))
(error (bytevector-u8-ref -1 (make-bytevector 1)))
(error (bytevector-s8-ref -1 (make-bytevector 1)))
(error (bytevector-u8-set! 1 (make-bytevector 1)) 1)
(error (bytevector-s8-set! 1 (make-bytevector 1)) 1)
(error (bytevector-u8-set! -1 (make-bytevector 1)) 1)
(error (bytevector-s8-set! -1 (make-bytevector 1)) 1)
(#vu8(1 2 3 4) (u8-list->bytevector '(1 2 3 4)))
(error (u8-list->bytevector '(1 2 3 -1 4)))
((1 2 3 4) (bytevector->u8-list #vu8(1 2 3  4)))
(65023 (bytevector-u16-ref #vu8(255 255 255 255 255 255 255 255
                                    255 255 255 255 255 255 255 253) 14 'little))
(-513 (bytevector-s16-ref #vu8(255 255 255 255 255 255 255 255
                                   255 255 255 255 255 255 255 253) 14 'little))
(65533 (bytevector-u16-ref #vu8(255 255 255 255 255 255 255 255
                                    255 255 255 255 255 255 255 253) 14 'big))
(-3 (bytevector-s16-ref #vu8(255 255 255 255 255 255 255 255
                                    255 255 255 255 255 255 255 253) 14 'big))
(#x00ff (bytevector-u16-ref #vu8(#xff 0) 0 'little))
(#xff00 (bytevector-u16-ref #vu8(#xff 0) 0 'big))
(error (bytevector-u16-ref #vu8(#xff 0) 1 'little))
(error (bytevector-u16-ref #vu8(#xff 0) 1 'big))
(error (bytevector-u16-native-ref #vu8(#xff 0 1 2) 1))
(error (bytevector-s16-native-ref #vu8(#xff 0 1 2) 1))
(#t (= (bytevector-u16-ref #vu8(255 255 255 255 255 255 255 255
                                    255 255 255 255 255 255 255 253) 14 (native-endianness))
       (bytevector-u16-native-ref #vu8(255 255 255 255 255 255 255 255
                                           255 255 255 255 255 255 255 253) 14)))
(#t (= (bytevector-s16-ref #vu8(255 255 255 255 255 255 255 255
                                    255 255 255 255 255 255 255 253) 14 (native-endianness))
       (bytevector-s16-native-ref #vu8(255 255 255 255 255 255 255 255
                                           255 255 255 255 255 255 255 253) 14)))
(12345 (let ([b #vu8(0 0 0 0 0)])
         (bytevector-u16-set! b 0 12345 'little)
         (bytevector-u16-ref b 0 'little)))
(12345 (let ([b #vu8(0 0 0 0 0)])
         (bytevector-u16-set! b 0 12345 'big)
         (bytevector-u16-ref b 0 'big)))
(12345 (let ([b #vu8(0 0 0 0 0)])
         (bytevector-s16-set! b 0 12345 'little)
         (bytevector-s16-ref b 0 'little)))
(12345 (let ([b #vu8(0 0 0 0 0)])
         (bytevector-s16-set! b 0 12345 'big)
         (bytevector-s16-ref b 0 'big)))
(error (let ([b #vu8(0 0 0 0 0)])
         (bytevector-s16-set! b 0 32768 'little)
         (bytevector-s16-ref b 0 'little)))
(#t (let ([b #vu8(0 0 0 0)])
      (bytevector-u16-set! b 0 12345 (native-endianness))
      (bytevector-u16-native-set! b 2 12345)
      (= (bytevector-u16-native-ref b 0)
         (bytevector-u16-native-ref b 2))))
(#t (let ([b #vu8(0 0 0 0)])
      (bytevector-s16-set! b 0 12345 (native-endianness))
      (bytevector-s16-native-set! b 2 12345)
      (= (bytevector-s16-native-ref b 0)
         (bytevector-s16-native-ref b 2))))

(4261412863 (bytevector-u32-ref #vu8(255 255 255 255 255 255 255 255
                                    255 255 255 255 255 255 255 253) 12 'little))
(-33554433 (bytevector-s32-ref #vu8(255 255 255 255 255 255 255 255
                                   255 255 255 255 255 255 255 253) 12 'little))
(4294967293 (bytevector-u32-ref #vu8(255 255 255 255 255 255 255 255
                                    255 255 255 255 255 255 255 253) 12 'big))
(-3 (bytevector-s32-ref #vu8(255 255 255 255 255 255 255 255
                                    255 255 255 255 255 255 255 253) 12 'big))
(#t (= (bytevector-u32-ref #vu8(255 255 255 255 255 255 255 255
                                    255 255 255 255 255 255 255 253) 12 (native-endianness))
       (bytevector-u32-native-ref #vu8(255 255 255 255 255 255 255 255
                                           255 255 255 255 255 255 255 253) 12)))
(#t (= (bytevector-s32-ref #vu8(255 255 255 255 255 255 255 255
                                    255 255 255 255 255 255 255 253) 12 (native-endianness))
       (bytevector-s32-native-ref #vu8(255 255 255 255 255 255 255 255
                                           255 255 255 255 255 255 255 253) 12)))
(12345 (let ([b #vu8(0 0 0 0 0)])
         (bytevector-u32-set! b 0 12345 'little)
         (bytevector-u32-ref b 0 'little)))
(12345 (let ([b #vu8(0 0 0 0 0)])
         (bytevector-u32-set! b 0 12345 'big)
         (bytevector-u32-ref b 0 'big)))

(18302628885633695743 (bytevector-u64-ref #vu8(255 255 255 255 255 255 255 255
                                    255 255 255 255 255 255 255 253) 8 'little))
(-144115188075855873 (bytevector-s64-ref #vu8(255 255 255 255 255 255 255 255
                                   255 255 255 255 255 255 255 253) 8 'little))
;; todo
;; (18446744073709551613 (bytevector-u64-ref #vu8(255 255 255 255 255 255 255 255
;;                                    255 255 255 255 255 255 255 253) 8 'big))
(-3 (bytevector-s64-ref #vu8(255 255 255 255 255 255 255 255
                                    255 255 255 255 255 255 255 253) 8 'big))
(#t (= (bytevector-u64-ref #vu8(255 255 255 255 255 255 255 255
                                    255 255 255 255 255 255 255 253) 8 (native-endianness))
       (bytevector-u64-native-ref #vu8(255 255 255 255 255 255 255 255
                                           255 255 255 255 255 255 255 253) 8)))
(#t (= (bytevector-s64-ref #vu8(255 255 255 255 255 255 255 255
                                    255 255 255 255 255 255 255 253) 8 (native-endianness))
       (bytevector-s64-native-ref #vu8(255 255 255 255 255 255 255 255
                                           255 255 255 255 255 255 255 253) 8)))
(12345 (let ([b #vu8(0 0 0 0 0 0 0 0)])
         (bytevector-u64-set! b 0 12345 'little)
         (bytevector-u64-ref b 0 'little)))
(12345 (let ([b #vu8(0 0 0 0 0 0 0 0)])
         (bytevector-u64-set! b 0 12345 'big)
         (bytevector-u64-ref b 0 'big)))
("あいう" (utf8->string #vu8(#xe3 #x81 #x82 #xe3 #x81 #x84 #xe3 #x81 #x86)))
("あいう" (bytevector->string #vu8(#xe3 #x81 #x82 #xe3 #x81 #x84 #xe3 #x81 #x86)
                             (make-transcoder (utf-8-codec))))
(#vu8(#xe3 #x81 #x82 #xe3 #x81 #x84 #xe3 #x81 #x86) (string->bytevector "あいう" (make-transcoder (utf-8-codec))))
(error (bytevector->string #vu8(#x81 #x82 #xe3 #x81 #x84 #xe3 #x81 #x86)
                             (make-transcoder (utf-8-codec))))
(#vu8(#xe3 #x81 #x82 #xe3 #x81 #x84 #xe3 #x81 #x86) (string->utf8 "あいう"))

(#vu8(97 0 0 0 112 0 0 0 112 0 0 0 #xBB #x3 0 0 101 0 0 0) (string->utf32 "app\x3BB;e" 'little))
(#vu8(0 0 0 97 0 0 0 112 0 0 0 112 0 0 #x3 #xBB 0 0 0 101) (string->utf32 "app\x3BB;e" 'big))
(#vu8(0 0 0 97 0 0 0 112 0 0 0 112 0 0 #x3 #xBB 0 0 0 101) (string->utf32 "app\x3BB;e"))
(error (string->utf32 "app\x3BB;e" 'hoge))
(#t (and
     (let ([str "apple"])
       (string=? str (utf32->string (string->utf32 str 'big) 'big)))
     (let ([str "app\x3BB;e"])
       (string=? str (utf32->string (string->utf32 str 'big) 'big)))
     (let ([str "\x0;\x1;\x80;\xFF;\xD7FF;\xE000;\x10FFFF;"])
       (string=? str (utf32->string (string->utf32 str 'big) 'big)))
     #t))
(#t (and
     (let ([str "apple"])
       (string=? str (utf32->string (string->utf32 str 'little) 'little)))
     (let ([str "app\x3BB;e"])
       (string=? str (utf32->string (string->utf32 str 'little) 'little)))
     (let ([str "\x0;\x1;\x80;\xFF;\xD7FF;\xE000;\x10FFFF;"])
       (string=? str (utf32->string (string->utf32 str 'little) 'little)))
     #t))

(#t (let ([str "apple"]
          [bv-append (lambda (bv1 bv2)
                       (let ([bv (make-bytevector (+ (bytevector-length bv1)
                                                     (bytevector-length bv2)))])
                         (bytevector-copy! bv1 0 bv 0  (bytevector-length bv1))
                         (bytevector-copy! bv2 0 bv (bytevector-length bv1) (bytevector-length bv2))
                         bv))])
      (and
       (string=? str (utf32->string (bv-append #vu8(#xFF #xFE 0 0) (string->utf32 str 'little)) 'big))
       (string=? str (utf32->string (bv-append #vu8(0 0 #xFE #xFF) (string->utf32 str 'big)) 'little))
       #t)))
(#t (let ([str "app\x3BB;e"]
          [bv-append (lambda (bv1 bv2)
                       (let ([bv (make-bytevector (+ (bytevector-length bv1)
                                                     (bytevector-length bv2)))])
                         (bytevector-copy! bv1 0 bv 0  (bytevector-length bv1))
                         (bytevector-copy! bv2 0 bv (bytevector-length bv1) (bytevector-length bv2))
                         bv))])
      (and
       (string=? str (utf32->string (bv-append #vu8(#xFF #xFE 0 0) (string->utf32 str 'little)) 'big))
       (string=? str (utf32->string (bv-append #vu8(0 0 #xFE #xFF) (string->utf32 str 'big)) 'little))
       #t)))
(#t (let ([str "apple"]
          [bv-append (lambda (bv1 bv2)
                       (let ([bv (make-bytevector (+ (bytevector-length bv1)
                                                     (bytevector-length bv2)))])
                         (bytevector-copy! bv1 0 bv 0  (bytevector-length bv1))
                         (bytevector-copy! bv2 0 bv (bytevector-length bv1) (bytevector-length bv2))
                         bv))])
      (and
       (string=? str (utf32->string (bv-append #vu8(#xFF #xFE 0 0) (string->utf32 str 'little)) 'big))
       (string=? str (utf32->string (bv-append #vu8(0 0 #xFE #xFF) (string->utf32 str 'big)) 'little)))))

(#t (let ([str "app\x3BB;e"]
          [bv-append (lambda (bv1 bv2)
                       (let ([bv (make-bytevector (+ (bytevector-length bv1)
                                                     (bytevector-length bv2)))])
                         (bytevector-copy! bv1 0 bv 0  (bytevector-length bv1))
                         (bytevector-copy! bv2 0 bv (bytevector-length bv1) (bytevector-length bv2))
                         bv))])
      (and
       (string=? (utf32->string (bv-append #vu8(#xFF #xFE 0 0) (string->utf32 str 'little)) 'little #t)
                 (string-append "\xFEFF;" str))
       (string=? (utf32->string (bv-append #vu8(#xFE #xFF 0 0) (string->utf32 str 'little)) 'little #t)
                 (string-append "\xFFFE;" str))
       (string=? (utf32->string (bv-append #vu8(0 0 #xFE #xFF) (string->utf32 str 'big)) 'big #t)
                 (string-append "\xFEFF;" str))
       (string=? (utf32->string (bv-append #vu8(0 0 #xFF #xFE) (string->utf32 str 'big)) 'big #t)
                 (string-append "\xFFFE;" str)))))
(#t (let ([str "\x0;\x1;\x80;\xFF;\xD7FF;\xE000;\x10FFFF;"]
          [bv-append (lambda (bv1 bv2)
                       (let ([bv (make-bytevector (+ (bytevector-length bv1)
                                                     (bytevector-length bv2)))])
                         (bytevector-copy! bv1 0 bv 0  (bytevector-length bv1))
                         (bytevector-copy! bv2 0 bv (bytevector-length bv1) (bytevector-length bv2))
                         bv))])
      (and
       (string=? (utf32->string (bv-append #vu8(#xFF #xFE 0 0) (string->utf32 str 'little)) 'little #t)
                 (string-append "\xFEFF;" str))
       (string=? (utf32->string (bv-append #vu8(#xFE #xFF 0 0) (string->utf32 str 'little)) 'little #t)
                 (string-append "\xFFFE;" str))
       (string=? (utf32->string (bv-append #vu8(0 0 #xFE #xFF) (string->utf32 str 'big)) 'big #t)
                 (string-append "\xFEFF;" str))
       (string=? (utf32->string (bv-append #vu8(0 0 #xFF #xFE) (string->utf32 str 'big)) 'big #t)
                 (string-append "\xFFFE;" str)))))

(#t (let ([str "\x0;\x1;\x80;\xFF;\xD7FF;\xE000;\x10FFFF;"]
          [bv-append (lambda (bv1 bv2)
                       (let ([bv (make-bytevector (+ (bytevector-length bv1)
                                                     (bytevector-length bv2)))])
                         (bytevector-copy! bv1 0 bv 0  (bytevector-length bv1))
                         (bytevector-copy! bv2 0 bv (bytevector-length bv1) (bytevector-length bv2))
                         bv))])
      (and
       (string=? (utf32->string (bv-append #vu8(#xFF #xFE 0 0) (string->utf32 str 'little)) 'little #t)
                 (string-append "\xFEFF;" str))
       (string=? (utf32->string (bv-append #vu8(#xFE #xFF 0 0) (string->utf32 str 'little)) 'little #t)
                 (string-append "\xFFFE;" str))
       (string=? (utf32->string (bv-append #vu8(0 0 #xFE #xFF) (string->utf32 str 'big)) 'big #t)
                 (string-append "\xFEFF;" str))
       (string=? (utf32->string (bv-append #vu8(0 0 #xFF #xFE) (string->utf32 str 'big)) 'big #t)
                 (string-append "\xFFFE;" str)))))

(#vu8(97 0 112 0 112 0 #xBB #x3 101 0) (string->utf16 "app\x3BB;e" 'little))
(#vu8(0 97 0 112 0 112 #x3 #xBB 0 101) (string->utf16 "app\x3BB;e" 'big))
(#vu8(0 97 0 112 0 112 #x3 #xBB 0 101) (string->utf16 "app\x3BB;e"))
(#t (and
     (let ([str "apple"])
       (string=? str (utf16->string (string->utf16 str 'big) 'big)))
     (let ([str "app\x3BB;e"])
       (string=? str (utf16->string (string->utf16 str 'big) 'big)))
     (let ([str "\x0;\x1;\x80;\xFF;\xD7FF;\xE000;\x10FFFF;"])
       (string=? str (utf16->string (string->utf16 str 'big) 'big)))
     #t))

(#t (let ([str "apple"]
          [bv-append (lambda (bv1 bv2)
                       (let ([bv (make-bytevector (+ (bytevector-length bv1)
                                                     (bytevector-length bv2)))])
                         (bytevector-copy! bv1 0 bv 0  (bytevector-length bv1))
                         (bytevector-copy! bv2 0 bv (bytevector-length bv1) (bytevector-length bv2))
                         bv))])
      (and (string=? (utf16->string (bv-append #vu8(#xFF #xFE) (string->utf16 str 'little)) 'big) str)
          (string=? (utf16->string (bv-append #vu8(#xFE #xFF) (string->utf16 str 'big)) 'little) str)
          (string=? (utf16->string (bv-append #vu8(#xFF #xFE) (string->utf16 str 'little)) 'little #t)
                    (string-append "\xFEFF;" str))
          (string=? (utf16->string (bv-append #vu8(#xFE #xFF) (string->utf16 str 'little)) 'little #t)
                    (string-append "\xFFFE;" str))
          (string=? (utf16->string (bv-append #vu8(#xFE #xFF) (string->utf16 str 'big)) 'big #t)
                    (string-append "\xFEFF;" str))
           (string=? (utf16->string (bv-append #vu8(#xFF #xFE) (string->utf16 str 'big)) 'big #t)
                     (string-append "\xFFFE;" str))
           )))
(#t (let ([str "app\x3BB;e"]
          [bv-append (lambda (bv1 bv2)
                       (let ([bv (make-bytevector (+ (bytevector-length bv1)
                                                     (bytevector-length bv2)))])
                         (bytevector-copy! bv1 0 bv 0  (bytevector-length bv1))
                         (bytevector-copy! bv2 0 bv (bytevector-length bv1) (bytevector-length bv2))
                         bv))])
      (and (string=? (utf16->string (bv-append #vu8(#xFF #xFE) (string->utf16 str 'little)) 'big) str)
           (string=? (utf16->string (bv-append #vu8(#xFE #xFF) (string->utf16 str 'big)) 'little) str)
           (string=? (utf16->string (bv-append #vu8(#xFF #xFE) (string->utf16 str 'little)) 'little #t)
                     (string-append "\xFEFF;" str))
           (string=? (utf16->string (bv-append #vu8(#xFE #xFF) (string->utf16 str 'little)) 'little #t)
                     (string-append "\xFFFE;" str))
           (string=? (utf16->string (bv-append #vu8(#xFE #xFF) (string->utf16 str 'big)) 'big #t)
                     (string-append "\xFEFF;" str))
           (string=? (utf16->string (bv-append #vu8(#xFF #xFE) (string->utf16 str 'big)) 'big #t)
                     (string-append "\xFFFE;" str)))))
(#t (let ([str "\x0;\x1;\x80;\xFF;\xD7FF;\xE000;\x10FFFF;"]
          [bv-append (lambda (bv1 bv2)
                       (let ([bv (make-bytevector (+ (bytevector-length bv1)
                                                     (bytevector-length bv2)))])
                         (bytevector-copy! bv1 0 bv 0  (bytevector-length bv1))
                         (bytevector-copy! bv2 0 bv (bytevector-length bv1) (bytevector-length bv2))
                         bv))])
      (and (string=? (utf16->string (bv-append #vu8(#xFF #xFE) (string->utf16 str 'little)) 'big) str)
           (string=? (utf16->string (bv-append #vu8(#xFE #xFF) (string->utf16 str 'big)) 'little) str)
           (string=? (utf16->string (bv-append #vu8(#xFF #xFE) (string->utf16 str 'little)) 'little #t)
                     (string-append "\xFEFF;" str))
           (string=? (utf16->string (bv-append #vu8(#xFE #xFF) (string->utf16 str 'little)) 'little #t)
                     (string-append "\xFFFE;" str))
           (string=? (utf16->string (bv-append #vu8(#xFE #xFF) (string->utf16 str 'big)) 'big #t)
                     (string-append "\xFEFF;" str))
           (string=? (utf16->string (bv-append #vu8(#xFF #xFE) (string->utf16 str 'big)) 'big #t)
                     (string-append "\xFFFE;" str)))))
