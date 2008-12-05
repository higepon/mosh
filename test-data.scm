;; don't edit start
(#t #t)
(mosh-only ("all-tests.scm" 2) (source-info '(3)))
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
(2 ((lambda (g) ((lambda (f) (f 2)) (lambda (a) (g a)))) (lambda (x) x)))
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
(3 (let1 a 3
    (let1 b (lambda () a)
      (b))))
(3 (let ([a 3]) (let ([b (lambda () a)]) (b))))
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
(error (letrec ([a 3]
                [b a])
         (display b)
         (newline)))

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
(102 102)
;; internal define
(4 (define val 3) (define (func8) (define val 4) val) (func8))
((1 . 3) ((lambda () (define p (cons 1 2)) (set-cdr! p 3) p)))
((3 . 2) ((lambda () (define q (cons 1 2)) (set-car! q 3) q)))
(#t (begin #f #t))
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
(error (call-with-values (lambda () (values 1 2)) (lambda (a b c) (+ a b c))))
(mosh-only "higepon" (receive (port proc) (open-string-output-port)
             (display "hige" port)
             (display "pon" port)
             (proc)))
("\"string\"" (call-with-string-output-port (lambda (port) (write "string" port))))
("123ABC456" (regexp-replace #/abc/ "123abc456" "ABC"))
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

;;; cdr
((b c d) (cdr '((a) b c d)))
(2 (cdr '(1 . 2)))

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
[mosh-only "no-error"
  (with-exception-handler
    (lambda (e)
      "error")
    (lambda () "no-error"))]
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
[todo mosh-only "catched at parent"
 (guard (e
         [(symbol? e)
          "catched at parent"]
         [else
          "error"])
   (guard (con
           [(string? con)
            "error-is-string"]) ;; no else clause
          (raise 'symbol-error)))]
[todo mosh-only 7
  (guard (con
          [con
           4]
          [else
           "error-is-not-string"])
         (+ 3 (raise-continuable "warn continuation")))]
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
[14 (for-all (lambda (n) (and (even? n) n))
            '(2 4 14))]
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
[mosh-only #t (rational? 3)]
[mosh-only #t (rational? (/ 1 4))]
[mosh-only #t (rational? (/ (/ 1 2) (+ (greatest-fixnum) 1)))]
[mosh-only #t (flonum? (/ (inexact (/ 1 3)) (+ (greatest-fixnum) 1)))]
[mosh-only #t (= (/ (+ (greatest-fixnum) 1) 1) (+ (greatest-fixnum) 1))]
[mosh-only #t (rational? (/ (+ (greatest-fixnum) 1) (/ 1 3)))]
[mosh-only #t (flonum? (/ (+ (greatest-fixnum) 1) (inexact (/ 1 3) )))]
[mosh-only 1 (/ (+ (greatest-fixnum) 1) (+ (greatest-fixnum) 1))]
[mosh-only #t (fixnum? (/ (+ (greatest-fixnum) 1) (+ (greatest-fixnum) 1)))] ;; normalization
[mosh-only 1/2 (/ 2)]
[mosh-only 1/3 (/ 3)]
[todo mosh-only error (/ 0)] -> division by zero

[#t (fixnum? (least-fixnum))]
[#t (fixnum? (greatest-fixnum))]
[#f (fixnum? (+ (greatest-fixnum) 1))]
[#f (fixnum? (- (least-fixnum) 1))]
[#t (number? (+ (greatest-fixnum) 1))]
[#t (number? (- (least-fixnum) 1))]
[#t (> (+ (greatest-fixnum) 1) (greatest-fixnum))]
[#t (< (- (least-fixnum) 1) (least-fixnum))]
[#t (fixnum? (- (+ (greatest-fixnum) 1) 1))] ;; normalization
[#t (fixnum? (+ (- (least-fixnum) 1) 1))] ;; normalization

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
[todo mosh-only (connect talk1 disconnect connect talk2 disconnect)
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
[todo mosh-only (3 connect talk1 disconnect connect talk2 disconnect 1) ;; from Gauche
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
[todo error (and #/(?<hage >.*)/ #t)] ;; invalid regexp group name
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
(todo error (+ 1 2 'a))
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

(#vu8(255 255 255 255 255 255 255 253) (let1 b (make-bytevector 8)
                                         (bytevector-u64-set! b 0 18302628885633695743 'little)
                                         b))
(#vu8(0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 253) (let1 b (make-bytevector 16 0)
                                         (bytevector-u64-set! b 8 18302628885633695743 'little)
                                         b))
(#vu8(255 255 255 255 255 255 255 253) (let1 b (make-bytevector 8)
                                         (bytevector-s64-set! b 0 -144115188075855873 'little)
                                         b))
(#vu8(0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 253) (let1 b (make-bytevector 16 0)
                                         (bytevector-s64-set! b 8 -144115188075855873 'little)
                                         b))
(18302628885633695743 (bytevector-u64-ref #vu8(255 255 255 255 255 255 255 255
                                    255 255 255 255 255 255 255 253) 8 'little))
(-144115188075855873 (bytevector-s64-ref #vu8(255 255 255 255 255 255 255 255
                                   255 255 255 255 255 255 255 253) 8 'little))
(18446744073709551613 (bytevector-u64-ref #vu8(255 255 255 255 255 255 255 255
                                                        255 255 255 255 255 255 255 253) 8 'big))
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

[definition
  (define (almost=? x y)
    (cond ((infinite? x)
           (=   x (* t2.0 y)))
          ((infinite? y)
           (= (* t2.0 x) y))
          ((nan? y)
           (nan? x))
          ((> (flabs y) (inexact (/ 1 1000000)))
           (< (/ (flabs (- x y))
                 (flabs y))
              (inexact (/ 1 1000))))
          (else
           (< (flabs (- x y)) (inexact (/ 1 1000000))))))

(define t7.389 (inexact (/ 7389 1000)))
(define t1024.0 (inexact 1024))
(define t1.570796 (inexact (/ 1570796 1000000)))
(define t-1.570796 (inexact (/ -1570796 1000000)))
(define t1.47113 (inexact (/ 147113 100000)))
(define t0.1 (inexact (/ 1 10)))
(define t8.0 (inexact 8))
(define t1000.0 (inexact 1000))
(define t0.0996687 (inexact (/ 00996687 10000000)))
(define t2.23607 (inexact (/ 223607 100000)))
]

(#t (almost=? 3.14 (bytevector-ieee-single-ref #vu8(#xc3 #xf5 #x48 #x40) 0 'little)))
(#t (almost=? 3.14 (bytevector-ieee-single-ref #vu8(#x40 #x48 #xf5 #xc3) 0 'big)))
(#t (almost=? 3.14 (bytevector-ieee-double-ref #vu8(0 0 0 0 0 0 0 0 #x1f #x85 #xeb #x51 #xb8 #x1e #x9  #x40) 8 'little)))
(#t (almost=? 3.14 (bytevector-ieee-double-ref #vu8(0 0 0 0 0 0 0 0 #x40 #x9  #x1e #xb8 #x51 #xeb #x85 #x1f) 8 'big)))
(#t (almost=? 3.14 (let ([b (make-bytevector 8)])
                     (bytevector-ieee-single-native-set! b 4 3.14)
                     (bytevector-ieee-single-native-ref b 4))))
(#t (almost=? 3.14 (let ([b (make-bytevector 8)])
                     (bytevector-ieee-single-set! b 4 3.14 'little)
                     (bytevector-ieee-single-ref b 4 'little))))
(#t (almost=? 3.14 (let ([b (make-bytevector 8)])
                     (bytevector-ieee-single-set! b 4 3.14 'big)
                     (bytevector-ieee-single-ref b 4 'big))))
(#t (almost=? 3.14 (let ([b (make-bytevector 16)])
                     (bytevector-ieee-double-native-set! b 8 3.14)
                     (bytevector-ieee-double-native-ref b 8))))
(#t (almost=? 3.14 (let ([b (make-bytevector 16)])
                     (bytevector-ieee-double-set! b 8 3.14 'little)
                     (bytevector-ieee-double-ref b 8 'little))))
(#t (almost=? 3.14 (let ([b (make-bytevector 16)])
                     (bytevector-ieee-double-set! b 8 3.14 'big)
                     (bytevector-ieee-double-ref b 8 'big))))
(#t (let1 b (make-bytevector 16 -127)
      (bytevector-uint-set! b 0 (- (expt 2 128) 3)
                            'little 16)
      (and (= #xfffffffffffffffffffffffffffffffd (bytevector-uint-ref b 0 'little 16))
           (= -3 (bytevector-sint-ref b 0 'little 16))
           (equal? (bytevector->u8-list b)
                '(253 255 255 255 255 255 255 255
               255 255 255 255 255 255 255 255)))))

;; Tests originally from Ikarus START
(test (bytevector? (make-bytevector 1)) #t)
(test (bytevector? (make-bytevector 1 17)) #t)
(test (bytevector? (make-bytevector 10 -17)) #t)
(test (bytevector? 'foo) #f)
(test (bytevector? "hey") #f)
(test (bytevector? '#(2837 2398 239)) #f)
(test (bytevector-length (make-bytevector 0)) 0)
(test (bytevector-length (make-bytevector 100 -30)) 100)
(test (let ((b (u8-list->bytevector '(1 2 3 4 5 6 7 8))))
        (bytevector-copy! b 0 b 3 4)
        (bytevector->u8-list b))
      '(1 2 3 1 2 3 4 8))
(test (bytevector-uint-ref
       (u8-list->bytevector '(17))
       0 'little 1)
      17)
(test (bytevector-uint-ref
       (u8-list->bytevector '(17))
       0 'big 1)
      17)
(test (bytevector-uint-ref
       (u8-list->bytevector '(17 54))
       0 'little 2)
      (+ 17 (* 54 256)))
(test (bytevector-uint-ref
       (u8-list->bytevector (reverse '(17 54)))
       0 'big 2)
      (+ 17 (* 54 256)))
(test (bytevector-uint-ref
       (u8-list->bytevector '(17 54 98))
       0 'little 3)
      (+ 17 (* 54 256) (* 98 256 256)))
(test (bytevector-uint-ref
       (u8-list->bytevector (reverse '(17 54 98)))
       0 'big 3)
      (+ 17 (* 54 256) (* 98 256 256)))
(test (bytevector-uint-ref
       (u8-list->bytevector '(17 54 98 120))
       0 'little 4)
      (+ 17 (* 54 256) (* 98 256 256) (* 120 256 256 256)))

(test (bytevector-uint-ref
       (u8-list->bytevector
        '(#x89 #x04 #x39 #x82 #x49 #x20 #x93 #x48 #x17
               #x83 #x79 #x94 #x38 #x87 #x34 #x97 #x38 #x12))
       0 'little 18)
      #x123897348738947983174893204982390489)
(test (bytevector-uint-ref
       (u8-list->bytevector
        (reverse
         '(#x89 #x04 #x39 #x82 #x49 #x20 #x93 #x48 #x17
                #x83 #x79 #x94 #x38 #x87 #x34 #x97 #x38 #x12)))
       0 'big 18)
      #x123897348738947983174893204982390489)
(test (let ((b (u8-list->bytevector '(1 2 3 255 1 2 1 2))))
        (bytevector->uint-list b 'little 2))
      '(513 65283 513 513))
(test (bytevector->u8-list
       (uint-list->bytevector '(513 65283 513 513) 'little 2))
      '(1 2 3 255 1 2 1 2))
(test (bytevector->u8-list
       (uint-list->bytevector '(513 65283 513 513) 'big 2))
      '(2 1 255 3 2 1 2 1))
(test (let ((b (u8-list->bytevector '(1 2 3 255 1 2 1 2))))
        (bytevector->sint-list b 'little 2))
      '(513 -253 513 513))
(test (let ((b (u8-list->bytevector '(2 1 255 3 2 1 2 1))))
        (bytevector->sint-list b 'big 2))
      '(513 -253 513 513))
(test (bytevector->u8-list
       (sint-list->bytevector '(513 -253 513 513) 'little 2))
      '(1 2 3 255 1 2 1 2))
(test (let ((b (u8-list->bytevector '(1 2 3 255 1 2 1 2))))
        (bytevector->sint-list b 'little 2))
      '(513 -253 513 513))
(test (let ((b (make-bytevector 16 -127)))
        (bytevector-uint-set! b 0 (- (expt 2 128) 3) 'little 16)
        (list
         (bytevector-uint-ref b 0 'little 16)
         (bytevector-sint-ref b 0 'little 16)
         (bytevector->u8-list b)))
      '(#xfffffffffffffffffffffffffffffffd
        -3
        (253 255 255 255 255 255 255 255
             255 255 255 255 255 255 255 255)))
(test (let ((b (make-bytevector 16 -127)))
        (bytevector-uint-set! b 0 (- (expt 2 128) 3) 'big 16)
        (list
         (bytevector-uint-ref b 0 'big 16)
         (bytevector-sint-ref b 0 'big 16)
         (bytevector->u8-list b)))
      '(#xfffffffffffffffffffffffffffffffd
        -3
        (255 255 255 255 255 255 255 255
             255 255 255 255 255 255 255 253)))
(test (bytevector->u8-list '#vu8(1 2 3 4))
      '(1 2 3 4))
(test (let ((b (make-bytevector 4 0)))
        (bytevector-sint-set! b 0 -1 'little 4)
        (bytevector-uint-ref b 0 'little 4))
      #xFFFFFFFF)
(test (let ((b (make-bytevector 4 0)))
        (bytevector-sint-set! b 0 -256 'little 4)
        (bytevector-uint-ref b 0 'little 4))
      #xFFFFFF00)
(test (let ((b (make-bytevector 4 0)))
        (bytevector-sint-set! b 0 (- (expt 256 2)) 'little 4)
        (bytevector-uint-ref b 0 'little 4))
      #xFFFF0000)
(test (let ((b (make-bytevector 8 0)))
        (bytevector-sint-set! b 0 (- (expt 256 2)) 'little 8)
        (bytevector-uint-ref b 0 'little 8))
      #xFFFFFFFFFFFF0000)
(test (let ((b (make-bytevector 8 0)))
        (bytevector-sint-set! b 0 (- (expt 256 4)) 'little 8)
        (bytevector-uint-ref b 0 'little 8))
      #xFFFFFFFF00000000)
(test (let ((b (make-bytevector 8 0)))
        (bytevector-sint-set! b 0 (- (expt 256 7)) 'little 8)
        (bytevector-uint-ref b 0 'little 8))
      #xFF00000000000000)
(test (let ((b (make-bytevector 8 0)))
        (bytevector-sint-set! b 0 (- 1 (expt 2 63)) 'little 8)
        (bytevector-sint-ref b 0 'little 8))
      (- 1 (expt 2 63)))
(test (let ((b (make-bytevector 4 38)))
        (bytevector-sint-set! b 0 (- (expt 2 31) 1) 'little 4)
        (bytevector-sint-ref b 0 'little 4))
      #x7FFFFFFF)
(test (let ((b (make-bytevector 4 38)))
        (bytevector-sint-set! b 0 (- (expt 2 31)) 'little 4)
        (bytevector-sint-ref b 0 'little 4))
      #x-80000000)
(test (let ((b (make-bytevector 5 38)))
        (bytevector-sint-set! b 0 (- (expt 2 32)) 'little 5)
        (bytevector-sint-ref b 0 'little 5))
      #x-100000000)
(test (let ((b (make-bytevector 4 0)))
        (bytevector-sint-set! b 0 -1 'big 4)
        (bytevector-uint-ref b 0 'big 4))
      #xFFFFFFFF)
(test (let ((b (make-bytevector 4 0)))
        (bytevector-sint-set! b 0 -256 'big 4)
        (bytevector-uint-ref b 0 'big 4))
      #xFFFFFF00)
(test (let ((b (make-bytevector 4 0)))
        (bytevector-sint-set! b 0 (- (expt 256 2)) 'big 4)
        (bytevector-uint-ref b 0 'big 4))
      #xFFFF0000)
(test (let ((b (make-bytevector 8 0)))
        (bytevector-sint-set! b 0 (- (expt 256 2)) 'big 8)
        (bytevector-uint-ref b 0 'big 8))
      #xFFFFFFFFFFFF0000)
(test (let ((b (make-bytevector 8 0)))
        (bytevector-sint-set! b 0 (- (expt 256 4)) 'big 8)
        (bytevector-uint-ref b 0 'big 8))
      #xFFFFFFFF00000000)
(test (let ((b (make-bytevector 8 0)))
        (bytevector-sint-set! b 0 (- (expt 256 7)) 'big 8)
        (bytevector-uint-ref b 0 'big 8))
      #xFF00000000000000)
(test (let ((b (make-bytevector 8 0)))
        (bytevector-sint-set! b 0 (- 1 (expt 2 63)) 'big 8)
        (bytevector-sint-ref b 0 'big 8))
      (- 1 (expt 2 63)))
(test (let ((b (make-bytevector 4 38)))
        (bytevector-sint-set! b 0 (- (expt 2 31) 1) 'big 4)
        (bytevector-sint-ref b 0 'big 4))
      #x7FFFFFFF)
(test (let ((b (make-bytevector 4 38)))
        (bytevector-sint-set! b 0 (- (expt 2 31)) 'big 4)
        (bytevector-sint-ref b 0 'big 4))
      #x-80000000)
(test (let ((b (make-bytevector 5 38)))
        (bytevector-sint-set! b 0 (- (expt 2 32)) 'big 5)
        (bytevector-sint-ref b 0 'big 5))
      #x-100000000)
(test (bytevector-u16-ref '#vu8(255 253) 0 'little)
      65023)
(test (bytevector-u16-ref '#vu8(255 253) 0 'big)
      65533)
(test (bytevector-s16-ref '#vu8(255 253) 0 'little)
      -513)
(test (bytevector-s16-ref '#vu8(255 253) 0 'big)
      -3)
(test (let ((v (make-bytevector 2)))
        (bytevector-u16-native-set! v 0 12345)
        (bytevector-u16-native-ref v 0))
      12345)
(test (let ((v (make-bytevector 2)))
        (bytevector-u16-set! v 0 12345 'little)
        (bytevector-u16-ref v 0 'little))
      12345)
(test (let ((v (make-bytevector 2)))
        (bytevector-u16-set! v 0 12345 'big)
        (bytevector-u16-ref v 0 'big))
      12345)
;; Tests originally from Ikarus END
;; complex
(#t (= (make-rectangular 1 1) (make-rectangular 1 1)))
(#t (= 3 (make-rectangular 3 0)))
(#t (= (+ (greatest-fixnum) 1) (make-rectangular (+ (greatest-fixnum) 1) 0)))
(#t (= (/ 1 3) (make-rectangular (/ 2 6) 0)))
(#t (= (inexact (/ 1 3)) (make-rectangular (inexact (/ 2 6)) 0))) ;; may be equal?
(#t (= (make-rectangular 3 0) 3))
(#t (= (make-rectangular (+ (greatest-fixnum) 1) 0) (+ (greatest-fixnum) 1)))
(#t (= (make-rectangular (/ 2 6) 0) (/ 1 3)))
(#t (= (make-rectangular (inexact (/ 2 6)) 0) (inexact (/ 1 3)))) ;; may be equal?
(#t (= (make-rectangular 2 3) (+ (make-rectangular 1 2) (make-rectangular 1 1))))
(#t (= (make-rectangular 0 1) (- (make-rectangular 1 2) (make-rectangular 1 1))))
(#t (= (make-rectangular 0 -1) (- (make-rectangular 1 1) (make-rectangular 1 2))))
(#t (= (make-rectangular -5 10) (* (make-rectangular 1 2) (make-rectangular 3 4))))
(#t (= (make-rectangular 1 0)  (/ (make-rectangular 1 2) (make-rectangular 1 2))))
(#t (= (make-rectangular 1 1) (/ (make-rectangular 2 2) 2)))
(1 (/ (make-rectangular 1 2) (make-rectangular 1 2)))
(2 (+ (make-rectangular 1 -1) (make-rectangular 1 1))) ;; normalize
(#t (= (make-rectangular (/ -1 2) (/ 3 2)) (/ (make-rectangular 1 2) (make-rectangular 1 -1))))
(#t (= (make-rectangular 2 1) (+ 1 (make-rectangular 1 1))))                                               ; fixnum + compnum
(#t (= (make-rectangular 3 1) (+ (make-rectangular 1 1) 2)))                                               ; compnum + fixnum
(#t (= (make-rectangular (+ (greatest-fixnum) 2) 1)(+ (+ (greatest-fixnum) 1) (make-rectangular 1 1))))    ; bignum + compnum
(#t (= (make-rectangular (+ (greatest-fixnum) 2) 1)(+ (make-rectangular 1 1) (+ (greatest-fixnum) 1) )))   ; compnum + bignum
(#t (= (make-rectangular 1 3) (+ (/ 1 2) (make-rectangular (/ 1 2) 3))))                                   ; ratnum + compnum
(#t (= (make-rectangular 1 3) (+ (make-rectangular (/ 1 2) 3) (/ 1 2))))                                   ; compnum + ratnum
(#t (let1 sum (+ (make-rectangular 1 2) (inexact (/ 1 2)))                                             ; compnum + flonum
      (and (< (inexact (/ 2 2)) (real-part sum) (inexact (/ 4 2)))
           (= (imag-part sum) 2))))
(#t (let1 sum (+ (inexact (/ 1 2)) (make-rectangular 1 2))                                             ; flonum + compnum
      (and (< (inexact (/ 2 2)) (real-part sum) (inexact (/ 4 2)))
           (= (imag-part sum) 2))))
(#t (= (make-rectangular 3 3) (+ (make-rectangular 1 1) (make-rectangular 2 2))))                              ; compnum + compnum
(#t (= (make-rectangular 0 -1) (- 1 (make-rectangular 1 1))))                                              ; fixnum - compnum
(#t (= (make-rectangular -1 1) (- (make-rectangular 1 1) 2)))                                              ; compnum - fixnum
(#t (= (make-rectangular (+ (greatest-fixnum) 1) -1) (- (+ (greatest-fixnum) 2) (make-rectangular 1 1))))  ; bignum - compnum
(#t (= (make-rectangular (least-fixnum) -1) (- (make-rectangular 1 1) (+ (greatest-fixnum) 2) )))          ; compnum - bignum
(#t (= (make-rectangular 0 3) (- (/ 1 2) (make-rectangular (/ 1 2) 3))))                                   ; ratnum - compnum
(#t (= (make-rectangular 0 3) (- (make-rectangular (/ 1 2) 3) (/ 1 2))))                                   ; compnum - ratnum
(#t (let1 sum (- (make-rectangular 1 2) (inexact (/ 1 2)))                                             ; compnum - flonum
      (and (< (inexact (/ 1 4)) (real-part sum) (inexact (/ 2 3)))
           (= (imag-part sum) 2))))
(#t (let1 sum (- (inexact (/ 1 2)) (make-rectangular 1 2))                                             ; flonum - compnum
      (and (< (inexact (- 0 (/ 3 4))) (real-part sum) (inexact (- 0 (/ 1 4))))
           (= (imag-part sum) -2))))
(#t (= (make-rectangular -1 -1) (- (make-rectangular 1 1) (make-rectangular 2 2))))                            ; compnum - compnum
(#t (= (make-rectangular 6 3) (* 3 (make-rectangular 2 1))))                                               ; fixnum * compnum
(#t (= (make-rectangular 6 3) (* (make-rectangular 2 1) 3)))                                               ; compnum * fixnum
(#t (= (make-rectangular (+ (greatest-fixnum) (greatest-fixnum) 2)
                     (+ 1 (greatest-fixnum)))
       (* (+ 1 (greatest-fixnum)) (make-rectangular 2 2))))                                            ; bignum * compnum
(#t (= (make-rectangular (+ (greatest-fixnum) (greatest-fixnum) 2)                                     ; compnum * bignum
                     (+ 1 (greatest-fixnum)))
       (* (make-rectangular 2 2) (+ 1 (greatest-fixnum)))))
(#t (= (make-rectangular (/ 1 3) (/ 2 6)) (* (/ 1 3) (make-rectangular 1 1))))                             ; ratnum * compnum
(#t (= (make-rectangular (/ 1 3) (/ 2 6)) (* (make-rectangular 1 1) (/ 1 3))))                             ; compnum * ratnum
(#t (let1 val (* (make-rectangular 2 3) (inexact (/ 1 3)))                                             ; compnum * flonum
      (and (< (/ 1 4) (real-part val) 1)
           (< (/ 99 100) (imag-part val) (inexact (/ 101 100))))))
(#t (let1 val (* (inexact (/ 1 3)) (make-rectangular 2 3))                                             ; flonum * compnum
      (and (< (/ 1 4) (real-part val) 1)
           (< (/ 99 100) (imag-part val) (inexact (/ 101 100))))))
(#t (= (make-rectangular -7 -27) (* (make-rectangular 2 3) (make-rectangular 4 5))))                           ; compnum * compnum
(#t (= (make-rectangular (/ 3 17) (/ 5 17)) (/ 2 (make-rectangular 3 5))))                                 ; fixnum / compnum
(#t (= (make-rectangular (/ 3 2) (/ 5 2)) (/ (make-rectangular 3 5) 2)))                                   ; compnum / fixnum
(#t (= (make-rectangular (/ (+ 1 (greatest-fixnum)) 2) (- 0 (/ (+ 1 (greatest-fixnum)) 2)))            ; bignum / compnum
       (/ (+ (greatest-fixnum) 1) (make-rectangular 1 1))))

(#t (= (make-rectangular 1 2)                                                                          ; compnum / bignum
       (/ (make-rectangular (+ (greatest-fixnum) 1) (* 2 (+ (greatest-fixnum) 1)))
          (+ (greatest-fixnum) 1))))
(#t (= (make-rectangular (/ 1 6) (/ -1 6)) (/ (/ 1 3) (make-rectangular 1 1))))                            ; ratnum / compnum
(#t (= (make-rectangular 3 3) (/ (make-rectangular 1 1) (/ 1 3))))                                         ; compnum / ratnum
(#t (> (/ 7 2) (real-part (/ (make-rectangular 1 1) (inexact (/ 1 3)))) (/ 5 2)))                      ; compnum / flonum
(#t (> (/ 2 6) (real-part (/ (inexact (/ 1 3)) (make-rectangular 1 1))) (/ 1 7)))                      ; flonum / compnum
(#t (= (make-rectangular (/ 5 13) (/ -1 13)) (/ (make-rectangular 1 1) (make-rectangular 2 3))))               ; compnum / compnum
; zero div
(error (/ 1 0))
(error (/ (/ 1 3) 0))
(error (/ (+ (greatest-fixnum) 1) 0))
(+inf.0 (/ (inexact (/ 1 3)) 0))
(error (/ (make-rectangular 1 1) 0))
(+inf.0 (/ 3 (inexact 0)))
; normalize compnum
(#t (let1 v (make-rectangular 3 0)
        (and (fixnum? v)
             (= v 3))))
;; number predicate
(#t (number? 3))
(#t (number? (/ 3 4)))
(#t (number? (inexact (/ 3 4))))
(#t (number? (+ (greatest-fixnum) 1)))
(#t (number? (make-rectangular 3 2)))
(#f (number? "string"))
(#f (number? #vu8(1 2)))
(#t (exact? 3))
(#t (exact? (+ (greatest-fixnum) 1)))
(#t (exact? (/ 1 3)))
(#f (exact? (inexact (/ 1 3))))
(#t (exact? (make-rectangular 1 2)))
(#f (exact? (make-rectangular (inexact (/ 1 3)) 3)))
(#f (inexact? 3))
(#f (inexact? (+ (greatest-fixnum) 1)))
(#f (inexact? (/ 1 3)))
(#t (inexact? (inexact (/ 1 3))))
(#f (inexact? (make-rectangular 1 2)))
(#t (inexact? (make-rectangular (inexact (/ 1 3)) 3)))

;; exact/inexact
(2 (exact 2))
(#t (= (exact (+ (greatest-fixnum) 1)) (+ (greatest-fixnum) 1)))
(#t (= (exact (/ 1 3)) (/ 1 3)))
(#t (= (make-rectangular 1 2)  (make-rectangular 1 (inexact (/ 2 1)))))
(2 (exact (inexact (/ 4 2))))

[definition
(define my-nan (/ (inexact 0) (inexact 0)))
(define minus-inf (/ (inexact -1) (inexact 0)))
(define plus-inf (/ (inexact 1) (inexact 0)))
(define t0.0 (inexact 0))
(define t0.5 (inexact (/ 1 2)))
(define t1.0 (inexact 1))
(define t2.0 (inexact 2))
(define t3.0 (inexact 3))
(define t3.1 (inexact (/ 31 10)))
(define t3.2 (inexact (/ 32 10)))
(define t5.0 (inexact 5))
(define t-5.0 (inexact -5))
(define t-3.0 (inexact -3))
(define t-2.0 (inexact -2))
(define t-1.0 (inexact -1))
]

;; zero?/positive?/negative?
(#t (zero? 0))
(#t (zero? t0.0)) ; 0.0
(#t (zero? (make-rectangular 0 0)))
(#f (let ([my-nan (/ t0.0 t0.0)])
      (zero? my-nan)))
(#t (positive? 3))
(#t (positive? (+ (greatest-fixnum) 1)))
(#t (positive? (/ 1 3)))
(#t (positive? (inexact (/ 1 3))))
(error (positive? (make-rectangular 1 1)))
(#f (negative? 3))
(#f (negative? (+ (greatest-fixnum) 1)))
(#f (negative? (/ 1 3)))
(#f (negative? (inexact (/ 1 3))))
(error (negative? (make-rectangular 1 1)))
(#f  (negative? my-nan))
(#f (positive? my-nan))
(#t (positive? plus-inf))
(#t (negative? minus-inf))

;; nan?/finite?/infinite?
(#t (nan? (/ t0.0 t0.0)))
(#t (finite? 5))
(#t (finite? t3.0))
(#f (infinite? t3.0))
(#f (finite? plus-inf))
(#t (infinite? plus-inf))

;; +0.0 -0.0
;(todo todo todo)

;; write flonum
("+inf.0" (format "~a" plus-inf))
("-inf.0" (format "~a" minus-inf))
("+nan.0" (format "~a" my-nan))

;; numerator/denominator
(3 (numerator (/ 6 4)))
(2 (denominator (/ 6 4)))
(#t (= t2.0 (denominator (inexact (/ 6 4)))))
(1 (denominator 0))
(#t (= t1.0 (denominator t0.0)))

;; flonums
(#t (= t3.0 (real->flonum 3)))
(#t (= t3.0 (real->flonum (/ 3 1))))
(#t (= t3.0 (real->flonum t3.0)))
(error (real->flonum (make-rectangular 1 1)))
(#t (fl=? t3.0 t3.0 t3.0))
(#f (fl=? t3.0 t3.1 t3.0))
(error (fl=? t3.0 3 t3.0))
(#f (fl<? t3.0 t3.0 t3.0))
(#t (fl<? t3.0 t3.1 t3.2))
(error (fl<? t3.0 3 t3.0))
(#t (fl<=? t3.0 t3.0 t3.2))
(#t (fl<=? t3.0 t3.1 t3.2))
(#f (fl<=? t3.1 t3.0 t3.2))
(error (fl<=? t3.0 3 t3.0))
(#f (fl>? t3.0 t3.0 t3.0))
(#t (fl>? t3.2 t3.1 t3.0))
(error (fl>? t3.0 3 t3.0))
(#t (fl>=? t3.0 t3.0 t3.0))
(#t (fl>=? t3.2 t3.1 t3.0))
(error (fl>=? t3.0 3 t3.0))
(#t (fl=? plus-inf plus-inf))
(#f (fl=? minus-inf plus-inf))
(#t (fl=? minus-inf minus-inf))
(#f (fl=? my-nan t3.0))
(#f (fl<? my-nan t3.0))
(#t (flinteger? t3.0))
(#f (flinteger? t3.1))
(#t (flzero? t0.0))
(#f (flpositive? t0.0))
(#t (flpositive? t3.0))
(#f (flnegative? t0.0))
(#t (flnegative? t-3.0))
(#t (flodd? t3.0))
(#f (fleven? t3.0))
(#f (flodd? t2.0))
(#t (fleven? t2.0))
(error (fleven? t3.1))
(#f (flfinite? plus-inf))
(#t (flfinite? t3.0))
(#f (flinfinite? t3.0))
(#t (flinfinite? plus-inf))
(#t (= t3.2 (flmax t3.1 t3.0 t3.2 t-3.0)))
(#t (nan? (flmax t3.1 t3.0 t3.2 my-nan t-3.0)))
(#t (= t-3.0 (flmin t3.1 t3.0 t3.2 t-3.0)))
(#t (nan? (flmin t3.1 t3.0 t3.2 my-nan t-3.0)))
(#t (fl=? t0.0 (fl+)))
(#t (fl=? t3.0 (fl+ t0.0 t1.0 t2.0)))
(#t (fl=? t1.0 (fl*)))
(#t (fl=? t2.0 (fl* t1.0 t2.0)))
(#t (nan? (fl+ plus-inf minus-inf)))
(#t (nan? (fl+ my-nan t2.0)))
(#t (nan? (fl* my-nan t2.0)))
(#t (fl=? t-3.0 (fl- t3.0)))
(#t (fl=? t2.0 (fl- t3.0 t1.0 t0.0)))
(#t (nan? (fl- plus-inf plus-inf)))
(#t (fl=? t1.0 (fl/ t1.0)))
(#t (fl=? t2.0 (fl/ t1.0 t0.5)))
(#t (fl=? t2.0 (fl/ t1.0 t0.5 t1.0)))
(#t (fl=? plus-inf (fl/ t1.0 t0.0)))
(#t (fl=? minus-inf (fl/ t-1.0 t0.0)))
(#t (nan? (fl/ t0.0 t0.0)))
(#t (positive? (flabs t-3.0)))
(#t (fl=? t3.0 (flabs t-3.0)))
(#t (fl=? (fldiv t3.0 t2.0) t1.0))
(#t (fl=? (flmod t3.0 t2.0) t1.0))
(#t (fl=? (fldiv t5.0 t-2.0) t-2.0))
(#t (fl=? (flmod t5.0 t-2.0) t1.0))
(#t (fl=? (fldiv t-5.0 t2.0) t-3.0))
(#t (fl=? (flmod t-5.0 t2.0) t1.0))
(#t (fl=? (fldiv t-5.0 t-2.0) t3.0))
(#t (fl=? (flmod t-5.0 t-2.0) t1.0))
(#t (receive (div mod) (fldiv-and-mod t5.0 t-2.0)
      (and (fl=? div t-2.0)
           (fl=? mod t1.0))))
[definition
(define t123.0 (inexact 123))
(define t-123.0 (inexact -123))
(define t10.0 (inexact 10))
(define t-10.0 (inexact -10))
(define t12.0 (inexact 12))
(define t-12.0 (inexact -12))
]
(#t (fl=? (fldiv0 t123.0 t10.0) t12.0))
(#t (fl=? (flmod0 t123.0 t10.0) t3.0))
(#t (fl=? (fldiv0 t123.0 t-10.0) t-12.0))
(#t (fl=? (flmod0 t123.0 t-10.0) t3.0))
(#t (fl=? (fldiv0 t-123.0 t10.0) t-12.0))
(#t (fl=? (flmod0 t-123.0 t10.0) t-3.0))
(#t (fl=? (fldiv0 t-123.0 t-10.0) t12.0))
(#t (fl=? (flmod0 t-123.0 t-10.0) t-3.0))
(#t (receive (div mod) (fldiv0-and-mod0 t-123.0 t-10.0)
      (and (fl=? div t12.0)
           (fl=? mod t-3.0))))

[definition
(define t0.75 (inexact (/ 3 4)))
(define t4.0 (inexact 4))
]
(#t (fl=? plus-inf (flnumerator plus-inf)))
(#t (fl=? minus-inf (flnumerator minus-inf)))
(#t (fl=? t1.0 (fldenominator plus-inf)))
(#t (fl=? t1.0 (fldenominator minus-inf)))
(#t (fl=? (flnumerator t0.75) t3.0))   ; probably
(#t (fl=? (fldenominator t0.75) t4.0)) ; probably

[definition
(define t-3.1 (inexact (/ -31 10)))
(define t3.8 (inexact (/ 38 10)))
(define t-3.8 (inexact (/ -38 10)))
(define t-4.0 (inexact -4))
]
(#t (fl=? (flfloor t3.1) t3.0))
(#t (fl=? (flfloor t-3.1) t-4.0))
(#t (fl=? (flceiling t3.1) t4.0))
(#t (fl=? (flceiling t-3.1) t-3.0))
(#t (fl=? (fltruncate t3.1) t3.0))
(#t (fl=? (fltruncate t-3.1) t-3.0))
(#t (fl=? (flround t3.1) t3.0))
(#t (fl=? (flround t-3.1) t-3.0))
(#t (fl=? (flround t3.8) t4.0))
(#t (fl=? (flround t-3.8) t-4.0))
(#t (fl=? (flfloor plus-inf) plus-inf))
(#t (fl=? (flceiling minus-inf) minus-inf))
(#t (nan? (fltruncate my-nan)))

(#t (almost=? (flexp t2.0) t7.389))
(#t (almost=? (fllog t7.389) t2.0))
(#t (almost=? (fllog t1024.0 t2.0) t10.0))
(#t (almost=? (flsin t0.0) t0.0))
(#t (almost=? (flsin t1.570796) t1.0))
(#t (almost=? (flcos t1.570796) t0.0))
(#t (almost=? (flcos t0.0) t1.0))
(#t (almost=? (flatan t0.0 t1.0) t0.0))
(#t (almost=? (flatan t0.0 t-1.0) (* t1.570796 t2.0)))
(#t (almost=? (flatan t1.0 t0.0) t1.570796))
(#t (almost=? (flatan t-1.0 t0.0) t-1.570796))
(#t (almost=? (flatan t1.0 t1.0) (/ t1.570796 t2.0)))
(#t (almost=? (flatan t-1.0 t1.0) (/ t-1.570796 t2.0)))
(#t (almost=? (flatan t0.0) t0.0))
(#t (almost=? (flatan t1.0) (/ t1.570796 t2.0)))
(#t (almost=? (flatan t10.0) t1.47113))
(#t (almost=? (flatan t0.1) t0.0996687))
(#t (almost=? (flsqrt t4.0) t2.0))
(#t (almost=? (flsqrt t5.0) t2.23607))
(#t (almost=? (flexpt t2.0 t3.0) t8.0))
(#t (almost=? (flexpt t10.0 t3.0) t1000.0))
(#t (fl=? t3.0 (fixnum->flonum 3)))

;; bitwise
(-13 (bitwise-not 12))
(11 (bitwise-not -12))
(0 (bitwise-not -1))
(-1 (bitwise-not 0))
(#t (= (least-fixnum) (bitwise-not (greatest-fixnum))))
(#t (= (greatest-fixnum) (bitwise-not (least-fixnum))))
(0 (bitwise-and 0 0))
(0 (bitwise-and 0 1))
(1 (bitwise-and 1 1))
(3 (bitwise-and 7 3))
(1 (bitwise-and 7 3 1))
(0 (bitwise-ior 0 0))
(1 (bitwise-ior 0 1))
(1 (bitwise-ior 1 1))
(7 (bitwise-ior 7 3))
(7 (bitwise-ior 7 3 1))
(0 (bitwise-xor 0 0))
(1 (bitwise-xor 0 1))
(0 (bitwise-xor 1 1))
(4 (bitwise-xor 7 3))
(5 (bitwise-xor 7 3 1))
(9 (bitwise-if 9 15 0))
(1 (bitwise-bit-count 1))
(2 (bitwise-bit-count 3))
(-1 (bitwise-bit-count -1))
(1 (bitwise-length 1))
(2 (bitwise-length 3))
(#t (= (fixnum-width) (bitwise-length (+ (greatest-fixnum) 1))))
(1 (bitwise-first-bit-set 2))
(2 (bitwise-first-bit-set 4))
(-1 (bitwise-first-bit-set 0))
(32 (bitwise-first-bit-set (* 256 256 256 256)))
(40 (bitwise-first-bit-set (* 256 256 256 256 256)))
(error (bitwise-arithmetic-shift-left 1 -1))
(2 (bitwise-arithmetic-shift-left 1 1))
(4 (bitwise-arithmetic-shift-left 1 2))
(8 (bitwise-arithmetic-shift-left 1 3))
(16 (bitwise-arithmetic-shift-left 1 4))
(32 (bitwise-arithmetic-shift-left 1 5))
(#t (fixnum? (bitwise-arithmetic-shift-left 1 5)))
(#t (= (bitwise-arithmetic-shift-left (* 256 256 256 256) 1) (* 256 256 256 256 2)))
(1 (bitwise-arithmetic-shift-right 32 5))
(#t (= (bitwise-arithmetic-shift-right (* 256 256 256 256 2) 1) (* 256 256 256 256)))
(-3 (bitwise-arithmetic-shift -6 -1))
(-3 (bitwise-arithmetic-shift -5 -1))
(-2 (bitwise-arithmetic-shift -4 -1))
(-2 (bitwise-arithmetic-shift -3 -1))
(-1 (bitwise-arithmetic-shift -2 -1))
(-1 (bitwise-arithmetic-shift -1 -1))
(88 (bitwise-reverse-bit-field 82 1 4))
(#t (bitwise-bit-set? -1 300))
(#t (bitwise-bit-set? -1 0))
(#f (bitwise-bit-set? -2 0))

;; Numerical type predicates
(#t (complex? (make-rectangular 3 4)))
(#t (complex? 3))
(#t (real? 3))
(#f (real? (make-rectangular t3.1 t0.0)))
(#t (real? (make-rectangular t3.1 0)))
(#t (real? t-3.1))
(#t (rational? (/ 6 10)))
(#t (rational? (/ 6 3)))
(#t (rational? 2))
(#t (integer? (make-rectangular 3 0)))
(#t (integer? t3.0))
(#t (integer? (/ 8 4)))
(#t (number? my-nan))
(#t (complex? my-nan))
(#t (real? my-nan))
(#f (rational? my-nan))
(#t (complex? plus-inf))
(#t (real? minus-inf))
(#f (rational? minus-inf))
(#f (integer? minus-inf))
(#t (real-valued? my-nan))
(#t (real-valued? (make-rectangular my-nan 0)))
(#t (real-valued? minus-inf))
(#t (real-valued? 3))
(#t (real-valued? (make-rectangular t3.1 t0.0)))
(#t (real-valued? (make-rectangular t3.1 0)))
(#t (real-valued? t-3.1))
(#f (rational-valued? my-nan))
(#f (rational-valued? minus-inf))
(#t (rational-valued? (/ 6 10)))
(#t (rational-valued? (make-rectangular (/ 6 10) t0.0)))
(#t (rational-valued? (make-rectangular (/ 6 10) t0.0)))
(#t (rational-valued? (/ 6 3)))
(#t (integer-valued? (make-rectangular 3 0)))
(#t (integer-valued? (make-rectangular 3 t0.0)))
(#t (integer-valued? t3.0))
(#t (integer-valued? (make-rectangular t3.0 t0.0)))
(#t (integer-valued? (/ 8 4)))

;; max, min
(error      (max))
(3          (max 3))
(4          (max 3 4))
(5          (max 3 5 4))
(2147483648 (max 1073741824 536870912 2147483648)) ; (max 2^30 2^29 2^31)
(0          (- (/ 1 2) (max (/ 1 2) (/ 1 4) (/ 1 3))))
(#t         (= t4.0 (max t3.0 t-3.1 t4.0)))
(4.0        (max 3.9 4))
(#t         (nan? (max my-nan 3.9 4 (/ 1 2) t4.0)))
(#f         (fl=? plus-inf (max plus-inf my-nan)))
(#t         (fl=? plus-inf (max plus-inf 3.9 4 (/ 1 2) t4.0)))
(error      (min))
(3          (min 3))
(3          (min 3 4))
(3          (min 3 5 4))
(536870912  (min 1073741824 536870912 2147483648)) ; (min 2^30 2^29 2^31)
(0          (- (/ 1 4) (min (/ 1 2) (/ 1 4) (/ 1 3))))
(#t         (= t-3.1 (min t3.0 t-3.1 t4.0)))
(3.9        (min 3.9 4))
(#t         (nan? (min my-nan 3.9 4 (/ 1 2) t4.0)))
(#f         (fl=? minus-inf (min minus-inf my-nan)))
(#t         (fl=? minus-inf (min minus-inf 3.9 4 (/ 1 2) t4.0)))

;; fixnums
(error (fx=?))
(error (fx=? 3))
(#t    (fx=? 3 3))
(#f    (fx=? 3 4))
(#t    (fx=? 3 3 3))
(error (fx=? 3.0 3))
(error (fx=? (+ (greatest-fixnum) 1) (+ (greatest-fixnum) 1)))
(error (fx>?))
(error (fx>? 3))
(#t    (fx>? 4 3))
(#f    (fx>? 3 4))
(#t    (fx>? 5 4 3))
(error (fx>? 4.0 3))
(error (fx>? (+ (greatest-fixnum) 1) (greatest-fixnum)))
(error (fx<?))
(error (fx<? 3))
(#t    (fx<? 3 4))
(#f    (fx<? 4 3))
(#t    (fx<? 3 4 5))
(error (fx<? 3 4.0))
(error (fx<? (greatest-fixnum) (+ (greatest-fixnum) 1)))
(error (fx>=?))
(error (fx>=? 3))
(#t    (fx>=? 3 3))
(#t    (fx>=? 4 3))
(#f    (fx>=? 3 4))
(#t    (fx>=? 4 3 3))
(error (fx>=? 4 3.0))
(error (fx>=? (+ (greatest-fixnum) 1) (greatest-fixnum)))
(error (fx<=?))
(error (fx<=? 3))
(#t    (fx<=? 3 3))
(#t    (fx<=? 3 4))
(#f    (fx<=? 4 3))
(#t    (fx<=? 3 3 4))
(error (fx<=? 3.0 4))
(error (fx<=? (greatest-fixnum) (+ (greatest-fixnum) 1)))
(#f    (fxzero? -1))
(#t    (fxzero? 0))
(#f    (fxzero? 1))
(error (fxzero?))
(error (fxzero? 0.0))
(error (fxzero? 0 0))
(#f    (fxpositive? -1))
(#f    (fxpositive? 0))
(#t    (fxpositive? 1))
(error (fxpositive?))
(error (fxpositive? 1.0))
(error (fxpositive? (+ (greatest-fixnum) 1)))
(error (fxpositive? 1 2))
(#t    (fxnegative? -1))
(#f    (fxnegative? 0))
(#f    (fxnegative? 1))
(error (fxnegative?))
(error (fxnegative? -1.0))
(error (fxnegative? (- (least-fixnum) 1)))
(error (fxnegative? -1 -2))
(#f    (fxodd? -2))
(#t    (fxodd? -1))
(#f    (fxodd? 0))
(#t    (fxodd? 1))
(#f    (fxodd? 2))
(error (fxodd?))
(error (fxodd? -1 1))
(#t    (fxeven? -2))
(#f    (fxeven? -1))
(#t    (fxeven? 0))
(#f    (fxeven? 1))
(#t    (fxeven? 2))
(error (fxeven?))
(error (fxeven? -2 2))
(error (fxmax))
(1     (fxmax 1))
(1     (fxmax 1 -1))
(2     (fxmax 1 2 -1))
(error (fxmax (+ (greatest-fixnum) 1)))
(error (fxmax 1.0))
(error (fxmin))
(1     (fxmin 1))
(-1    (fxmin 1 -1))
(-1    (fxmin 1 -1 2))
(error (fxmin (- (least-fixnum) 1)))
(error (fxmin 1.0))
(error (fx+))
(error (fx+ 1))
(5     (fx+ 2 3))
(-5    (fx+ -2 -3))
(error (fx+ 4 5 6))
(error (fx+ 2.0 3))
(error (fx+ (greatest-fixnum) 1))
(error (fx+ (least-fixnum) -1))
(error (fx*))
(error (fx* 1))
(9     (fx* 3 3))
(-9    (fx* -3 3))
(#t    (fx=? -268435456 (fx* -134217728 2))) ; -(2^28) (fx* -(2^27) 2)
(error (fx* (greatest-fixnum) 2))
(error (fx* 3.0 3))
(error (fx-))
(3     (fx- -3))
(-7    (fx- 7))
(error (fx- 1.0))
(error (fx- (least-fixnum)))
(4     (fx- 7 3))
(error (fx- 7 3.0))
(error (fx- (least-fixnum) 1))
(error (fxdiv-and-mod))
(error (fxdiv-and-mod 123))
(mosh-only  12 (receive (a b) (fxdiv-and-mod  123  10) a))
(mosh-only   3 (receive (a b) (fxdiv-and-mod  123  10) b))
(mosh-only -12 (receive (a b) (fxdiv-and-mod  123 -10) a))
(mosh-only   3 (receive (a b) (fxdiv-and-mod  123 -10) b))
(mosh-only -13 (receive (a b) (fxdiv-and-mod -123  10) a))
(mosh-only   7 (receive (a b) (fxdiv-and-mod -123  10) b))
(mosh-only  13 (receive (a b) (fxdiv-and-mod -123 -10) a))
(mosh-only   7 (receive (a b) (fxdiv-and-mod -123 -10) b))
(error (fxdiv-and-mod 123 0))
(error (fxdiv-and-mod 123.0 10.0))
(error (fxdiv))
(error (fxdiv 123))
(12    (fxdiv 123 10))
(-12   (fxdiv 123 -10))
(-13   (fxdiv -123 10))
(13    (fxdiv -123 -10))
(error (fxdiv 123 0))
(error (fxdiv 123.0 10.0))
(error (fxmod))
(error (fxmod 123))
(3     (fxmod 123 10))
(3     (fxmod 123 -10))
(7     (fxmod -123 10))
(7     (fxmod -123 -10))
(error (fxmod 123 0))
(error (fxmod 123.0 10.0))
(mosh-only  12 (receive (a b) (fxdiv0-and-mod0  123  10) a))
(mosh-only   3 (receive (a b) (fxdiv0-and-mod0  123  10) b))
(mosh-only -12 (receive (a b) (fxdiv0-and-mod0  123 -10) a))
(mosh-only   3 (receive (a b) (fxdiv0-and-mod0  123 -10) b))
(mosh-only -12 (receive (a b) (fxdiv0-and-mod0 -123  10) a))
(mosh-only  -3 (receive (a b) (fxdiv0-and-mod0 -123  10) b))
(mosh-only  12 (receive (a b) (fxdiv0-and-mod0 -123 -10) a))
(mosh-only  -3 (receive (a b) (fxdiv0-and-mod0 -123 -10) b))
(error (fxdiv0-and-mod0 123 0))
(error (fxdiv0-and-mod0 123.0 10.0))
(error (fxdiv0))
(error (fxdiv0 123))
(12    (fxdiv0 123 10))
(-12   (fxdiv0 123 -10))
(-12   (fxdiv0 -123 10))
(12    (fxdiv0 -123 -10))
(error (fxdiv0 123 0))
(error (fxdiv0 123.0 10.0))
(error (fxmod0))
(error (fxmod0 123))
(3     (fxmod0 123 10))
(3     (fxmod0 123 -10))
(-3    (fxmod0 -123 10))
(-3    (fxmod0 -123 -10))
(error (fxmod0 123 0))
(error (fxmod0 123.0 10.0))
(mosh-only 5 (receive (a b) (fx+/carry (greatest-fixnum) (greatest-fixnum) 7) a))
(mosh-only 1 (receive (a b) (fx+/carry (greatest-fixnum) (greatest-fixnum) 7) b))
(error (fx+/carry 1.0 2 3))
(error (fx+/carry 1 2+2i 3))
(error (fx+/carry 1 2 (/ 3 2)))
(mosh-only -5 (receive (a b) (fx-/carry (least-fixnum) (greatest-fixnum) 6) a))
(mosh-only -1 (receive (a b) (fx-/carry (least-fixnum) (greatest-fixnum) 6) b))
(error (fx-/carry 1.0 2 3))
(error (fx-/carry 1 2+2i 3))
(error (fx-/carry 1 2 (/ 3 2)))
(mosh-only 3 (receive (a b) (fx*/carry 4 (greatest-fixnum) 7) a))
(mosh-only 2 (receive (a b) (fx*/carry 4 (greatest-fixnum) 7) b))
(error (fx*/carry 1.0 2 3))
(error (fx*/carry 1 2+2i 3))
(error (fx*/carry 1 2 (/ 3 2)))
(error (fxnot))
(-1    (fxnot 0))
(1     (fxnot -2))
(-2    (fxnot 1))
(error (fxnot 1.0))
(error (fxnot 1 2))
(7     (fxand 7))
(0     (fxand 7 0))
(1     (fxand 7 1))
(5     (fxand 7 5))
(4     (fxand 7 4 5))
(4     (fxand 7 5 4))
(#t    (fx=? 4 (fxand (fxand) 7 5 4)))
(error (fxand 7.0))
(7     (fxior 7))
(7     (fxior 7 0))
(5     (fxior 5 4))
(7     (fxior 5 3))
(39    (fxior 5 3 32))
(#t    (fx=? 39 (fxior (fxior) 5 3 32)))
(error (fxior 7.0))
(7     (fxxor 7))
(7     (fxxor 7 0))
(1     (fxxor 5 4))
(6     (fxxor 5 3))
(36    (fxxor 5 1 32))
(#t    (fx=? 36 (fxxor (fxxor) 5 1 32)))
(error (fxxor 7.0))
(5     (fxif 5 15 0))
(10    (fxif 5 0 15))
(0     (fxif 5 0 1))
(2     (fxif 5 0 3))
(1     (fxif 5 3 0))
(error (fxif 5 15))
(error (fxif 5 15 0.0))
(error (fxbit-count))
(2     (fxbit-count 5))
(2     (fxbit-count 6))
(3     (fxbit-count 7))
(-3    (fxbit-count -7))
(error (fxbit-count 5.0))
(error (fxlength))
(1     (fxlength 1))
(8     (fxlength 255))
(0     (fxlength 0))
(1     (fxlength -2))
(8     (fxlength -255))
(error (fxlength 1.0))
(error (fxfirst-bit-set))
(-1    (fxfirst-bit-set 0))
(0     (fxfirst-bit-set 1))
(4     (fxfirst-bit-set 16))
(1     (fxfirst-bit-set -2))
(17    (fxfirst-bit-set (expt 2 17)))
(error (fxfirst-bit-set 4.0))
(#t    (fxbit-set? 15 0))
(#f    (fxbit-set? 14 0))
(#t    (fxbit-set? 14 3))
(#f    (fxbit-set? 14 10))
(#t    (fxbit-set? -1 10))
(#t    (fxbit-set? -1 (- (fixnum-width) 1)))
(error (fxbit-set? -1 (fixnum-width)))
(error (fxbit-set? 14 -3))
(error (fxarithmetic-shift))
(error (fxarithmetic-shift 1))
(2     (fxarithmetic-shift 1 1))
(0     (fxarithmetic-shift 1 -1))
(40    (fxarithmetic-shift 10 2))
(10    (fxarithmetic-shift 40 -2))
(-2    (fxarithmetic-shift -1 1))
(-1    (fxarithmetic-shift -1 -1))
(-40   (fxarithmetic-shift -10 2))
(-10   (fxarithmetic-shift -40 -2))
(error (fxarithmetic-shift (greatest-fixnum) 1))
(error (fxarithmetic-shift 1.0 1))
(error (fxarithmetic-shift-left))
(error (fxarithmetic-shift-left 1))
(2     (fxarithmetic-shift-left 1 1))
(40    (fxarithmetic-shift-left 10 2))
(-2    (fxarithmetic-shift-left -1 1))
(-40   (fxarithmetic-shift-left -10 2))
(error (fxarithmetic-shift-left (greatest-fixnum) 1))
(error (fxarithmetic-shift-left 1.0 1))
(error (fxarithmetic-shift-right))
(error (fxarithmetic-shift-right 1))
(0     (fxarithmetic-shift-right 1 1))
(10    (fxarithmetic-shift-right 40 2))
(-1    (fxarithmetic-shift-right -1 1))
(-10   (fxarithmetic-shift-right -40 2))
(error (fxarithmetic-shift-right 1.0 1))
(1     (fxcopy-bit 0 0 1))
(2     (fxcopy-bit 0 1 1))
(16    (fxcopy-bit 0 4 1))
(0     (fxcopy-bit 0 4 0))
(15    (fxcopy-bit 31 4 0))
(error (fxcopy-bit 0 -1 1))
(error (fxcopy-bit 0 (+ (fixnum-width) 1) 1))
(error (fxcopy-bit 0 1 2))
(3     (fxbit-field 30 1 3))
(7     (fxbit-field 30 1 4))
(15    (fxbit-field 30 1 5))
(15    (fxbit-field 30 1 6))
(6     (fxbit-field 30 0 3))
(error (fxbit-field 30 -1 3))
(error (fxbit-field 30 (+ (fixnum-width) 1) 3))
(error (fxbit-field 30 1 -3))
(error (fxbit-field 30 1 (+ (fixnum-width) 1)))
(error (fxbit-field 30 3 2))
(6     (fxcopy-bit-field 0 0 3 30))
(6     (fxcopy-bit-field 7 0 3 30))
(14    (fxcopy-bit-field 15 0 3 30))
(24    (fxcopy-bit-field 0 2 5 30))
(25    (fxcopy-bit-field 1 2 5 30))
(27    (fxcopy-bit-field 7 2 5 30))
(27    (fxcopy-bit-field 15 2 5 30))
(0     (fxcopy-bit-field 0 2 5 120))
(1     (fxcopy-bit-field 1 2 5 120))
(error (fxcopy-bit-field 0 -1 3 30))
(error (fxcopy-bit-field 0 (+ (fixnum-width) 1) 3 30))
(error (fxcopy-bit-field 0 1 -3 30))
(error (fxcopy-bit-field 0 1 (+ (fixnum-width) 1) 30))
(error (fxcopy-bit-field 0 3 2 30))
(10    (fxrotate-bit-field 10 0 2 0))
(9     (fxrotate-bit-field 10 0 2 1))
(10    (fxrotate-bit-field 10 2 4 0))
(6     (fxrotate-bit-field 10 2 4 1))
(12    (fxrotate-bit-field 10 1 4 2))
(6     (fxrotate-bit-field 10 1 4 1))
(6     (fxrotate-bit-field 10 2 4 1))
(error (fxrotate-bit-field 10 -2 4 0))
(error (fxrotate-bit-field 10 (+ (fixnum-width) 1) 4 0))
(error (fxrotate-bit-field 10 2 -4 0))
(error (fxrotate-bit-field 10 2 (+ (fixnum-width) 1) 0))
(error (fxrotate-bit-field 10 1 4 -2))
(error (fxrotate-bit-field 10 1 4 (+ (fixnum-width) 1)))
(error (fxrotate-bit-field 10 4 2 0)) ; (> fx2 fx3)
(error (fxrotate-bit-field 10 2 4 2)) ; (> fx4 (- fx3 fx2))
(88    (fxreverse-bit-field #b1010010 1 4)) ; #b1011000
(84    (fxreverse-bit-field #b1010010 1 3)) ; #b1010100
(error (fxreverse-bit-field #b1010010 -1 4))
(error (fxreverse-bit-field #b1010010 (+ (fixnum-width) 1) 4))
(error (fxreverse-bit-field #b1010010 1 -4))
(error (fxreverse-bit-field #b1010010 4 2)) ; (> fx2 fx3)

;; number reader 2
(1 #b1)
(0 #b0)
(1 #b1)
(5 #b101)
(5 #e#b101)
(#t (= (inexact 5) #i#b101))
(-3 #b-11)
(-3 #e#b-11)
(#t (= (inexact -5) #i#b-101))
(#t (= (/ 3 2) #b11/10))
(#t (= #b11+10i (make-rectangular 3 2)))
(#t (= #b11-10i (make-rectangular 3 -2)))
(#t (= #b11+i (make-rectangular 3 1)))
(#t (= #b+i (make-rectangular 0 1)))
(#t (= #b-i (make-rectangular 0 -1)))
(#t (and (nan? (imag-part #b11+nan.0i))
         (= 3 (real-part #b11+nan.0i))))
(#t (and (nan? (imag-part #b+nan.0i))
         (zero? (real-part #b+nan.0i))))
(#t (almost=? (real-part #b1@1) (real-part 1@1)))

;; number reader 8
(1 #o1)
(0 #o0)
(1 #o1)
(65 #o101)
(65 #e#o101)
(#t (= (inexact 65) #i#o101))
(-9 #o-11)
(-9 #e#o-11)
(#t (= (inexact -65) #i#o-101))
(#t (= (/ 9 8) #o11/10))
(#t (= #o11+10i (make-rectangular 9 8)))
(#t (= #o11-10i (make-rectangular 9 -8)))
(#t (= #o11+i (make-rectangular 9 1)))
(#t (= #o+i (make-rectangular 0 1)))
(#t (= #o-i (make-rectangular 0 -1)))
(#t (and (nan? (imag-part #o11+nan.0i))
         (= 9 (real-part #o11+nan.0i))))
(#t (and (nan? (imag-part #o+nan.0i))
         (zero? (real-part #o+nan.0i))))
(#t (almost=? (real-part #o1@1) (real-part 1@1)))

;; number reader 10
(101 101)
(-101 -101)
(-10000000000000001 -10000000000000001)
(#t (nan? +nan.0))
(#t (infinite? +inf.0))
(2/3 (/ 2 3))
(#t (almost=? .45 (inexact 45/100)))
(#t (fl=? .45 0.45))
(0.45 .45)
(1.45 (inexact 145/100))
(#t (infinite? (imag-part +inf.0i)))
(#t (= 10+11i (make-rectangular 10 11)))
(#t (= 10-11i (make-rectangular 10 -11)))
(#t (= 10+i (make-rectangular 10 1)))
(#t (= 10-i (make-rectangular 10 -1)))
(#t (= 3.1+i (make-rectangular 3.1 1)))
(#t (= 3.1-i (make-rectangular 3.1 -1)))
(#t (= +3.25i (make-rectangular 0 3.25)))
(#t (= -3.25i (make-rectangular 0 -3.25)))
(#t (= +i (make-rectangular 0 1)))
(#t (= -i (make-rectangular 0 -1)))
(#t (nan? (imag-part 10+nan.0i)))
(10 (real-part 10+nan.0i))
(#t (nan? (imag-part 10-nan.0i)))
(10 (real-part 10-nan.0i))
(#t (nan? (imag-part +nan.0i)))
(0 (real-part +nan.0i))
(#t (nan? (imag-part -nan.0i)))
(0 (real-part -nan.0i))
(#t (nan? (imag-part 10.0+nan.0i)))
(10.0 (real-part 10.0+nan.0i))
(320.0 3.2e+2)
(0.32 3.2e-1)
(20.0 .2e+2)
(0.02 .2e-1)
(50 5e+1)
(500000000000 5e+11)
("+1i" (format "~a" +1i))
("-1i" (format "~a" -1i))
("1" (format "~a" 1+0i))
("1" (format "~a" 1-0i))
("+1i" (format "~a" 0+i))
("+inf.0i" (format "~a" +inf.0i))
(#t (almost=? 1 (real-part 2@1.047))) ;pi/3
(#t (almost=? -1 (real-part 2@2.0943))) ;2pi/3

;; number reader 16
(1 #x1)
(0 #x0)
(1 #x1)
(257 #x101)
(257 #e#x101)
(#t (= (inexact 257) #i#x101))
(-17 #x-11)
(-17 #e#x-11)
(#t (= (inexact -257) #i#x-101))
(#t (= (/ 17 16) #x11/10))
(#t (= #x11+10i (make-rectangular 17 16)))
(#t (= #x11-10i (make-rectangular 17 -16)))
(#t (= #x11+i (make-rectangular 17 1)))
(#t (= #x+i (make-rectangular 0 1)))
(#t (= #x-i (make-rectangular 0 -1)))
(#t (and (nan? (imag-part #x11+nan.0i))
         (= 17 (real-part #x11+nan.0i))))
(#t (and (nan? (imag-part #x+nan.0i))
         (zero? (real-part #x+nan.0i))))
(#t (almost=? (real-part #x1@1) (real-part 1@1)))

;; even?/odd?
(#t (even? 2))
(#t (even? 100000000000000000000000000000000000000000000000))
(#t (even? 4+0i))
(#t (even? 4.0))
(#f (odd? 2))
(#f (odd? 100000000000000000000000000000000000000000000000))
(#f (odd? 4+0i))
(#f (odd? 4.0))

;; +-*/
(3333333333333333333333333333333333333 (apply + '(3333333333333333333333333333333333333)))
(4+5i (+ 3+i 1+4i))
(#t (almost=? 3.45 (+ 3.0 0.450)))
(1 (+ 1/2 1/2))
(2/3 (+ 1/3 1/3))
(-3333333333333333333333333333333333333 (apply - '(3333333333333333333333333333333333333)))
(2-3i (- 3+i 1+4i))
(#t (almost=? 2.55 (- 3.0 0.450)))
(0 (- 1/2 1/2))
(1/3 (- 2/3 1/3))
(1/3333333333333333333333333333333333333 (apply / '(3333333333333333333333333333333333333)))
(1/3333333333333333333333333333333333333 (/ 3333333333333333333333333333333333333))
(7/17-11/17i (/ 3+i 1+4i))
(#t (almost=? 6.66666 (/ 3.0 0.450)))
(1 (/ 1/2 1/2))
(2 (/ 2/3 1/3))

;; abs
(7 (abs -7))
(+inf.0 (abs -inf.0))
(33333333333333333333 (abs -33333333333333333333))
(0.35 (abs -0.35))

;; div
(333333333333333333 (div 1000000000000000000 3))
(error (div 10 0))
(error (div +inf.0 1))
(error (div -inf.0 1))
(error (div +nan.0 1))
;; Flonum : div/div0/mod/mod0/
(3.0 (div 13.9 3.5))
(#t (almost=? 3.4 (mod 13.9 3.5)))
(4.0 (div0 13.9 3.5))
(#t (almost=? -0.1 (mod0 13.9 3.5)))
(-3.0 (div 13.9 -3.5))
(#t (almost=? 3.4 (mod 13.9 -3.5)))
(-4.0 (div0 13.9 -3.5))
(#t (almost=? -0.1 (mod0 13.9 -3.5)))
(4.0 (div -13.9 -3.5))
(#t (almost=? 0.1 (mod -13.9 -3.5)))
(4.0 (div0 -13.9 -3.5))
(#t (almost=? 0.1 (mod0 -13.9 -3.5)))
(-4.0 (div -13.9 3.5))
(#t (almost=? 0.1 (mod -13.9 3.5)))
(-4.0 (div0 -13.9 3.5))
(#t (almost=? 0.1 (mod0 -13.9 3.5)))

;; Bignum : div/div0/mod/mod0/
(3 (div 13900000000000000000000 3500000000000000000000))
(3400000000000000000000 (mod 13900000000000000000000 3500000000000000000000))
(4 (div0 13900000000000000000000 3500000000000000000000))
(-100000000000000000000 (mod0 13900000000000000000000 3500000000000000000000))
(-3 (div 13900000000000000000000 -3500000000000000000000))
(3400000000000000000000 (mod 13900000000000000000000 -3500000000000000000000))
(-4 (div0 13900000000000000000000 -3500000000000000000000))
(-100000000000000000000 (mod0 13900000000000000000000 -3500000000000000000000))
(4 (div -13900000000000000000000 -3500000000000000000000))
(100000000000000000000 (mod -13900000000000000000000 -3500000000000000000000))
(4 (div0 -13900000000000000000000 -3500000000000000000000))
(100000000000000000000 (mod0 -13900000000000000000000 -3500000000000000000000))
(-4 (div -13900000000000000000000 3500000000000000000000))
(100000000000000000000 (mod -13900000000000000000000 3500000000000000000000))
(-4(div0 -13900000000000000000000 3500000000000000000000))
(100000000000000000000 (mod0 -13900000000000000000000 3500000000000000000000))

;; Fixnum : div/div0/mod/mod0/
(3 (div 13900 3500))
(3400 (mod 13900 3500))
(4 (div0 13900 3500))
(-100 (mod0 13900 3500))
(-3 (div 13900 -3500))
(3400 (mod 13900 -3500))
(-4 (div0 13900 -3500))
(-100 (mod0 13900 -3500))
(4 (div -13900 -3500))
(100 (mod -13900 -3500))
(4 (div0 -13900 -3500))
(100 (mod0 -13900 -3500))
(-4 (div -13900 3500))
(100 (mod -13900 3500))
(-4 (div0 -13900 3500))
(100 (mod0 -13900 3500))

;; Ratnum
(3 (div 139/10 35/10))
(34/10 (mod 139/10 35/10))
(4 (div0 139/10 35/10))
(-1/10 (mod0 139/10 35/10))
(-3 (div 139/10 -35/10))
(34/10 (mod 139/10 -35/10))
(-4 (div0 139/10 -35/10))
(-1/10 (mod0 139/10 -35/10))
(4 (div -139/10 -35/10))
(1/10 (mod -139/10 -35/10))
(4 (div0 -139/10 -35/10))
(1/10 (mod0 -139/10 -35/10))
(-4 (div -139/10 35/10))
(1/10 (mod -139/10 35/10))
(-4 (div0 -139/10 35/10))
(1/10 (mod0 -139/10 35/10))

;; gcd/lcm
(4 (gcd 32 -36))
(0 (gcd))
(10 (gcd -10))
(#t (flonum? (gcd 9.0 3)))
(288 (lcm 32 -36))
(288.0 (lcm 32.0 -36))
(1 (lcm))

;; denominator/numerator
(3 (numerator (/ 6 4)))
(2 (denominator (/ 6 4)))
(2.0 (denominator
      (inexact (/ 6 4))))

(-5.0 (floor -4.3))
(-4.0 (ceiling -4.3))
(-4.0 (truncate -4.3))
(-4.0 (round -4.3))
(3.0 (floor 3.5))
(4.0 (ceiling 3.5))
(3.0 (truncate 3.5))
(4.0 (round 3.5))
(4 (round 7/2))
(7 (round 7))
(+inf.0 (floor +inf.0))
(-inf.0 (ceiling -inf.0))
(#t (nan? (round +nan.0)))

(1/3 (rationalize (exact .3) 1/10))
(#t (almost=? #i1/3 (rationalize .3 1/10)))
(+inf.0 (rationalize +inf.0 3))
(#t (nan? (rationalize +inf.0 +inf.0)))
(0.0 (rationalize 3 +inf.0))
(+inf.0 (exp +inf.0))
(0.0 (exp -inf.0))
(#t (almost=? 2.718 (exp 1)))
(#t (almost=? 2.718 (exp 1.0)))

;; log
(-inf.0 (log 0.0))
(error (log 0))
(+inf.0 (real-part (log -inf.0)))
(#t (almost=? 3.1415  (imag-part (log -inf.0))))
(0.0 (real-part (log -1.0+0.0i)))
(#t (almost=? 3.1415 (imag-part (log -1.0+0.0i))))
(0.0 (real-part (log -1.0-0.0i)))
(#t (almost=? -3.1415 (imag-part (log -1.0-0.0i))))

;; cos/sin
(1 (cos 0))
(0 (sin 0))
(0.0 (sin 0.0))
(#t (almost=? 1.0 (sin 1.570796)))
(#t (almost=? 0.0 (cos 1.570796)))
(1.0 (cos 0.0))
(#t (almost=? 1.2984 (real-part (sin 1+1i))))
(#t (almost=? 0.6349 (imag-part (sin 1+1i))))
(#t (almost=? 1.09252 (real-part (cos 12.0+3/4i))))
(#t (almost=? 0.44123 (imag-part (cos 12.0+3/4i))))
(#t (almost=? -0.08039 (real-part (tan 2+1.5i))))
(#t (almost=? 1.0641 (imag-part (tan 2+1.5i))))
(123456789123456789123456789 (floor 123456789123456789123456789))

;; string->number
(10 (string->number "10"))
(123456789123456789123456789 (string->number "123456789123456789123456789"))
(#f (string->number "hige"))
(10 (string->number "10" 10))
(123456789123456789123456789 (string->number "123456789123456789123456789" 10))
(#f (string->number "hige" 10))
(error (string->number "hige" 11))
(2 (string->number "10" 2))
(100 (string->number "100"))
(256 (string->number "100" 16))
(100.0 (string->number "1e2"))
(#f (string->number "0/0"))
(+inf.0 (string->number "+inf.0"))
(-inf.0 (string->number "-inf.0"))
(#t (nan? (string->number "+nan.0")))
(#t (fl=? (inexact (expt 10 100)) (string->number "1.0e100")))
;(#t (fl=? (inexact (expt 10 99)) (string->number ".1e100")))
(#t (fl=? (inexact (expt 10 100)) (string->number "1.e100")))
;(#t (almost=? (inexact (expt 10 99)) (string->number "0.1e100")))

;; magnitude
(10 (magnitude 10))
(10.0 (magnitude 10.0))
(10.0 (magnitude -10.0))
(2/3 (magnitude -2/3))
(#xfffffffffffffffffffffff (magnitude #x-fffffffffffffffffffffff))
(#t (almost=? 5.3851 (magnitude 2+5i)))
;; angle
(0 (angle 0))
(0.0 (angle 0.0))
(#t (almost=? 3.14159 (angle -3)))
(#t (almost=? (/ 3.141592 4) (angle 1+1i)))
;; sqrt
(2 (sqrt 4))
(2.0 (sqrt 4.0))
(1/2 (sqrt 1/4))
(#t (almost=? 1.0986 (real-part (sqrt 1+1i))))
(#t (almost=? 0.455 (imag-part (sqrt 1+1i))))

;; asin, acos, atan
(#t (almost=? 0.666239 (real-part (asin 1+i))))
(#t (almost=? 1.061275 (imag-part (asin 1+i))))
(#t (almost=? 0.904557 (real-part (acos 1+i))))
(#t (almost=? -1.061275 (imag-part (acos 1+i))))
(#t (almost=? 1.017222 (real-part (atan 1+i))))
(#t (almost=? 0.402359 (imag-part (atan 1+i))))
(0 (atan 0 1))
(error (atan 1+i 3))
(#t (almost=? 0.785398 (atan 1 1)))
(#t (let1 val 5
      (receive (s r) (exact-integer-sqrt val)
        (= val (+ r (* s s))))))
(#t (let1 val 4
      (receive (s r) (exact-integer-sqrt val)
        (= val (+ r (* s s))))))
(#t (let1 val 3
      (receive (s r) (exact-integer-sqrt val)
        (= val (+ r (* s s))))))

;; expt
[definition
  (define unspec (if #f #f))
  (define (unspec? x) (eq? unspec x))
]
(125 (expt 5 3))
(1 (expt 5 0))
(0 (expt 0 5))
(1 (expt 0 0))
(#t (unspec? (expt 0 -5)))
(1/125 (expt 5 -3))
(#b1000000000000000000000000000000000 (expt 2 33)) ;; fixnum, fixnum => bignum
(#b1/1000000000000000000000000000000000 (expt 2 -33)) ;; fixnum, fixnum => bignum
(#t (almost=? 2.0 (expt 4 0.5)))
(#t (almost=? 0.25 (expt 4 -1.0)))
(0 (expt 0 5+.0000312i))
(#t (almost=? 0.0 (expt 0.0 5+.0000312i)))
(#t (unspec? (expt 0 -5+.0000312i)))
(error (expt 2 (+ (greatest-fixnum) 1)))
(#t (almost=? 2.0 (expt 4 1/2)))
(#t (almost=? -1.8958 (real-part (expt 2 1+5i))))
(#t (almost=? -0.6369 (imag-part (expt 2 1+5i))))
(1 (expt 1 1+5i))
(1.0 (expt 0.0 0.0))
(#t (almost=? 4.0 (expt 2.0 2)))
(#t (almost=? 4.0 (expt 2.0 2.0)))
(#t (almost=? 2.0 (expt 4.0 1/2)))
(10000000000000000000000000000000000000000000000000000000000 (expt 100000000000000000000000000000 2))
(#t (= (* (+ (greatest-fixnum) 1) (+ (greatest-fixnum) 1)) (expt (+ (greatest-fixnum) 1) 2)))
(1/4 (expt 1/2 2))
(4 (expt 1/2 -2))

;; complex things
[definition
  (define (complex-almost=? a b)
    (and (almost=? (real-part a) (real-part b))
         (almost=? (imag-part a) (imag-part b))))
]
(#t (complex-almost=? 1.1+2.2i (make-rectangular 1.1 2.2)))
(#t (complex-almost=? 1.1@2.2 (make-polar 1.1 2.2)))
(#t (almost=? 1.1 (real-part 1.1+2.2i)))
(#t (almost=? 2.2 (imag-part 1.1+2.2i)))
(#t (almost=? 1.1 (magnitude 1.1@2.2)))
(#t (almost=? 2.2 (angle 1.1@2.2)))
(#t (almost=? 3.141592653589793 (angle -1)))
(#t (almost=? 3.141592653589793 (angle -1.0)))
(#t (almost=? 3.141592653589793 (angle -1.0+0.0i)))
(#t (almost=? -3.141592653589793 (angle -1.0-0.0i)))
(#t (almost=? 0.0 (angle +inf.0)))
(#t (almost=? 3.141592653589793 (angle -inf.0)))

;; number->string
("123" (number->string 123))
("123" (number->string 123 10))
("10" (number->string 2 2))
("2" (number->string 2 16))
("a" (number->string 10 16))
("12" (number->string 10 8))
(error (number->string 1.0 8))
("1.000000" (number->string 1.0))
("1/a" (number->string 1/10 16))
("1+1i" (number->string 1+i))
(-5 (string->number (number->string -5 16)))

[definition
  (define path 3)]
;; test for miss copy $lambda-node
(1
 (let ((add (lambda (s)
              (set! path s))))
   (add #f)
   (add #f)
   1))
;; testing jump optimize
[definition
  (define aa 1)
  (define xx 0)]
[done (let loop ([i 1])
        (if (= i 10)
            'done
            (loop (+ i 1))))]
[done
 (let loop ([i 1])
   (if (= i 10)
       'done
       (let1 b aa
         (loop (+ i b)))))]
[done
 (let loop ([i 1])
   (if (= i 10)
       'done
       (let1 b aa
         (let1 c xx
         (loop (+ i b c))))))]
[done
 (let loop ([i 1])
   (if (= i 10)
       'done
       (let ([b aa]
             [c xx])
         (loop (+ i b c)))))]
[9
 (let loop ([i 1]
            [j 0])
   (if (= i 10)
       j
       (let ([b aa]
             [c xx])
         (loop (+ i b c) (+ j b)))))]
[9
 (let loop ([i 1]
            [j 0])
   (if (= i 10)
       j
       (letrec ([b aa]
                [c xx])
         (loop (+ i b c) (+ j b)))))]
[9
 (let loop ([i 1]
            [j 0])
   (if (= i 10)
       j
       (let* ([b aa]
              [c xx])
         (loop (+ i b c) (+ j b)))))]
[9 (let loop ([i 1]
              [j 0])
     (if (= i 10)
         j
         ((lambda (b c)
                    (loop (+ i b c) (+ j b))) aa xx)))]
[9 (let loop ([i 1]
              [j 0])
     (if (= i 10)
         j
         (if aa
             (let* ([b aa]
                    [c xx])
               (loop (+ i b c) (+ j b))))))]
[9
 (let loop ([i 1]
            [j 0])
   (if (= i 10)
       j
       (receive (b c) (values aa xx)
         (loop (+ i b c) (+ j b)))))]
(#t (letrec ([e (lambda (x) (if (= 0 x) #t (o (- x 1))))]
             [o (lambda (x) (if (= 0 x) #f (e (- x 1))))])  (e 50000)))
[5
 (+ 1 (letrec ((loop (lambda (lst)
                       (if (null? lst)
                           '0
                           (if (pair? lst)
                               (+ (loop (cdr lst)) '1))))))
        (loop '(1 2 3 3))))]
[1
 (let1 yy (lambda (ww) ww)
   (let1 zz (min 1)
     (yy zz)))]
;; set-count suppress optimization
[#t
(procedure?
 (letrec ((loop (lambda (x)
                  ((lambda (y) (set! loop '())) (display x)))))
   (lambda (z) (loop z))))]
