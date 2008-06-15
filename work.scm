;; (print (source-info (lambda () x)))
;; (define (a) 3)
;(a 3)
;((lambda () x) 33)

;(print (compile '(source-info (lambda () x))))
;; (define (hoge)
;;   (define (int)
;;     3)
;;   (print (source-info int))
;;   (print (source-info hoge)))
;; (hoge)
;(exit)

;; (print (cdr (lambda () x)))
;; (exit)

;(print (vector->list '#(a b c)))

;; (define a (lambda () 3))

;; (let1 h (make-eq-hashtable)
;;   (print a)
;;   (hashtable-set! h a 3)
;;   (print (hashtable-ref h a))
;;   (print (hashtable-keys h)))

;; (exit)

(define (hashtable-keys->list ht)
  (vector->list (hashtable-keys ht)))

(define (hashtable->alist ht)
  (hashtable-map cons ht))

;; (define t (make-eq-hashtable))
;; (hashtable-set! t 3 4)
;; (print (hashtable->alist t))
;; (exit)

(define (show-profile result)
  (let ([total (first result)]
        [calls-hash (second result)]
        [syms  (cddr result)]
        [table (make-eq-hashtable)])
    (print "time%        msec      calls   name")
    (let loop ([syms syms])
      (cond
       [(null? syms)
          '()]
       [else
        (aif (hashtable-ref table (car syms) #f)
             (hashtable-set! table (car syms) (+ it 1))
             (hashtable-set! table (car syms) 1))
        (loop (cdr syms))]))
    (print "before")
    (hashtable-for-each (lambda (key val)
                          (if (> val 100)
                              (format #t "~a ~a\n" key val)))
                        calls-hash)
    (print "after")
    (for-each
     (lambda (x)
       (aif (hashtable-ref calls-hash (first x) #f)
         (format #t " ~a   ~a ~a   ~a    ~a\n"
                 (lpad (third x) " " 3)
                 (lpad (* (second x) 10) " " 10)
                 (lpad it " " 10)
                 (rpad (source-info (first x)) " " 30))
         (format #t " ~a   ~a ~a   ~a    ~a\n"
                 (lpad (third x) " " 3)
                 (lpad (* (second x) 10) " " 10)
                 (lpad "?" " " 10)
                 (rpad (source-info (first x)) " " 30))
        ))
     (sort
      (hashtable-map
       (lambda (key value)
         (list key value (/ (* 100 value) total)))
       table)
      (lambda (x y) (> (third x) (third y)))))
    (let1 seen-syms (vector->list (hashtable-keys table))
      (for-each (lambda (x) (if (procedure? x) (print (source-info x))))  seen-syms)
;      (print (vector->list (hashtable-keys calls-hash)))
;      (print calls)
      (for-each
       (lambda (p)
         (foramt #t "p = ~a\n" p)
         (format #t "   0            0 ~a   ~a\n" (lpad (cdr p) " " 10) (rpad (car p) " " 30)))
       (print "before")
       (print (hashtable->alist calls-hash))
       (let1 filterd (filter (lambda (x) (print x) (not (memq (car x) seen-syms))) (hashtable->alist calls-hash))
         (print "filter done")
       ($take (sort  filterd(lambda (a b) (format #t "a=~a b=~a\n" a b) (> (cdr a) (cdr b)))) 30)))
    (format #t "  **   ~d          **   total\n" (lpad (* (* total 10)) " " 10)))))

(define a (lambda (x) 3))
(print (eqv? a a))

;; (print "hige")

;; (write (macroexpand '(do () ((not (pred (vector-ref v i) x))) (set! i (+ 1 i)))))
;; (write (macroexpand '(do () ((not (pred x (vector-ref v j)))) (set! j (- j 1)))))


(define (hoge i)
  (define (hige) (set! i i))
  (define (hage) (set! i i))
  (cond
   [(> i 100000)
    i]
   [else
    (hige)
    (hage)
    (hoge (+ i 1))
    ]))
  (format #t "hoge=~a \n" hoge)
(hoge 0)

;; (let ([v (list->vector '(1 3))]
;;       [x 1])
;;   (do () ((not ((lambda (a b) (> a b)) (vector-ref v 0) x))) (set! x (+ 1 0))))


;; (let ([v (list->vector '(1 3))]
;;       [i 0]
;;       [x 1])
;;   (letrec ((loop (lambda () (if (not ((lambda (a b) (> a b)) (vector-ref v i) x)) (begin #f) (begin (set! i (+ 1 i)) (loop)))))) (loop)))

;; (let ([v (list->vector '(1 3))]
;;       [x 1])
;;   (do () (((lambda (a b) (> a b)) (vector-ref v 0) x))))
;; (print 'done)

;; (write (macroexpand '(do () ((not (pred (vector-ref v i) x))) (set! i (+ 1 i)))))
;; (display "\n")
;; (hoge)



;; (define (sort2 obj pred)
;;   (vector->list (sort! (list->vector obj) pred)))


;; (sort2 '(4 1) (lambda (a b) (> a b)))
;  ($take (sort (filter (lambda (x) (not (memq (car x) seen-syms))) calls) (lambda (a b) (> (cdr a) (cdr b)))) 30))

;; (receive x (apply values '(1 2 3))
;;   (print x))

;; (receive x (values 1 2 3)
;;   (print x))

;; (receive (a b . c) (values 1 2 3 4)
;;   (format #t "a=~a b=~a c=~a \n" a b c))


;; (define (val) (values 'a 'b 'c 'd))

;; (receive (a b c d) (values 'a 'b 'c 'd) (print a))

;; (receive (a b c d) (val) (print a))




;; (receive (a b c d) (values 'a 'b 'c 'd)
;;   (receive (x y) (values 'x 'y)
;;    (format #t "a=~a b=~a c=~a d=~a x=~a y=~a\n" a b c d x y)
;;    (print a)))

;; (receive (a) 3
;;   (print "OK")
;;    (print a))

;; (define (hoge)
;;   (letrec ([hige (lambda () (values 1))])
;;     (receive (a) (hige)
;;       (format #t "~a\n" a))))

;; (hoge)


