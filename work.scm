(define a 3)
(print 3)
;; (define find-table (make-eq-hashtable))





;; (take '(a b c d e)  2)
;; (drop '(a b c d e)  2)

;; (take '(1 2 3 . d) 2)
;; (drop '(1 2 3 . d) 2)
;; (take '(1 2 3 . d) 3)
;; (drop '(1 2 3 . d) 3)
;; (take-right '(a b c d e) 2)
;; (drop-right '(a b c d e) 2)
;; (take-right '(1 2 3 . d) 2)
;; (drop-right '(1 2 3 . d) 2)
;; (take-right '(1 2 3 . d) 0)
;; (drop-right '(1 2 3 . d) 0)


;; (exit)
;; (define a '(1 2))
;; (print (append a a))

;; (print (command-line))
;; ;; (define (hoge i)
;; ;;   (define (hige) (set! i i))
;; ;;   (define (hage) (set! i i))
;; ;;   (cond
;; ;;    [(> i 100000)
;; ;;     i]
;; ;;    [else
;; ;;     (hige)
;; ;;     (hage)
;; ;;     (hoge (+ i 1))
;; ;;     ]))
;; ;; (hoge 0)

;; (print 'work)
;; ;; (define (hage)
;; ;;   (define (collect-labels)
;; ;;     (let* ([len (vector-length '#(1))]
;; ;;            [ret (make-vector len 'NOP)])
;; ;;       (let loop ([i 0]
;; ;;                  [j 0])
;; ;;         (loop i))))
;; ;;   (collect-labels))

;; ;; (hage)


;; ;; ;; (apply print '(1))
;; ;; ;; (apply print '(2))
;; ;; ;; (apply print '(3))
;; ;; ;; (apply (lambda (a b) (print a) (print b)) '(3 1235))

;; ;; ;; (apply (lambda (a b c) (print (+ a b c))) 1 2 '(3))

;; ;;                                         ;(apply (lambda () 3) '(4))




;; ;; (aif 3
;; ;;      (print it)
;; ;;      4)
;; ;; ;((lambda () 3) 4)
;; ;; ;(exit)
;; ;; ;(define a 3)
;; ;; ;; (let1 b a
;; ;; ;;   (print b))

;; ;; ;; (letrec ([b a])
;; ;; ;;   (print b))

;; ;; ;(exit)
;; ;; ;; (define (http-get url)
;; ;; ;;   (call-process (format "wget ~a -O- -q" url)))

;; ;; ;; ;(print (wget "http://livedoor.com"))
;; ;; ;; ;(print 'done)
;; ;; ;; ;(write (call-process "hage"))

;; ;; ;; (suma 4)
;; ;; ;; (exit)

;; ;; ;; (map print '(1 2 3 4))
;; ;; ;; (exit)
;; ;; ;; (print (source-info (lambda () x)))
;; ;; ;; (define (a) 3)
;; ;; ;(a 3)
;; ;; ;((lambda () x) 33)

;; ;; ;(print (compile '(source-info (lambda () x))))
;; ;; ;; (define (hoge)
;; ;; ;;   (define (int)
;; ;; ;;     3)
;; ;; ;;   (print (source-info int))
;; ;; ;;   (print (source-info hoge)))
;; ;; ;; (hoge)
;; ;; ;(exit)

;; ;; ;; (print (cdr (lambda () x)))
;; ;; ;; (exit)

;; ;; ;(print (vector->list '#(a b c)))

;; ;; ;; (define a (lambda () 3))

;; ;; ;; (let1 h (make-eq-hashtable)
;; ;; ;;   (print a)
;; ;; ;;   (hashtable-set! h a 3)
;; ;; ;;   (print (hashtable-ref h a))
;; ;; ;;   (print (hashtable-keys h)))

;; ;; ;; (exit)


;; ;; (define a (lambda (x) 3))
;; ;; (print (eqv? a a))

;; ;; ;; (print "hige")

;; ;; ;; (write (macroexpand '(do () ((not (pred (vector-ref v i) x))) (set! i (+ 1 i)))))
;; ;; ;; (write (macroexpand '(do () ((not (pred x (vector-ref v j)))) (set! j (- j 1)))))


;; ;; (define (hoge i)
;; ;;   (define (hige) (set! i i))
;; ;;   (define (hage) (set! i i))
;; ;;   (cond
;; ;;    [(> i 100000)
;; ;;     i]
;; ;;    [else
;; ;;     (hige)
;; ;;     (hage)
;; ;;     (hoge (+ i 1))
;; ;;     ]))
;; ;; (hoge 0)

;; ;; ;; (let ([v (list->vector '(1 3))]
;; ;; ;;       [x 1])
;; ;; ;;   (do () ((not ((lambda (a b) (> a b)) (vector-ref v 0) x))) (set! x (+ 1 0))))


;; ;; ;; (let ([v (list->vector '(1 3))]
;; ;; ;;       [i 0]
;; ;; ;;       [x 1])
;; ;; ;;   (letrec ((loop (lambda () (if (not ((lambda (a b) (> a b)) (vector-ref v i) x)) (begin #f) (begin (set! i (+ 1 i)) (loop)))))) (loop)))

;; ;; ;; (let ([v (list->vector '(1 3))]
;; ;; ;;       [x 1])
;; ;; ;;   (do () (((lambda (a b) (> a b)) (vector-ref v 0) x))))
;; ;; ;; (print 'done)

;; ;; ;; (write (macroexpand '(do () ((not (pred (vector-ref v i) x))) (set! i (+ 1 i)))))
;; ;; ;; (display "\n")
;; ;; ;; (hoge)



;; ;; ;; (define (sort2 obj pred)
;; ;; ;;   (vector->list (sort! (list->vector obj) pred)))


;; ;; ;; (sort2 '(4 1) (lambda (a b) (> a b)))
;; ;; ;  ($take (sort (filter (lambda (x) (not (memq (car x) seen-syms))) calls) (lambda (a b) (> (cdr a) (cdr b)))) 30))

;; ;; ;; (receive x (apply values '(1 2 3))
;; ;; ;;   (print x))

;; ;; ;; (receive x (values 1 2 3)
;; ;; ;;   (print x))

;; ;; ;; (receive (a b . c) (values 1 2 3 4)
;; ;; ;;   (format #t "a=~a b=~a c=~a \n" a b c))


;; ;; ;; (define (val) (values 'a 'b 'c 'd))

;; ;; ;; (receive (a b c d) (values 'a 'b 'c 'd) (print a))

;; ;; ;; (receive (a b c d) (val) (print a))




;; ;; ;; (receive (a b c d) (values 'a 'b 'c 'd)
;; ;; ;;   (receive (x y) (values 'x 'y)
;; ;; ;;    (format #t "a=~a b=~a c=~a d=~a x=~a y=~a\n" a b c d x y)
;; ;; ;;    (print a)))

;; ;; ;; (receive (a) 3
;; ;; ;;   (print "OK")
;; ;; ;;    (print a))

;; ;; ;; (define (hoge)
;; ;; ;;   (letrec ([hige (lambda () (values 1))])
;; ;; ;;     (receive (a) (hige)
;; ;; ;;       (format #t "~a\n" a))))

;; ;; ;; (hoge)
