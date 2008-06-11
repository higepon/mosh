

(print "hige")

(write (macroexpand '(do () ((not (pred (vector-ref v i) x))) (set! i (+ 1 i)))))
(write (macroexpand '(do () ((not (pred x (vector-ref v j)))) (set! j (- j 1)))))

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


