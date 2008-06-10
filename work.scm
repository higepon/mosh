(define (sort2 obj pred)
  (vector->list (sort! (list->vector obj) pred)))


(sort2 '(4 1) (lambda (a b) (> a b)))
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


