(define (sort2 obj pred)
  (vector->list (sort2! (list->vector obj) pred)))

(define (sort2! v pred)
    (define (internal-sort l r)
      (let ((i l) (j r) (x (vector-ref v (/ (- (+ l r) 1) 2))))
    (let loop ()
         (do () ((not (pred (vector-ref v i) x))) (set! i (+ 1 i)))
         (do () ((not (pred x (vector-ref v j)))) (set! j (- j 1)))
         (if (<= i j)
         (let ((temp (vector-ref v i)))
           (vector-set! v i (vector-ref v j))
           (vector-set! v j temp)
           (set! i (+ 1 i))
           (set! j (- j 1))))
         (if (<= i j)
         (loop)))
    (if (< l j)
        (internal-sort l j))
    (if (< i r)
        (internal-sort i r))))
  (let ((len (vector-length v)))

    (if (> len 1)
    (internal-sort 0 (- len 1)))
    v))
; From elk end

(sort '(4 1) (lambda (a b) (> a b)))
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


