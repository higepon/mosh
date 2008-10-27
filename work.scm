(define (a x) x)
;((lambda (o p q r) (a 3)) 2 3 4 5)
;; (let ([o 2]
;;       [p 3]
;;       [q 4]
;;       [r 5])
;;   (a 3))

;; ((lambda (o p q r)
;;    (let ([x 1])
;;      (a 3)))
;;  2 3 4 5)

;; this causes stack over flow!
;; (define kons cons)
(define (my-map1 f l)
  (if (null? l)
      l
      (cons (f (car l)) (my-map1 f (cdr l)))))

;; ;#(CLOSURE 42 2 #f 0 10 ((work.scm 15) my-map1 f l) REFER_LOCAL 1 NULL_P TEST 5 REFER_LOCAL 1 LOCAL_JMP 26 FRAME 9 REFER_LOCAL 1 CAR PUSH REFER_LOCAL 0 CALL 1 PUSH FRAME 12 REFER_LOCAL 0 PUSH REFER_LOCAL 1 CDR PUSH REFER_GLOBAL top-level:$:my-map1 CALL 2 CONS RETURN 2 DEFINE_GLOBAL top-level:$:my-map1 HALT NOP NOP NOP NOP NOP)

((lambda (x y z)
   (my-map1 (lambda (x) x) (vector->list (make-vector 10000 3)))
  (display (+ x y z))) 1 100 2)

;((lambda (o p q r) (cons 3 2)) 2 3 4 5)

