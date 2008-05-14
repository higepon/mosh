;; The function point. This function acts as a class.
(define (point x y)
 (let ((x x) 
       (y y)
      )

   ;;; Accessors

   ;; The method that accesses x
   (define (getx) x)

   ;; The method that accesses y
   (define (gety) y)

   ;; Add p to this point
   (define (add p) 

     ;;; xxx.

     ;; The identify function. 1.
     (define (id x) 
 
       ;; yes
       (define (iii) 1)
         
        x)

     ;; The identify function. 2.
     (define (idd x) 
        x)

     ; The identify function. 2.
     (define (iddd x) 
        x)

     ;; A strange function
     (define (strange x)
        5)

    (point 
     (+ x (send 'getx p))
     (+ y (send 'gety p))))

   ;; Return the type of this point
   (define (type-of) 'point 
   )
     
   (define (self message)
     (cond ((eqv? message 'getx) getx)
           ((eqv? message 'gety) gety)
           ((eqv? message 'add)  add)
           ((eqv? message 'type-of) type-of)
	   (else (error "Undefined message" message))))
     
   self))

;; The factorial functions. Also known as n!
;; .parameter n An integer
;; .pre-condition n >= 0
;; .returns n * (n-1) * ... * 1
(define (fac n)
 (if (= n 0) 1 (* n (fac (- n 1)))))