;; OK
;; (letrec ([a (lambda () b)]
;;          [b (lambda () 3)])
;;   (display ((a)))
;;   (newline))
;; (display "***********\n")
;; ;; Error

;; (guard
;;  (c [#t #f])
;;  (letrec ([a 3]
;;           [b a])
;;    (display b)
;;    (newline))
;;  )

  (call-with-current-continuation
   (lambda (k)
     (dynamic-wind
         (lambda ()
           (k))
         (lambda () #f)
         (lambda () #f))))

(let ((n 0))
            (call-with-current-continuation
             (lambda (k)
               (dynamic-wind
                   values
                   (lambda ()
                     (dynamic-wind
                         values
                         (lambda ()
                           (set! n (+ n 1))
                           (k))
                         (lambda ()
                           (set! n (+ n 2))
                           (k))))
                   (lambda ()
                     (set! n (+ n 4))))))
            n) 
;;           7
