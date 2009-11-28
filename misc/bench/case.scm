(define (loop i l)
  (case 6
    ((1 2 3 4 5) #f)
    ((6)
     (if (< i l)
         (loop (+ 1 i) l)
         l))))

(unless (= 500000 (loop 0 500000))
  (error "case failed"))
