;;; SUM -- Compute sum of integers from 0 to 10000
;;;   Sums the integers from 0 to 10000, 10000 iterations
;;    from http://www.ccs.neu.edu/home/will/Twobit/benchmarksAbout.html

(define (run n)
  (let loop ((i n) (sum 0))
    (if (< i 0)
      sum
      (loop (- i 1) (+ i sum)))))

(do ((i 0 (+ i 1)))
    ((= i 100) '())
  (unless (= 50005000 (run 10000))
    (error "sum failed")))
