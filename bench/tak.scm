;;; TAK -- A vanilla version of the TAKeuchi function.
;;;   A triply recursive integer function related to the Takeuchi function
;;    from http://www.ccs.neu.edu/home/will/Twobit/benchmarksAbout.html

(define (tak x y z)
  (if (not (< y x))
      z
      (tak (tak (- x 1) y z)
           (tak (- y 1) z x)
           (tak (- z 1) x y))))

(do ((i 0 (+ i 1)))
    ((= i 100) '())
  (unless (= 7 (tak 18 12 6))
    (error "tak failed")))

