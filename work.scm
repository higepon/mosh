;; OK
(letrec ([a (lambda () b)]
         [b (lambda () 3)])
  (display ((a)))
  (newline))
(display "***********\n")
;; ;; Error
(letrec ([a 3]
         [b a])
  (display b)
  (newline))
