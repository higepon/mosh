;; OK
;; (letrec ([a (lambda () b)]
;;          [b (lambda () 3)])
;;   (display ((a)))
;;   (newline))
;; (display "***********\n")
;; ;; Error

(guard
 (c [#t #f])
 (letrec ([a 3]
          [b a])
   (display b)
   (newline))
 )
