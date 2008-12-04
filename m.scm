;; ((lambda (G0@dummy) (begin (set! G0@dummy (begin (letrec ((G1@final-handler (lambda (G5@args1) ((lambda (G7@procedure) (set! G1@final-handler '())) (display G5@args1))))) (lambda (G3@args) (G1@final-handler G3@args))) (void))) (void))) '#f)

;(letrec ((G1@final-handler (lambda (G5@args1) ((lambda (G7@procedure) (set! G1@final-handler '())) (display G5@args1))))) (lambda (G3@args) (G1@final-handler G3@args)))
(display "OK\n")
(letrec ((loop (lambda (x)
                 ((lambda (y) (set! loop '())) (display x)))))
  (lambda (z) (loop z)))
