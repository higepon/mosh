(let ([captured '()])
  (dynamic-wind
      (lambda () (print "before"))
      (lambda ()
        (if (call/cc
             (lambda (cont)
               (set! captured cont)))
            '()
            (set! captured #f)))
      (lambda () (print "after")))
  (if captured
      (captured #f)
      (print "done")))

(let ([captured '()])
  (dynamic-wind
      (lambda () (print "before1"))
      (lambda ()
        (print "thunk1")
        (if (call/cc
             (lambda (cont)
               (set! captured cont)))
            '()
            (set! captured #f)))
      (lambda () (print "after1")))
  (dynamic-wind
      (lambda () (print "before2"))
      (lambda ()
        (print "thunk2")
        (if captured
            (captured #f)
            (print "done")))
      (lambda () (print "after2"))))
;; before1->thunk1->after1->before2->thunk2->after2->before1->after1->before2->thunk2->done->after2

(let ([captured '()])
  (dynamic-wind
      (lambda () (print "before1"))
      (lambda ()
        (print "thunk1")
        (dynamic-wind
            (lambda () (print "before1-1"))
            (lambda ()
              (print "thunk1-1")
              (if (call/cc
                   (lambda (cont)
                     (set! captured cont)))
                  '()
                  (set! captured #f)))
            (lambda () (print "after1-1"))))
      (lambda () (print "after1")))
  (dynamic-wind
      (lambda () (print "before2"))
      (lambda ()
        (print "thunk2")
        (if captured
            (captured #f)
            (print "done")))
      (lambda () (print "after2"))))
;; => before1->thunk1->before1-1->thunk1-1->after1-1->after1
;;    ->before2->thunk2->after2->before1->before1-1->after1-1
;;     ->after1->before2->thunk2->done->after2

