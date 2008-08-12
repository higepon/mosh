(receive x (values)
  (display x))

(exit)


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




;; (call-with-values (lambda ()
;;                     ((call/cc
;;                       (lambda (outerk)
;;                         (lambda ()
;;                           (with-exception-handler
;;                            (lambda (c)
;;                              ((call/cc
;;                                (lambda (gen)
;;                                  (outerk
;;                                   (lambda () (if #t (begin #t) (gen (lambda () (raise c))))))))))
;;                            (lambda () #f (raise #f))))))))
;;   (lambda x
;;     x))

;; (call-with-values (lambda ()
;;                     ((call/cc
;;                       (lambda (outerk)
;;                         (lambda ()
;;                           (with-exception-handler
;;                            (lambda (c)
;;                              ((call/cc
;;                                (lambda (gen)
;;                                  (outerk
;;                                   (lambda () #t))))))
;;                            (lambda () #f (raise #f))))))))
;;   (lambda x
;;     x))

;; (call-with-values (lambda ()
;;                     ((call/cc
;;                       (lambda (outerk)
;;                         (lambda ()
;;                           (with-exception-handler
;;                            (lambda (c)
;;                              ((call/cc
;;                                (lambda (gen)
;;                                  (outerk
;;                                   (lambda () #t))))))
;;                            (lambda () (raise #f))))))))
;;   (lambda x x))

;; (call-with-values (lambda ()
;;                     ((call/cc
;;                       (lambda (outerk)
;;                         (lambda ()
;;                           (with-exception-handler
;;                            (lambda (c)
;;                              (
;;                                  (outerk
;;                                   (lambda () #t))))
;;                            (lambda () (raise #f))))))))
;;   (lambda x x))

;; (call-with-values
;;     (lambda ()
;;       ((call/cc
;;         (lambda (outerk)
;;           (lambda ()
;;             (with-exception-handler
;;              (lambda (c)
;;                (
;;                 (outerk
;;                  (lambda () #t))))
;;              (lambda () (raise #f)))
;;             )))))
;;   (lambda x x))

;; (call-with-values
;;     (lambda ()
;;       ((call/cc
;;         (lambda (outerk)
;;           (lambda ()
;;             (with-exception-handler
;;              (lambda (c)
;;                (
;;                 (outerk
;;                  (lambda () #t))))
;;              (lambda () (raise #f)))
;;             )))))
;;   (lambda x x))

;; (display
;;  ((lambda ()
;;     ((call/cc
;;       (lambda (outerk)
;;         (lambda ()
;;           (with-exception-handler
;;            (lambda (c)
;;              (
;;               (outerk
;;                (lambda () #t))))
;;            (lambda () (raise #f)))
;;           )))))))

;; (display
;;  ((call/cc
;;    (lambda (outerk)
;;      (lambda ()
;;        (with-exception-handler
;;         (lambda (c)
;;           (
;;            (outerk
;;             (lambda () #t))))
;;         (lambda () (raise #f)))
;;           )))))

;; (display
;;  ((call/cc
;;    (lambda (outerk)
;;      (lambda ()
;;        (with-exception-handler
;;         (lambda (c)
;;           (
;;            (outerk
;;             (lambda () #t))))
;;         (lambda () (raise #f)))
;;           )))))

;; (display
;;  ((call/cc
;;    (lambda (outerk)
;;      (lambda ()
;;        (with-exception-handler
;;         (lambda (c)
;;           (
;;            (outerk
;;             (lambda () #t))))
;;         (lambda () (raise #f)))
;;           )))))

;; (display
;; (call/cc
;;  (lambda (cont)
;;    (with-exception-handler
;;      (lambda (exception)
;;        (cont 3))
;;      (lambda () (raise #f))))))

;; (display
;; (call/cc
;;  (lambda (cont)
;;    (with-exception-handler
;;      (lambda (exception)
;;        (cont 3))
;;      (lambda () (raise #f))))))
