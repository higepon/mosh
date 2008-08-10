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

(display
(call/cc
 (lambda (cont)
   (with-exception-handler
     (lambda (exception)
       (cont 3))
     (lambda () (raise #f))))))
