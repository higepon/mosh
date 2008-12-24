(import (rnrs)
        (srfi :26)
        (mosh string)
        )

;; (for-each 
;;  (lambda (x) (display x))
;;           '(1 2 32))

;;   (define-syntax guard2
;;     (syntax-rules (else)
;;       ((_ (var clause ... (else e1 e2 ...)) b1 b2 ...)
;;        ((call/cc
;;          (lambda (guard-k)
;;            (with-exception-handler
;;             (lambda (condition)
;;               ((call/cc
;;                 (lambda (handler-k)
;;                   (guard-k
;;                    (lambda ()
;;                      (let ((var condition))
;;                        (cond clause ... (else e1 e2 ...)))))))))
;;             (lambda ()
;;               (call-with-values
;;                (lambda () b1 b2 ...)
;;                (lambda args (guard-k (lambda () (apply values args)))))))))))
;;       ((_ (var clause ...) b1 b2 ...)
;;        ((call/cc
;;          (lambda (guard-k)
;;            (with-exception-handler
;;             (lambda (condition)
;;               ((call/cc
;;                 (lambda (handler-k)
;;                   (guard-k
;;                    (lambda ()
;;                      (let ((var condition))
;;                        (cond clause ...
;;                              (else (handler-k (lambda () (raise-continuable condition))))))))))))
;;             (lambda ()
;;               (call-with-values
;;                (lambda () b1 b2 ...)
;;                (lambda args (guard-k (lambda () (apply values args)))))))))))))

  (define-record-type expected-exception
    (fields))


(guard2 (c (((condition-predicate (record-type-descriptor &non-continuable)) c) (display "expected" )(make-expected-exception))
          (else (display c))
          )
       (with-exception-handler (lambda (x) 0) (lambda () (error #f "bad"))))

;; ((call/cc
;;                  (lambda (outerk)
;;                    (lambda ()
;;                      (with-exception-handler
;;                        (lambda (c)
;;                          (cond [((condition-predicate (record-type-descriptor &non-continuable)) c)
;;                                 (display "expected" )(make-expected-exception)]
;;                                [else (display "else")]))
;;                        (lambda () (with-exception-handler (lambda (x) 0) (lambda () (error #f "bad")))))))))

;; (define (hoge c)
;;                           (display "hoge\n")
;;                           (cond
;;                            [((condition-predicate (record-type-descriptor &non-continuable)) c) (make-expected-exception)]
;;                            (else (display "hoge") (display c))))

;; (define (hige x)
;; (display "\n0\n") 0)


;; (format #t "hoge=~a\nhige=~a\n" hoge hige)
;; (with-exception-handler hoge
;;        (lambda () (with-exception-handler hige (lambda () (error #f "bad")))))
