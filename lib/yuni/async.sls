(library (yuni async)
         (export 
           ;; AUX keywords
           => 
           quote ;; reserved. Label a pipeline.
           quasiquote ;; for Label
           unquote

           ;; syntax
           seq)
         (import (yuni scheme))
         (begin
;;

;; In (yuni sync) library, every job-enqueue procedures should have
;; (^[arg0 arg1 ... callback] ...)
;; signature. (callback should be placed at last argument of procedure.)

;; WARNING: This library intentionally flatten program's nest.
;; For programer's viewpoint, this effectively breaks lexical scoping rules..

;; seq syntax. A toplevel => assumed as a job enqueue. 
;; callbacks are dynamically generated.
;; (seq 
;;   (=> write host0 bv len => (res) obj
;;   => write host1 obj len => (res) obj)
;;   => error-handler)
;;
;; (seq
;;   (=> [CALL] => [BIND/CHECK] => [CALL] => [BIND/CHECK] => ...)
;;   (normal-application)
;;   (=> ....))
;;
;; [BIND/CHECK] = [e0] ...
;; [e0] = bind / (check)

(define-syntax seq
  (syntax-rules (=>)
    ((_ clauses ... => error-handler)
     (seq => error-handler clauses ...))
    ((_ => error-handler clauses ...)
     (let ((err error-handler))
       (%seq/clauses err clauses ...)))
    ((_ otherwise ...)
     (seq => (lambda x (if #f #f)) otherwise ...))))

(define-syntax %seq/clauses
  (syntax-rules (=>)
    ((_ err (=> something ...) anything ...)
     (%seq/splitL () () (something ...) (err (%seq/clauses err anything ...))))
    ((_ err normal-form next-form ...)
     (begin
       normal-form
       (%seq/clauses err next-form ...)))
    ((_ err) (begin)))) 

;; Split (A => B => C ...) form
(define-syntax %seq/splitL
  (syntax-rules (=>)
    ((_ cur (acc ...) (item => something ...) n)
     (%seq/splitR cur (acc ... item) () (something ...) n))
    ((_ cur (acc ...) (s0 s1 ...) n)
     (%seq/splitL cur (acc ... s0) (s1 ...) n))))

(define-syntax %seq/splitR
  (syntax-rules (=>)
    ((_ (cur ...) L R () (err next))
     (%seq/emit err (cur ... (L R)) next))
    ((_ (cur ...) L (acc ...) (item => something ...) n)
     (%seq/splitL (cur ... (L (acc ... item))) () (something ...) n))
    ((_ cur L (acc ...) (something anything ...) n)
     (%seq/splitR cur L (acc ... something) (anything ...) n))))

(define-syntax %seq/emit
  (syntax-rules ()
    ((_ err () next)
     next)
    ((_ err ((q0 c0) qc1 ...) next)
     (%seq/gen err q0 c0 (%seq/emit err (qc1 ...) next)))))

(define-syntax %seq/gen
  (syntax-rules (quote unquote)
    ((_ err ((quote something) q0 ...) c0 next)
     (%seq/call 'err something (q0 ...) c0 next))
    ((_ err ((unquote something) q0 ...) c0 next)
     (%seq/call err something (q0 ...) c0 next))
    ((_ err (anything ...) c0 next)
     (%seq/call err #f (anything ...) c0 next))))

(define-syntax %seq/call
  (syntax-rules ()
    ((_ err id (call ...) recv next)
     (call ... (%seq/gencallback err id () () recv next)))))

;; Generate callback
(define-syntax %seq/gencallback
  (syntax-rules ()
    ((_ err id (names ...) (checks ...) () next)
     (lambda (names ...)
       (if (and checks ...) next (err id names ...))))
    ((_ err id (names ...) (checks ...) ((checker name) obj ...) next)
     (%seq/gencallback 
       err id 
       (names ... name) (checks ... (checker name)) (obj ...) next))
    ((_ err id (names ...) (checks ...) ((name) obj ...) next)
     (%seq/gencallback 
       err id 
       (names ... name) (checks ... name) (obj ...) next))
    ((_ err id (names ...) checks (name obj ...) next)
     (%seq/gencallback err id (names ... name) checks (obj ...) next))))

))
