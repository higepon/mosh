; http://srfi.schemers.org/srfi-41/srfi-41.html
(library (srfi :41 primitive)

  (export stream-null stream-cons stream? stream-null? stream-pair?
          stream-car stream-cdr stream-lambda)

  (import
   (only (rnrs) define-syntax define-record-type fields mutable syntax-rules define
                immutable cons quote let case cdr let* if not eqv? car begin lambda
                and cond error else ...)
   (only (rnrs mutable-pairs) set-car! set-cdr!))

  (define-record-type (stream-type make-stream stream?)
    (fields (mutable box stream-promise stream-promise!)))

  (define-syntax stream-lazy
    (syntax-rules ()
      ((stream-lazy expr)
        (make-stream
          (cons 'lazy (lambda () expr))))))

  (define (stream-eager expr)
    (make-stream
      (cons 'eager expr)))

  (define-syntax stream-delay
    (syntax-rules ()
      ((stream-delay expr)
        (stream-lazy (stream-eager expr)))))

  (define (stream-force promise)
    (let ((content (stream-promise promise)))
      (case (car content)
        ((eager) (cdr content))
        ((lazy)  (let* ((promise* ((cdr content)))
                        (content  (stream-promise promise)))
                   (if (not (eqv? (car content) 'eager))
                       (begin (set-car! content (car (stream-promise promise*)))
                              (set-cdr! content (cdr (stream-promise promise*)))
                              (stream-promise! promise* content)))
                   (stream-force promise))))))

  (define stream-null (stream-delay (cons 'stream 'null)))

  (define-record-type (stream-pare-type make-stream-pare stream-pare?)
    (fields (immutable kar stream-kar) (immutable kdr stream-kdr)))

  (define (stream-pair? obj)
    (and (stream? obj) (stream-pare? (stream-force obj))))

  (define (stream-null? obj)
    (and (stream? obj)
         (eqv? (stream-force obj)
               (stream-force stream-null))))

  (define-syntax stream-cons
    (syntax-rules ()
      ((stream-cons obj strm)
        (stream-eager (make-stream-pare (stream-delay obj) (stream-lazy strm))))))

  (define (stream-car strm)
    (cond ((not (stream? strm)) (error 'stream-car "non-stream"))
          ((stream-null? strm) (error 'stream-car "null stream"))
          (else (stream-force (stream-kar (stream-force strm))))))

  (define (stream-cdr strm)
    (cond ((not (stream? strm)) (error 'stream-cdr "non-stream"))
          ((stream-null? strm) (error 'stream-cdr "null stream"))
          (else (stream-kdr (stream-force strm)))))

  (define-syntax stream-lambda
    (syntax-rules ()
      ((stream-lambda formals body0 body1 ...)
        (lambda formals (stream-lazy (let () body0 body1 ...)))))))
