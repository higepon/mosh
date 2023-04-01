(library (yuni util readerwriter)
         (export define-reader
                 define-writer
                 define-reader/binary
                 define-writer/binary)
         (import (rnrs))

(define (realize-input-port binary? sym x)
  (define (return-port x)
    (if binary?
      x
      (transcoded-port x (native-transcoder))))
  (cond
    ((port? x) x)
    ((string? x)
     (return-port
       (open-file-input-port x)))))

(define (realize-output-port binary? sym x)
  (define (return-port x)
    (if binary?
      x
      (transcoded-port x (native-transcoder))))
  (cond
    ((port? x) x)
    ((string? x)
     (return-port
       (open-file-output-port x)))))

(define-syntax define-reader
  (syntax-rules ()
    ((_ (name port?) code ...)
     (define (name port?)
       (call-with-port (realize-input-port #f 'name port?)
                       (lambda (port?) code ...))))))

(define-syntax define-writer
  (syntax-rules ()
    ((_ (name x port?) code ...)
     (define (name port?)
       (call-with-port (realize-output-port #f 'name port?)
                       (lambda (port?) code ...))))))

(define-syntax define-reader/binary
  (syntax-rules ()
    ((_ (name port?) code ...)
     (define (name port?)
       (call-with-port (realize-input-port #t 'name port?)
                       (lambda (port?) code ...))))))

(define-syntax define-writer/binary
  (syntax-rules ()
    ((_ (name x port?) code ...)
     (define (name port?)
       (call-with-port (realize-output-port #t 'name port?)
                       (lambda (port?) code ...))))))

)
