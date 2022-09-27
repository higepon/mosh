;; R7RS -> R6RS Library converter.
;; This mainly targets psyntax based R6RS.

(library (mosh internal library-converter)
         (export rewrite-define-library
                 rewrite-export rewrite-body parse-define-library)
         (import (rnrs)
                 (match))

;; The main API.
(define (rewrite-define-library dirname exp)
    (let-values (((name export* import* body*) (parse-define-library exp)))
        `(library ,name (export ,@(rewrite-export export*))
                        (import ,@import*)
            ,@(rewrite-body dirname body*))))

(define (parse-define-library exp)
  (match exp
    [('define-library (name* ...)
                      ('export export* ...)
                      ('import import* ...)
       body* ...)
        (values name* export* import* body*)]
    [else (values #f #f)]))

(define (rewrite-export exp)
   (match exp
     [(('rename from to) other ...)
        `((rename (,from ,to)) ,@(rewrite-export other))]
     [(one other ...)
        `(,one ,@(rewrite-export other))]
     [() '()]))

(define (rewrite-body dirname exp)
    (flatten
        (map 
            (lambda (e)
            (match e
                [('include path* ...)
                (map (lambda (path) `(include ,(string-append dirname "/" path))) path*)]
                [else `(,e)])) exp)))


;; Utilities.
(define (fold1 kons knil lst)
    (if (null? lst)
        knil
       (fold1 kons (kons (car lst) knil) (cdr lst))))
  
  
  (define (flatten lists)
      (fold1 (lambda (right left)
                      (append left right))
                  '() lists))        
)