(library (yuni util tables)
         (export 
           =>
           define-static-table)
         (import 
           (for (yuni util tables internal) run expand)
           (for (rnrs) run expand))

;; table syntax
;;
;; (define-static-table NAME
;;   CONVERTER-SPEC*
;;   #(FIELD-NAMES*)
;;   (FIELDS*)*)

(define-syntax emit-converter-syntax
  (lambda (x)
    (syntax-case x (=>)
      ((_ (name lookup => return) field-names orig-data)
       #'(define-syntax name
           (lambda (y)
             (define my-field-data 'orig-data)
             (define-converter1 (conv lookup => return equal?) field-names my-field-data)
             (syntax-case y ()
               ((_ input)
                (datum->syntax #'y
                               (list 'quote
                                     (conv (syntax->datum #'input))))))))))))

(define-syntax emit-converter1
  (syntax-rules (=>)
    ((_ #(name lookup => return) field-names field-data orig-data)
     (emit-converter-syntax (name lookup => return) field-names orig-data))
    ((_ (name lookup => return) field-names field-data orig-data)
     (emit-converter1 (name lookup => return equal?) field-names field-data orig-data))
    ((_ (name lookup => return pred) field-names field-data orig-data)
     (define-converter1 (name lookup => return pred) field-names field-data))))

(define-syntax emit-converters
  (syntax-rules ()
    ((_ (convspec0) field-names field-data orig-data)
     (emit-converter1 convspec0 field-names field-data orig-data))
    ((_ (convspec0 convspec1 ...) field-names field-data orig-data)
     (begin
       (emit-converter1 convspec0 field-names field-data orig-data)
       (emit-converters (convspec1 ...) field-names field-data orig-data)))))

(define-syntax define-static-table
  (syntax-rules ()
    ((_ name #(field-name ...) fields ...)
     (define-static-table name () #(field-name ...) fields ...))
    ((_ name (convspec ...) #(field-name ...) (fields ...) ...)
     (begin
       (define field-data '(#(fields ...) ...))
       (emit-converters (convspec ...) #(field-name ...) 
                        field-data ;; for run-time lookup
                        (#(fields ...) ...) ;; for expand-time lookup
                        )))))

)
