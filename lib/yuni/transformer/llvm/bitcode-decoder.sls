(library (yuni transformer llvm bitcode-decoder)
         (export bitcode-decode)
         (import (rnrs)
                 (for (yuni transformer llvm bitcode-dict) run expand))

(define-syntax decoder
  (syntax-rules ()
    ((_ (#f) input)
     input)
    ((_ (root) input)
     (if (pair? input)
       (let ((code (car input))
             (rest (cdr input)))
         ;(display (list 'DECODE code '=> (root code)))(newline)
         (cons (root code)
               rest))
       input))))

(define-syntax define-bitcode-blocks
  (syntax-rules ()
    ((_ name root (spec-sym spec-code ...) ...)
     (define (name block-id record)
       (let ((sym (root block-id)))
         (case sym
           ((spec-sym) (decoder (spec-code ...) record))
           ...
           (else record)))))))

(define-bitcode-blocks bitcode-decode
   llvm-ir-blockid
   (MODULE_BLOCK record-MODULE_BLOCK)
   (PARAMATTR_BLOCK record-PARAMATTR_BLOCK)
   (TYPE_BLOCK record-TYPE_BLOCK)
   (CONSTANTS_BLOCK record-CONSTANTS_BLOCK)
   (TYPE_SYMTAB_BLOCK record-TYPE_SYMTAB_BLOCK)
   (VALUE_SYMTAB_BLOCK record-VALUE_SYMTAB_BLOCK)
   (METADATA_BLOCK record-METADATA_BLOCK)
   (METADATA_ATTACHMENT #f))

)
