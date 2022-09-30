(define-library (srfi 151) 
(import (scheme base))
(import (scheme case-lambda))

(export bitwise-not bitwise-and bitwise-ior bitwise-xor bitwise-eqv
        bitwise-nand bitwise-nor bitwise-andc1 bitwise-andc2
        bitwise-orc1 bitwise-orc2)
(export arithmetic-shift bit-count integer-length bitwise-if 
        bit-set? copy-bit bit-swap any-bit-set? every-bit-set?  first-set-bit)
(export bit-field bit-field-any? bit-field-every?  bit-field-clear bit-field-set
        bit-field-replace  bit-field-replace-same
        bit-field-rotate bit-field-reverse)
(export bits->list list->bits bits->vector vector->bits bits
        bitwise-fold bitwise-for-each bitwise-unfold make-bitwise-generator)

;; Provide core functions
(cond-expand
  (chibi
    (include-shared "srfi/142/bit")
    (begin
      (define (bitwise-not i) (- -1 i))
      
      (define (make-nary proc2 default)
        (lambda args
          (if (null? args)
              default
              (let lp ((i (car args)) (ls (cdr args)))
                (if (null? ls)
                    i
                    (lp (proc2 i (car ls)) (cdr ls)))))))
      
      (define bitwise-and  (make-nary bit-and  -1))
      (define bitwise-ior  (make-nary bit-ior   0))
      (define bitwise-xor  (make-nary bit-xor   0))))

  ((library (rnrs arithmetic bitwise))
    (import (only (rnrs arithmetic bitwise)
                  bitwise-not bitwise-and bitwise-ior bitwise-xor
                  bitwise-bit-count))
    (import (rename (only (rnrs arithmetic bitwise)
                          bitwise-arithmetic-shift bitwise-length)
                    (bitwise-arithmetic-shift arithmetic-shift)
                    (bitwise-length integer-length)))
    (begin
      (define (bit-count i) ; Negative case different to R6RS bitwise-bit-count
         (if (>= i 0)
           (bitwise-bit-count i)
           (bitwise-bit-count (bitwise-not i))))))

  (gauche
    (import (only (gauche base)
                  integer-length))
    (import (rename (only (gauche base)
                          lognot logand logior logxor ash)
                    (lognot bitwise-not)
                    (logand bitwise-and)
                    (logior bitwise-ior)
                    (logxor bitwise-xor)
                    (ash arithmetic-shift))))
  (else (include "bitwise-core.scm")))

;; Stable part of the implementation
(include "bitwise-33.scm")
(include "bitwise-60.scm")
(include "bitwise-other.scm")
)