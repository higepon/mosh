(library (yuni scheme refimpl arithmetic impl fixnum)
         (export least-fixnum
                 greatest-fixnum
                 core->fixnum
                 fixnum->core
                 fixnum*
                 fixnum-zero?
                 fixnum<?
                 fixnum-quotient+remainder
                 fixnum+
                 fixnum-negative?
                 fixnum-
                 fixnum-positive?
                 fixnum-min
                 fixnum=?
                 fixnum-remainder
                 fixnum-quotient
                 fixnum-arithmetic-shift-left
                 fixnum>=?
                 fixnum<=?
                 fixnum-even?
                 fixnum-odd?
                 fixnum-first-bit-set
                 fixnum-length
                 fixnum-bit-count
                 fixnum?
                 fixnum-ior
                 fixnum-arithmetic-shift
                 fixnum-and
                 fixnum>?
                 fixnum-not
                 fixnum-xor
                 fx
                 fixnum-max
                 fxdiv0
                 fxmod0
                 fixnum-bit-set?
                 fixnum-arithmetic-shift-right
                 fx=?
                 fx>?
                 fx<?
                 fx>=?
                 fx<=?
                 fxzero?
                 fxpositive?
                 fxnegative?
                 fxodd?
                 fxeven?
                 fxmax
                 fxmin
                 fx+
                 fx-
                 fx*
                 (rename 
                   (fixnum+/carry fx+/carry)
                   (fixnum-/carry fx-/carry)
                   (fixnum*/carry fx*/carry)
                   (fxdiv0+mod0 fxdiv0-and-mod0)
                   (fxdiv+mod fxdiv-and-mod))
                 fxdiv
                 fxmod
                 fxnot
                 fxand
                 fxior
                 fxxor
                 fxif
                 fxbit-count
                 fxlength
                 fxfirst-bit-set
                 fxbit-set?
                 fxcopy-bit
                 fxbit-field
                 fxcopy-bit-field
                 fxrotate-bit-field
                 fxreverse-bit-field
                 fxarithmetic-shift
                 fxarithmetic-shift-left
                 fxarithmetic-shift-right
                 )
         (import (yuni scheme refimpl arithmetic backend)
                 (yuni scheme refimpl arithmetic impl nary)
                 (yuni scheme refimpl arithmetic impl custom)
                 (yuni scheme refimpl arithmetic impl bitwise))

; This file is part of the reference implementation of the R6RS Arithmetic SRFI.
; See file COPYING.

; Fixnums in terms of R5RS

; Some of this code may not actually depend upon a two's complement
; range, but all of it depends upon the fixnum range being represented
; by exact integers in the underlying implementation.
;
; It should not depend upon any exact numbers that are outside
; the fixnum range.
;
; Hence all of this code must be written carefully to avoid
; intermediate results outside the fixnum range.
;
; Consider, for example, the definitions of *high* and *low*
; and the definitions of fixnum+/2 and fixnum*/2.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; The fixnum range is a half-open two's complement range.
;
; *width* is defined in custom.scm
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *high*
  (let* ((i (core:expt 2 (core:- *width* 2)))
         (i-1 (core:- i 1)))
    (core:+ i i-1)))

(define *low*
  (core:- (core:- *high*) 1))

(define *half-width* (core:quotient *width* 2))

(define *half-modulus* (core:expt 2 *half-width*))

(define *half-high* (core:- (core:quotient *half-modulus* 2) 1))

(define *half-low* (core:- (core:quotient *half-modulus* 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Defining a fixnum as a record helps to catch certain kinds of
; errors but interferes with debugging.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ported to (yuni core):

(define* :fixnum
  (representative))

(define (really-make-fixnum-record x)
  (make :fixnum
        (representative x)))

(define (fixnum-record? x)
  (is-a? x :fixnum))

(define (fixnum-record-rep x)
  (let-with x (representative)
    representative))



#|
; SRFI 9
;
; This defines :fixnum to be a record type with
;     really-make-fixnum-record -- a unary constructor of instances
;     fixnum-record?            -- a predicate true only of instances
;     fixnum-record-rep         -- an accessor for the representative field

(define-record-type :fixnum
  (really-make-fixnum-record representative)
  fixnum-record?
  (representative fixnum-record-rep))

; Scheme 48 extension; comment out if not available
; (For portability, the r5rs.sch file defines
; define-record-discloser as a stub.)

(define-record-discloser :fixnum
  (lambda (r)
    (list 'fixnum (fixnum-record-rep r))))
|#

; Here are the definitions that matter.

(define really-make-fixnum
  (if *fixnums-are-records*
      really-make-fixnum-record
      (lambda (x) x)))

(define fixnum?
  (if *fixnums-are-records*
      fixnum-record?
      (lambda (x)
        (and (core:number? x)
             (core:exact? x)
             (core:integer? x)
             (core:<= *low* x *high*)))))

(define fixnum-rep
  (if *fixnums-are-records*
      fixnum-record-rep
      (lambda (x)
        (if (fixnum? x) x (error "bad argument to fixnum-rep" x)))))

(define fixnum->core fixnum-rep)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Basic subprimitives, from which the basic primitives are defined.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-fixnum n)
  (if (core:<= *low* n *high*)
      (really-make-fixnum n)
      (error "argument to make-fixnum is out of range" n)))

(define core->fixnum make-fixnum)

; for playing around
(define fx make-fixnum)

; See "Cleaning up the Tower"
; This version of div has been cleaned up to match the
; semantics of fixnum-div in the current draft of SRFI 77.
; Note that it returns *low* when *low* is divided by -1.

(define (core-fixnum-div x y)
  (if (not (and (core:<= *low* x *high*)
                (core:<= *low* y *high*)))
      (error "non-fixnum arguments to core-fixnum-div" x y)
      (cond ((core:positive? y)
             (if (core:>= x 0)
                 (core:quotient x y)
                 (let ((z (core:quotient x y)))
                   (if (core:= x (core:* y z))
                       z
                       (core:- z 1)))))
            ((core:zero? y)
             (error "zero divisor (fixnum-div)" x y))
            ((core:= y -1)
             ; Can't negate *low*, which is its own additive inverse.
             (if (core:= x *low*)
                 x
                 (core:- x)))
            ((core:= y *low*)
             ; Can't negate *low*, which is its own additive inverse.
             (if (core:= x *low*)
                 1
                 0))
            (else
             (core:- (core-fixnum-div x (core:- y)))))))

(define (core-fixnum-mod x y)
  (if (core:= y -1)
      0
      (core:- x (core:* (core-fixnum-div x y) y))))

; Given two fixnums and their fixnum sum, returns the carry.

(define (fixnum-carry x y x+y)
  (if (fixnum-negative? x)
      (if (fixnum-negative? y)
          ; Both x and y are negative, so x+y should be negative.
          (if (fixnum-negative? x+y)
              (make-fixnum 0)
              (make-fixnum -1))
          (make-fixnum 0))
      (if (fixnum-negative? y)
          (make-fixnum 0)
          ; Both x and y are non-negative, so x+y should be non-negative.
          (if (fixnum-negative? x+y)
              (make-fixnum 1)
              (make-fixnum 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Basic primitives, from which the rest are defined.
;
; These basic primitives are more easily and more appropriately
; written in assembly language than in Scheme.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (fixnum-div+mod x y)
  (let* ((a (fixnum-rep x))
         (b (fixnum-rep y))
         (d (core-fixnum-div a b))
	 (m (if (core:= b -1)
                0
                (core:- a (core:* d b)))))
    (values (make-fixnum d) (make-fixnum m))))

(define (fixnum-div0+mod0 x y)
  (call-with-values
   (lambda () (fixnum-div+mod x y))
   (lambda (d m)
     (let ((y/2 (fixnum-div y (make-fixnum 2))))
       (cond ((fixnum-positive? y/2)
              (if (fixnum>=? m y/2)
                  (if (and (fixnum=? m y/2)
                           (fixnum-odd? y))
                      (values d m)
                      (values (fixnum+ d (make-fixnum 1))
                              (fixnum- m y)))
                  (values d m)))
             (else
              (let ((y/2abs (fixnum- y/2)))
                (if (fixnum>=? m y/2abs)
                    (if (and (fixnum=? m y/2abs)
                             (fixnum-odd? y))
                        (values d m)
                        (values (fixnum- d (make-fixnum 1))
                                (fixnum+ m y)))
                    (values d m)))))))))

(define (fixnum+/2 x y)
  (let* ((a (fixnum-rep x))
         (b (fixnum-rep y)))
    (if (core:>= a 0)
        (if (core:>= b 0)
            (let* ((b- (core:+ b *low*))
                   (c (core:+ a b-)))
              (if (core:< c 0)
                  (make-fixnum (core:+ a b))
                  (make-fixnum (core:+ c *low*))))
            (make-fixnum (core:+ a b)))
        (if (core:< b 0)
            (let* ((b+ (core:- b *low*))
                   (c (core:+ a b+)))
              (if (core:>= c 0)
                  (make-fixnum (core:+ a b))
                  (make-fixnum (core:- c *low*))))
            (make-fixnum (core:+ a b))))))

(define (fixnum-/2 x y)
  (let ((b (fixnum-rep y)))
    (if (core:= b *low*)
        (fixnum+/2 x y)
        (fixnum+/2 x (make-fixnum (core:- b))))))

; (a * m + b) * (c * m + d)
; = (a * c) * m^2 + (a * d + b * c) * m + b * d

(define (fixnum*/2 x y)
  (call-with-values
   (lambda () (fixnum-div0+mod0 x (make-fixnum *half-modulus*)))
   (lambda (a b)
     (call-with-values
      (lambda () (fixnum-div0+mod0 y (make-fixnum *half-modulus*)))
      (lambda (c d)
        (let* ((a (fixnum-rep a))
               (b (fixnum-rep b))
               (c (fixnum-rep c))
               (d (fixnum-rep d))
               (a*d (core:* a d))
               (b*c (core:* b c))
               (b*d (core:* b d))
               (a*d+b*c (fixnum+/2 (make-fixnum a*d) (make-fixnum b*c)))
               (hibits (fixnum-rep
                        (fixnum-mod a*d+b*c (make-fixnum *half-modulus*))))
               (hibits (if (core:> hibits *half-high*)
                           (core:- hibits *half-modulus*)
                           hibits))
               (hi (make-fixnum (core:* hibits *half-modulus*)))
               (lo (make-fixnum b*d)))
        (fixnum+/2 hi lo)))))))

; Operations with carry

(define (fixnum+/carry x y c)
  (let* ((x+y (fixnum+/2 x y))
         (x+y+c (fixnum+/2 x+y c)))
    (values x+y+c
            (fixnum+/2 (fixnum-carry x y x+y)
                       (fixnum-carry x+y c x+y+c)))))

(define (fixnum-/carry x y c)
  (if (fixnum>? y (least-fixnum))
      (if (fixnum>? c (least-fixnum))
          ; This tail call is the usual case.
          (fixnum+/carry x (fixnum- y) (fixnum- c))
          ; c is the least fixnum
          (call-with-values
           (lambda () (fixnum+/carry x (fixnum- y) c))
           (lambda (x+y+c carry)
             (values x+y+c (fixnum+/2 carry (make-fixnum 1))))))
      (if (fixnum>? c (least-fixnum))
          ; y is the least fixnum
          (call-with-values
           (lambda () (fixnum+/carry x y (fixnum- c)))
           (lambda (x+y+c carry)
             (values x+y+c (fixnum+/2 carry (make-fixnum 1)))))
          ; Both y and c are the least fixnum.
          (call-with-values
           (lambda () (fixnum+/carry x y c))
           (lambda (x+y+c carry)
             (values x+y+c (fixnum+/2 carry (make-fixnum 2))))))))

; fixnum*/carry is almost the same as fixnum*/2
;
; (a * m + b) * (c * m + d)
; = (a * c) * m^2 + (a * d + b * c) * m + b * d

(define (fixnum*/carry x y z)
  (call-with-values
   (lambda () (fixnum-div0+mod0 x (make-fixnum *half-modulus*)))
   (lambda (a b)
     (call-with-values
      (lambda () (fixnum-div0+mod0 y (make-fixnum *half-modulus*)))
      (lambda (c d)
        (let* ((a*c (fixnum*/2 a c))
               (a*d (fixnum*/2 a d))
               (b*c (fixnum*/2 b c))
               (b*d (fixnum*/2 b d))
               (a*d+b*c (fixnum+/2 a*d b*c))
               (carry1 (fixnum-carry a*d b*c a*d+b*c)))
          (call-with-values
           (lambda () (fixnum-div0+mod0 a*d+b*c (make-fixnum *half-modulus*)))
           (lambda (carry2 hibits)
             (let* ((hi (fixnum*/2 hibits (make-fixnum *half-modulus*)))
                    (product0 (fixnum+/2 hi b*d))
                    (carry3 (fixnum-carry hi b*d product0))
                    (result0 (fixnum+/2 product0 z))
                    (carry4 (fixnum-carry product0 z result0)))
               '
               (begin
                (write (list (list x y z)
                            ;(list a b c d)
                             (list a*c a*d b*c b*d)
                             (list carry1 a*d+b*c)
                             (list carry2 hibits)
                             hi
                             (list carry3 product0)
                             (list carry4 result0)))
                (newline))
               (values
                result0
                (fixnum+/2 a*c
                           (fixnum+/2 (fixnum+/2 carry1 carry2)
                                      (fixnum+/2 carry3 carry4)))))))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; For making derived procedures.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-fixnum*fixnum->fixnum fixnum-op)
  (lambda (a b)
    (make-fixnum (fixnum-op (fixnum-rep a) (fixnum-rep b)))))

(define (make-fixnum->fixnum fixnum-op)
  (lambda (a)
    (make-fixnum (fixnum-op (fixnum-rep a)))))

(define (make-fixnum*fixnum->val fixnum-op)
  (lambda (a b)
    (fixnum-op (fixnum-rep a) (fixnum-rep b))))

(define (make-fixnum->val fixnum-op)
  (lambda (a)
    (fixnum-op (fixnum-rep a))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Derived procedures.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *fixnum-width* (make-fixnum *width*))
(define *fixnum-min* (make-fixnum *low*))
(define *fixnum-max* (make-fixnum *high*))

(define (fixnum-width) *fixnum-width*)
(define (least-fixnum) *fixnum-min*)
(define (greatest-fixnum) *fixnum-max*)

(define fixnum=? (make-transitive-pred (make-fixnum*fixnum->val core:=)))
(define fixnum>=? (make-transitive-pred (make-fixnum*fixnum->val core:>=)))
(define fixnum<=? (make-transitive-pred (make-fixnum*fixnum->val core:<=)))
(define fixnum>? (make-transitive-pred (make-fixnum*fixnum->val core:>)))
(define fixnum<? (make-transitive-pred (make-fixnum*fixnum->val core:<)))

(define fixnum-zero? (make-fixnum->val core:zero?))
(define fixnum-positive? (make-fixnum->val core:positive?))
(define fixnum-negative? (make-fixnum->val core:negative?))
(define fixnum-even? (make-fixnum->val core:even?))
(define fixnum-odd? (make-fixnum->val core:odd?))

(define fixnum-min (make-min/max fixnum<?))
(define fixnum-max (make-min/max fixnum>?))

(define (fixnum+ . args)
  (reduce (make-fixnum 0) fixnum+/2 args))

(define (fixnum* . args)
  (reduce (make-fixnum 1) fixnum*/2 args))

(define (fixnum- arg0 . args)
  (if (null? args)
      (fixnum-/2 (make-fixnum 0) arg0)
      ;FIXME: the definition of reduce in nary.scm is bonkers
      (do ((result arg0 (fixnum-/2 result (car args)))
           (args args (cdr args)))
          ((null? args) result))))

;FIXME: these should go away

(define fixnum-quotient (make-fixnum*fixnum->fixnum core:quotient))
(define fixnum-remainder (make-fixnum*fixnum->fixnum core:remainder))
(define fixnum-modulo (make-fixnum*fixnum->fixnum core:modulo))

(define (fixnum-quotient+remainder a b)
  (values (fixnum-quotient a b)
	  (fixnum-remainder a b)))

;end of FIXME

(define (fixnum-div x y)
  (call-with-values
   (lambda () (fixnum-div+mod x y))
   (lambda (d m)
     d)))

(define (fixnum-mod x y)
  (call-with-values
   (lambda () (fixnum-div+mod x y))
   (lambda (d m)
     m)))

(define (fixnum-div0 x y)
  (call-with-values
   (lambda () (fixnum-div0+mod0 x y))
   (lambda (d m) d)))

(define (fixnum-mod0 x y)
  (call-with-values
   (lambda () (fixnum-div0+mod0 x y))
   (lambda (d m) m)))

; See r5rs.sch for definitions of
; arithmetic-shift, bitwise-and, bitwise-ior, bitwise-xor, 
; bitwise-not, and bit-count.

(define fixnum-not (make-fixnum->fixnum bitwise-not))

(define fixnum-and/2 (make-fixnum*fixnum->fixnum bitwise-and))
(define (fixnum-and . args)
  (reduce (make-fixnum -1)
	  fixnum-and/2
	  args))

(define fixnum-ior/2 (make-fixnum*fixnum->fixnum bitwise-ior))
(define (fixnum-ior . args)
  (reduce (make-fixnum 0)
	  fixnum-ior/2
	  args))

(define fixnum-xor/2 (make-fixnum*fixnum->fixnum bitwise-xor))
(define (fixnum-xor . args)
  (reduce (make-fixnum 0)
	  fixnum-xor/2
	  args))

(define (fixnum-if x y z)
  (fixnum-ior (fixnum-and x y)
              (fixnum-and (fixnum-not x) z)))

(define fixnum-bit-count (make-fixnum->fixnum bit-count))

(define (fixnum-length x)
  (do ((result (make-fixnum 0) (fixnum+ result (make-fixnum 1)))
       (bits (if (fixnum-negative? x)
                 (fixnum-not x)
                 x)
             (fixnum-logical-shift-right bits (make-fixnum 1))))
      ((fixnum-zero? bits)
       result)))

(define (fixnum-first-bit-set x)
  (if (fixnum-zero? (fixnum-and x (make-fixnum -1)))
      (make-fixnum -1)
      (let loop ((result (make-fixnum 0))
                 (x x)
                 (mask (make-fixnum 255))
                 (increment (make-fixnum 8)))
        (let ((y (fixnum-and x mask)))
          (if (fixnum-zero? y)
              (loop (fixnum+ result increment)
                    (fixnum-logical-shift-right x increment)
                    mask
                    increment)
              (if (fixnum=? increment (make-fixnum 1))
                  result
                  (loop result x (make-fixnum 1) (make-fixnum 1))))))))

(define (fixnum-bit-set? x y)
  (if (fixnum-negative? y)
      (error "illegal second argument to fixnum-bit-set?" x y)
      (not (fixnum-zero?
            (fixnum-and x (fixnum-logical-shift-left (make-fixnum 1) y))))))

(define (fixnum-copy-bit x y z)
  (if (fixnum-negative? y)
      (error "illegal second argument to fixnum-copy-bit" x y z)
      (let* ((mask (fixnum-logical-shift-left (make-fixnum 1) y)))
      (fixnum-if mask
                 (fixnum-logical-shift-left z y)
                 x))))

(define (fixnum-bit-field x y z)
  (if (or (fixnum-negative? y) (fixnum-negative? z))
      (error "illegal second or third argument to fixnum-bit-field" x y z)
      (let* ((mask (fixnum-not
                    (fixnum-logical-shift-left (make-fixnum -1) z))))
        (fixnum-logical-shift-right (fixnum-and x mask)
                                    y))))

(define (fixnum-copy-bit-field to start end from)
  (if (or (fixnum-negative? start) (fixnum-negative? end))
      (error "illegal second or third argument to fixnum-copy-bit-field"
             to start end from)
      (let* ((mask1 (fixnum-logical-shift-left (make-fixnum -1) start))
             (mask2 (fixnum-not
                     (fixnum-logical-shift-left (make-fixnum -1) end)))
             (mask (fixnum-and mask1 mask2)))
        (fixnum-if mask
                   (fixnum-logical-shift-left from start)
                   to))))

(define (fixnum-arithmetic-shift x y)
  (cond ((fixnum-positive? y)
         (fixnum-arithmetic-shift-left x y))
        ((fixnum-zero? y)
         x)
        (else
         (fixnum-arithmetic-shift-right x (fixnum- y)))))

(define (fixnum-arithmetic-shift-left x y)
  (if (fixnum-negative? y)
      (error "negative second argument to fixnum-arithmetic-shift-left" x y)
      (do ((x x (fixnum+ x x))
           (y y (fixnum- y (make-fixnum 1))))
          ((or (fixnum-zero? x) (fixnum-zero? y))
           x))))

(define (fixnum-arithmetic-shift-right x y)
  (if (fixnum-negative? y)
      (error "negative second argument to fixnum-arithmetic-shift-left" x y)
      (do ((x x (fixnum-div x (make-fixnum 2)))
           (y y (fixnum- y (make-fixnum 1))))
          ((or (fixnum-zero? x) (fixnum-zero? y))
           x))))

(define fixnum-logical-shift-left fixnum-arithmetic-shift-left)

(define (fixnum-logical-shift-right fixnum1 fixnum2)
  (cond
   ((fixnum-negative? fixnum2)
    (error "negative shift argument to fixnum-logical-shift-left"
           fixnum1 fixnum2))
   ((fixnum-zero? fixnum2) fixnum1)
   ((fixnum-positive? fixnum1)
    (fixnum-arithmetic-shift-right fixnum1 fixnum2))
   ((fixnum>? fixnum2 *fixnum-width*) (make-fixnum 0))
   (else
    (fixnum-logical-shift-right
     (fixnum-and (fixnum-arithmetic-shift-right fixnum1 (make-fixnum 1))
                 (greatest-fixnum))
     (fixnum- fixnum2 (make-fixnum 1))))))

(define (fixnum-rotate-bit-field n start end count)
  (if (or (fixnum-negative? start)
          (fixnum-negative? end)
          (fixnum-negative? count))
      (error "illegal negative argument to fixnum-rotate-bit-field"
             n start end count)
      (let ((width (fixnum- end start)))
        (if (fixnum-positive? width)
            (let* ((count (fixnum-mod count width))
                   (field0 (fixnum-bit-field n start end))
                   (field1 (fixnum-logical-shift-left field0 count))
                   (field2 (fixnum-logical-shift-right field0
                                                       (fixnum- width count)))
                   (field (fixnum-ior field1 field2)))
              (fixnum-copy-bit-field n start end field))
            n))))

(define (fixnum-reverse-bit-field n start end)
  (if (or (fixnum-negative? start)
          (fixnum-negative? end))
      (error "illegal negative argument to fixnum-reverse-bit-field"
             n start end)
      (let* ((width (fixnum- end start))
             (field (fixnum-bit-field n start end)))
        (do ((reversed (make-fixnum 0)
                       (fixnum+ (fixnum+ reversed reversed)
                                (fixnum-and field (make-fixnum 1))))
             (field field (fixnum-logical-shift-right field (make-fixnum 1)))
             (k width (fixnum- k (make-fixnum 1))))
            ((fixnum-zero? k)
             (fixnum-copy-bit-field n start end reversed))))))


; The fx operations signal an error instead of returning a value
; that might depend upon the fixnum precision.

; Some of the fx operations are the same as the fixnum operations.

(define fx=? fixnum=?)
(define fx>? fixnum>?)
(define fx<? fixnum<?)
(define fx>=? fixnum>=?)
(define fx<=? fixnum<=?)

(define fxzero? fixnum-zero?)
(define fxpositive? fixnum-positive?)
(define fxnegative? fixnum-negative?)

(define fxodd? fixnum-odd?)
(define fxeven? fixnum-even?)

(define fxmax fixnum-max)
(define fxmin fixnum-min)

(define (fx+ x y)
  (let ((z (fixnum+ x y)))
    (if (fixnum-positive? x)
        (if (fixnum-positive? y)
            (if (fixnum-positive? z)
                z
                (error "fixnum overflow in fx+" x y))
            z)
        (if (fixnum-negative? y)
            (if (fixnum-negative? z)
                z
                (error "fixnum overflow in fx+" x y))
            z))))

(define (fx- x . rest)
  (if (null? rest)
      (fx- (make-fixnum 0) x)
      (let* ((y (car rest))
             (z (fixnum- x y)))
        (if (fixnum-positive? x)
            (if (fixnum-negative? y)
                (if (fixnum-positive? z)
                    z
                    (error "fixnum overflow in fx-" x y))
                z)
            (if (fixnum-positive? y)
                (if (fixnum-negative? z)
                    z
                    (error "fixnum overflow in fx-" x y))
                z)))))

(define (fx* x y)
  (call-with-values
   (lambda () (fixnum*/carry x y (make-fixnum 0)))
   (lambda (result carry)
     (if (fixnum-zero? carry)
         result
         (error "fixnum overflow in fx*" x y)))))

(define (fxdiv+mod x y)
  (if (and (fixnum=? x (least-fixnum))
           (fixnum=? y (make-fixnum -1)))
      (error "fixnum overflow in fxdiv+mod" x y)
      (fixnum-div+mod x y)))

(define (fxdiv x y)
  (if (and (fixnum=? x (least-fixnum))
           (fixnum=? y (make-fixnum -1)))
      (error "fixnum overflow in fxdiv" x y)
      (fixnum-div x y)))

(define fxmod fixnum-mod)

(define (fxdiv0+mod0 x y)
  (if (and (fixnum=? x (least-fixnum))
           (fixnum=? y (make-fixnum -1)))
      (error "fixnum overflow in fxdiv0+mod0" x y)
      (fixnum-div0+mod0 x y)))

(define (fxdiv0 x y)
  (if (and (fixnum=? x (least-fixnum))
           (fixnum=? y (make-fixnum -1)))
      (error "fixnum overflow in fxdiv0" x y)
      (fixnum-div0 x y)))

(define fxmod0 fixnum-mod0)

(define fxnot fixnum-not)
(define fxand fixnum-and)
(define fxior fixnum-ior)
(define fxxor fixnum-xor)

(define fxif fixnum-if)

(define fxbit-count fixnum-bit-count)
(define fxlength fixnum-length)
(define fxfirst-bit-set fixnum-first-bit-set)

(define (fxbit-set? x y)
  (if (or (fixnum<? y (make-fixnum 0))
          (fixnum>=? y (fixnum-width)))
      (error "second argument to fxbit-set! is out of range" x y)
      (fixnum-bit-set? x y)))

(define (fxcopy-bit x y z)
  (if (or (fixnum<? y (make-fixnum 0))
          (fixnum>=? y (fixnum-width))
          (fixnum<? z (make-fixnum 0))
          (fixnum>? z (make-fixnum 1)))
      (error "illegal argument(s) to fxcopy-bit" x y z)
      (fixnum-copy-bit x y z)))

(define (fxbit-field x y z)
  (if (or (fixnum<? y (make-fixnum 0))
          (fixnum<? z (make-fixnum 0))
          (fixnum>? y z))
      (error "illegal argument(s) to fxbit-field" x y z)
      (fixnum-bit-field x y z)))

(define (fxcopy-bit-field x y z w)
  (if (or (fixnum<? y (make-fixnum 0))
          (fixnum<? z (make-fixnum 0))
          (fixnum>? y (fixnum-width))
          (fixnum>? z (fixnum-width)))
      (error "illegal argument(s) to fxcopy-bit-field" x y z w)
      (fixnum-copy-bit-field x y z w)))

(define (fxarithmetic-shift x y)
  (if (or (fixnum<? y (fixnum- (fixnum-width)))
          (fixnum>? y (fixnum-width)))
      (error "illegal second argument to fxarithmetic-shift" x y)
      (let ((z (fixnum-arithmetic-shift x y)))
        (cond ((fixnum-negative? y) z)
              ((fixnum=? x (fixnum-arithmetic-shift z (fixnum- y)))
               z)
              (else
               (error "fixnum overflow in fxarithmetic-shift" x y))))))

(define (fxarithmetic-shift-left x y)
  (if (fixnum<? y (make-fixnum 0))
      (error "negative second argument to fxarithmetic-shift-left" x y)
      (fxarithmetic-shift x y)))

(define (fxarithmetic-shift-right x y)
  (if (fixnum<? y (make-fixnum 0))
      (error "negative second argument to fxarithmetic-shift-right" x y)
      (fxarithmetic-shift x (fixnum- y))))

(define (fxrotate-bit-field n start end count)
  (if (or (fixnum-negative? start)
          (fixnum-negative? end)
          (fixnum-negative? count)
          (fixnum>? start (fixnum-width))
          (fixnum>? end (fixnum-width))
          (fixnum>? count (fixnum- end start)))
      (error "illegal argument to fxrotate-bit-field"
             n start end count)
      (fixnum-rotate-bit-field n start end count)))

(define (fxreverse-bit-field n start end)
  (if (or (fixnum-negative? start)
          (fixnum-negative? end)
          (fixnum>? start (fixnum-width))
          (fixnum>? end (fixnum-width))
          (fixnum>? start end))
      (error "illegal argument to fxreverse-bit-field"
             n start end)
      (fixnum-reverse-bit-field n start end)))

)
