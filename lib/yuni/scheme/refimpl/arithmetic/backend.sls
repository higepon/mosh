;; scheme backend for (rnrs)

(library (yuni scheme refimpl arithmetic backend)
         (export 

           ;; yuni core things
           define* make let-with is-a?


           ;; rnrs things
           define-syntax syntax-rules
           delay force

           set!
           let define if or and cond letrec case
           lambda let* do not quote begin
           cons
           null? car cdr else eq?
           cadr cddr length
           list
           map assq
           pair?
           memq
           values
           call-with-values

           vector
           vector-ref
           vector-set!
           make-vector

           string-copy
           list->string
           string->list
           string-ref
           string-append
           substring
           make-string
           string-length

           char=?
           char->integer
           char<=?
           char>=?
           char-downcase
           integer->char

           display
           write
           newline

           ;; error
           error
           (rename 
             ;; error
             (error r6rs-error)
             ;; predicates
             (eqv? core:eqv?)
             (number? core:number?)
             (complex? core:complex?)
             (real? core:real?)
             (rational? core:rational?)
             (integer? core:integer?)
             (exact? core:exact?)
             (inexact? core:inexact?)

             (zero? core:zero?)
             (positive? core:positive?)
             (negative? core:negative?)
             (odd? core:odd?)
             (even? core:even?)


             ;; ops
             (= core:=)
             (< core:<)
             (> core:>)
             (<= core:<=)
             (>= core:>=)
             (max core:max)
             (min core:min)
             (+ core:+)
             (* core:*)
             (- core:-)
             (/ core:/)
             (remainder core:remainder)
             (abs core:abs)
             (quotient core:quotient)
             (modulo core:modulo)
             (gcd core:gcd)
             (lcm core:lcm)
             (numerator core:numerator)
             (denominator core:denominator)
             (floor core:floor)
             (ceiling core:ceiling)
             (truncate core:truncate)
             (round core:round)
             (rationalize core:rationalize)
             (exp core:exp)
             (log core:log)
             (sin core:sin)
             (cos core:cos)
             (tan core:tan)
             (asin core:asin)
             (acos core:acos)
             (atan core:atan)
             (sqrt core:sqrt)
             (expt core:expt)
             (make-rectangular core:make-rectangular)
             (make-polar core:make-polar)
             (real-part core:real-part)
             (imag-part core:imag-part)
             (magnitude core:magnitude)
             (angle core:angle)
             (inexact core:exact->inexact)
             (exact core:inexact->exact)
             (number->string core:number->string)
             (string->number core:string->number)
             ))
         (import 
           (for (yuni core) run expand)
           (for (except (rnrs) error) run expand)
           (for (rnrs r5rs) run expand))
(define (error . x)
  (assertion-violation 'error "error" x))

)
