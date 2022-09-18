#!r6rs
(library (r7b-impl base)
         (export
* +
- ...
/ <
<= =
=> >
>=
abs and
append apply
assoc assq
assv begin
binary-port? boolean=?
boolean? bytevector
bytevector-append bytevector-copy
bytevector-copy! bytevector-length
bytevector-u8-ref bytevector-u8-set!
bytevector? caar
cadr
call-with-current-continuation
call-with-port call-with-values
call/cc car
case cdar
cddr cdr
ceiling char->integer
char-ready? char<=?
char<? char=?
char>=? char>?
char? close-input-port
close-output-port close-port
complex? cond
cond-expand cons
current-error-port current-input-port
current-output-port define
define-record-type define-syntax
define-values denominator
do dynamic-wind
else eof-object
eof-object? eq?
equal? eqv?
error error-object-irritants
error-object-message error-object?
even? exact
exact-integer-sqrt exact-integer?
exact? expt
features file-error?
floor floor-quotient
floor-remainder floor/
flush-output-port for-each
gcd get-output-bytevector
get-output-string guard
if include
include-ci inexact
inexact? input-port-open?
input-port? integer->char
integer? lambda
lcm length
let let*
let*-values let-syntax
let-values letrec
letrec* letrec-syntax
list list->string
list->vector list-copy
list-ref list-set!
list-tail list?
make-bytevector make-list
make-parameter make-string
make-vector map
max member
memq memv
min modulo
negative? newline
not null?
number->string number?
numerator odd?
open-input-bytevector open-input-string
open-output-bytevector open-output-string
or output-port-open?
output-port? pair?
parameterize peek-char
peek-u8 port?
positive? procedure?
quasiquote quote
quotient raise
raise-continuable rational?
rationalize read-bytevector
read-bytevector! read-char
read-error? read-line
read-string read-u8
real? remainder
reverse round
set! set-car!
set-cdr! square
string string->list
string->number string->symbol
string->utf8 string->vector
string-append string-copy
string-copy! string-fill!
string-for-each string-length
string-map string-ref
string-set! string<=?
string<? string=?
string>=? string>?
string? substring
symbol->string symbol=?
symbol? syntax-error
syntax-rules textual-port?
truncate truncate-quotient
truncate-remainder truncate/
u8-ready? unless
unquote unquote-splicing
utf8->string values
vector vector->list
vector->string vector-append
vector-copy vector-copy!
vector-fill! vector-for-each
vector-length vector-map
vector-ref vector-set!
vector? when
with-exception-handler write-bytevector
write-char write-string
write-u8 zero?
   )
         (import (rename (except (rnrs) vector-copy vector-fill! case syntax-rules error string->list define-record-type)
                   (vector->list r6rs:vector->list) (bytevector-copy! r6rs:bytevector-copy!) (utf8->string r6rs:utf8->string) (string->utf8 r6rs:string->utf8) (bytevector-copy r6rs:bytevector-copy))
                 (rnrs mutable-pairs)
                 (except (rnrs mutable-strings) string-fill!)
                 (rnrs r5rs)
                 (srfi i0)
                 (srfi i6)
                 (srfi i23)
                 (srfi i9)
                 (srfi i39)
                 (only (srfi :13) string-copy! string-fill! string->list)
                 (only (srfi :43) vector-append vector-fill!)
                 (only (r7b-impl division) floor/ floor-quotient floor-remainder truncate/ truncate-remainder truncate-quotient)
                 (r7b-util bytevector-buffer)
                 (r7b-util char-ready)
                 (r7b-util u8-ready)
                 (r7b-util port-open)
                 (for (r7b-util syntax-rules) run expand)
                 (for (r7b-util case) run expand)
                 (only (mosh) include)
                 )

;; R7RS-bridge format doesn't allow (begin (import ...) ...)
(define-syntax import
  (lambda (x)
    (syntax-case x ()
      ((_ ...) (assertion-violation 'import
                                    "Not allowed here..")))))

(define-syntax syntax-error
  (lambda (x)
    (syntax-case x ()
      ((_ message args ...) (syntax-violation 'syntax-error
                                              #'message
                                              (quote  #'(args ...)))))))
;; R7RS error object will be mapped to R6RS condition object
(define error-object? condition?)

(define (error-object-irritants obj)
  (and (irritants-condition? obj)
       (condition-irritants obj)))

(define (error-object-message obj)
  (and (message-condition? obj)
       (condition-message obj)))

(define (open-input-bytevector bv) (open-bytevector-input-port bv))

(define bytevector-copy
  (case-lambda
    ((bv start end)
      (bytevector-copy-partial bv start end))
    ((bv start)
      (bytevector-copy-partial bv start (bytevector-length bv)))
    ((bv)
      (r6rs:bytevector-copy bv))))

(define (bytevector-copy-partial bv start end)
  (let ((ret (make-bytevector (- end start))))
    (define (itr cur)
      (unless (= (+ start cur) end)
        (bytevector-u8-set! ret cur (bytevector-u8-ref bv (+ start cur)))
        (itr (+ cur 1))))
    (itr 0)
    ret))

(define (bytevector-copy-partial! from start end to at)
  (define (itr cur)
    (unless (= (+ start cur) end)
      (let ((val (bytevector-u8-ref from (+ start cur))))
        (bytevector-u8-set! to (+ at cur) val)
        (itr (+ cur 1)))))
  (itr 0))


(define-syntax define-values
  (lambda (x)
    (syntax-case x ()
      ((_ (val ...) body)
       (with-syntax (((name ...)
                      (generate-temporaries #'(val ...)))
                     ((tmp ...)
                      (generate-temporaries #'(val ...))))
         #'(begin
             (define name #f)
             ...
             (define bogus
               (begin
                 (call-with-values (lambda () body)
                                   (lambda (tmp ...)
                                     (set! name tmp)
                                     ...
                                     ))))
             (define val name)
             ...
             ))))))

(define (exact-integer? i) (and (integer? i) (exact? i)))

(define (list-set! l k obj)
  (define (itr cur count)
    (if (= count k)
      (set-car! cur obj)
      (itr (cdr cur) (+ count 1))))
  (itr l 0))

(define make-list
  (case-lambda
    ((k fil) (vector->list (make-vector k fil)))
    ((k) (make-list k 'unspecified))))

(define peek-u8
  (case-lambda
    (() (peek-u8 (current-input-port)))
    ((port)
     (lookahead-u8 port))))

(define read-bytevector
  (case-lambda
    ((len) (read-bytevector len (current-input-port)))
    ((len port) (get-bytevector-n port len))))

(define read-bytevector!
  (case-lambda
    ((bv start end)
     (read-bytevector! bv start end (current-input-port)))
    ((bv start end port)
     (get-bytevector-n! port bv start (- end start)))))

(define read-line
  (case-lambda
    (() (read-line (current-input-port)))
    ((port) (get-line port))))

(define write-u8
  (case-lambda
    ((obj) (write-u8 obj (current-output-port)))
    ((obj port) (put-u8 port obj))))

(define read-u8
  (case-lambda
    (() (read-u8 (current-input-port)))
    ((port) (get-u8 port))))

(define write-bytevector
  (case-lambda
    ((bv port)
     (put-bytevector port bv))
    ((bv) (write-bytevector bv (current-output-port)))))

(define write-partial-bytevector
  (case-lambda
    ((bv start end) (write-partial-bytevector bv start end
                                              (current-output-port)))
    ((bv start end port)
     (put-bytevector port bv start (- end start)))))

(define (write-string . args)
  (raise "write-string not supported"))

(define vector-copy
  (case-lambda
    ((v start end)
      (vector-copy-partial v start end))
    ((v start)
      (vector-copy-partial v start (vector-length v)))
    ((v)
      (vector-copy-partial v 0 (vector-length v)))))

(define (vector-copy-partial v start end)
  (let ((ret (make-vector (- end start))))
    (define (itr cur)
      (unless (= (+ start cur) end)
        (vector-set! ret cur (vector-ref v (+ start cur)))
        (itr (+ cur 1))))
    (itr 0)
    ret))

;; vector-copy! bytevector-copy! bytevector-append from　https://github.com/okuoku/yuni.
(define (vector-copy!/itr+ to at from start end)
  (unless (= start end)
    (vector-set! to at (vector-ref from start))
    (vector-copy!/itr+ to (+ 1 at) from (+ 1 start) end)))

(define (vector-copy!/itr- to at from start end)
  (vector-set! to at (vector-ref from end))
  (unless (= start end)
    (vector-copy!/itr- to (- at 1) from start (- end 1))))

(define vector-copy!
  (case-lambda
    ((to at from)
     (vector-copy! to at from 0
                   (min (- (vector-length to) at) (vector-length from))))
    ((to at from start)
     (vector-copy! to at from start
                   (min (+ start (- (vector-length to) at))
                        (vector-length from))))
    ((to at from start end)
     (unless (= start end)
       (cond
         ((and (eq? from to)
               (< start at end))
          (vector-copy!/itr- to (+ (- end start) at) from start (- end 1)))
         (else
           (vector-copy!/itr+ to at from start end)))))))

(define (bytevector-append . bvs)
  (u8-list->bytevector (apply append (map bytevector->u8-list bvs))))

(define (bytevector . elm*)
  (u8-list->bytevector elm*))

(define bytevector-copy!
  (case-lambda
    ((to at from) (bytevector-copy! to at from 0))
    ((to at from start)
     (let ((flen (bytevector-length from))
           (tlen (bytevector-length to)))
       (let ((fmaxcopysize (- flen start))
             (tmaxcopysize (- tlen at)))
         (bytevector-copy! to at from start (+ start
                                               (min fmaxcopysize
                                                    tmaxcopysize))))))
    ((to at from start end)
     (r6rs:bytevector-copy! from start to at (- end start)))))

; TODO(higepon): Using bytevector-copy is not very efficient.
(define utf8->string
  (case-lambda
    ((bv start end)
      (r6rs:utf8->string (bytevector-copy bv start end)))
    ((bv start)
      (r6rs:utf8->string (bytevector-copy bv start (bytevector-length bv))))
    ((bv)
      (r6rs:utf8->string bv))))

; TODO(higepon): Using string-copy is not very efficient.
(define string->utf8
  (case-lambda
    ((s start end)
      (r6rs:string->utf8 (string-copy s start end)))
    ((s start)
      (r6rs:string->utf8 (string-copy s start (string-length s))))
    ((s)
      (r6rs:string->utf8 s))))

(define (read-string . args)
  (raise "read-string not supported"))

(define (output-port-open? . args)
  (raise "output-port-open? not supported"))

(define (input-port-open? . args)
  (raise "inpput-port-open? not supported"))

(define read-error?  i/o-read-error?)

(define file-error?  i/o-read-error?)

;; TODO(higepon): Include is macro.
(define (include-ci . args)
  (raise "include-ci not supported"))

 (define (features)
   '(r7rs mosh))

(define string->vector
  (case-lambda
    ((s start end)
      (string->vector-partial s start end))
    ((s start)
      (string->vector-partial s start (string-length s)))
    ((s)
      (string->vector-partial s 0 (string-length s)))))

(define (string->vector-partial s start end)
  (let ((ret (make-vector (- end start))))
    (define (itr cur)
      (unless (= (+ start cur) end)
        (vector-set! ret cur (string-ref s (+ start cur)))
        (itr (+ cur 1))))
    (itr 0)
    ret))

(define vector->string
  (case-lambda
    ((v start end)
      (vector->string-partial v start end))
    ((v start)
      (vector->string-partial v start (vector-length v)))
    ((v)
      (vector->string-partial v 0 (vector-length v)))))

(define (vector->string-partial v start end)
  (let ((ret (make-string (- end start))))
    (define (itr cur)
      (unless (= (+ start cur) end)
        (string-set! ret cur (vector-ref v (+ start cur)))
        (itr (+ cur 1))))
    (itr 0)
    ret))

(define vector->list
  (case-lambda
    ((v start end)
      (vector->list-partial v start end))
    ((v start)
      (vector->list-partial v start (vector-length v)))
    ((v)
      (vector->list-partial v 0 (vector-length v)))))

(define (vector->list-partial v start end)
  (let ((ret (make-vector (- end start))))
    (define (itr cur)
      (unless (= (+ start cur) end)
        (vector-set! ret cur (vector-ref v (+ start cur)))
        (itr (+ cur 1))))
    (itr 0)
    (r6rs:vector->list ret)))

;; list-copy from https://github.com/okuoku/yuni/.
(define (list-copy/itr! cur lis)
  (cond
    ((pair? lis)
     (let ((c (cons (car lis) '())))
      (set-cdr! cur c)
      (list-copy/itr! c (cdr lis))))
    (else
      (set-cdr! cur lis))))

(define (list-copy obj)
  (if (pair? obj)
    (let ((c (cons (car obj) '())))
     (list-copy/itr! c (cdr obj))
     c)
    obj))

(define (string-map proc . strs)
  (list->string (apply map proc (map string->list strs))))

(define (square z)
  (* z z))

)
