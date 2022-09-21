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
         (import (rename (except (rnrs) assoc member vector-copy map for-each vector-fill! vector-map case syntax-rules error string->list define-record-type)
                   (vector->list r6rs:vector->list) (bytevector-copy! r6rs:bytevector-copy!) (utf8->string r6rs:utf8->string) (string->utf8 r6rs:string->utf8) (bytevector-copy r6rs:bytevector-copy))
                 (rnrs mutable-pairs)
                 (except (rnrs mutable-strings) string-fill!)
                 (rnrs r5rs)
                 (srfi i0)
                 (srfi i6)
                 (srfi i23)
                 (srfi i9)
                 (srfi i39)
                 (only (srfi :1) member assoc)
                 (only (srfi :13) string-copy! string-fill! string->list)
                 (only (srfi :43) vector-append vector-fill! vector-copy!)
                 (only (r7b-impl division) floor/ floor-quotient floor-remainder truncate/ truncate-remainder truncate-quotient)
                 (r7b-util bytevector-buffer)
                 (r7b-util char-ready)
                  (for (r7b-util syntax-rules) run expand)
                 (for (r7b-util case) run expand)
                 (only (mosh) include)
                 (only (system) port-open?)
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

;; N.B. This is fragile.
(define (byte-array-input-port? port)
   (and (input-port? port)
        (string=? "<byte-array-input-port>" (let ([p (open-output-string)]) (write port p) (get-output-string p)))))

(define (u8-ready? port)
  (byte-array-input-port? port))

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
    ((bv)
      (read-bytevector! bv (current-input-port) 0 (bytevector-length bv)))
    ((bv port)
      (read-bytevector! bv port 0 (bytevector-length bv)))
    ((bv port start)
     (read-bytevector! bv port start (bytevector-length bv)))
    ((bv port start end)
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
    ((bv port start end)
      (put-bytevector port bv start (- end start)))
    ((bv port start)
      (write-bytevector bv port start (bytevector-length bv)))
    ((bv port)
      (write-bytevector bv port 0 (bytevector-length bv)))
    ((bv)
      (write-bytevector bv (current-output-port) 0 (bytevector-length bv)))))

(define write-partial-bytevector
  (case-lambda
    ((bv start end) (write-partial-bytevector bv start end
                                              (current-output-port)))
    ((bv start end port)
     (put-bytevector port bv start (- end start)))))

(define write-string
  (case-lambda
    ((str) (write-string str (current-output-port)))
    ((str port) (put-string port str))
    ((str port start) (write-string str port start (string-length str)))
    ((str port start end)
     (write-string (substring str start end) port))))

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

(define (bytevector-append . bvs)
  (u8-list->bytevector (apply append (map bytevector->u8-list bvs))))

(define (bytevector . elm*)
  (u8-list->bytevector elm*))

;; bytevector-copy! bytevector-append write-string
;; string-map vector-map map
;; from https://github.com/okuoku/yuni.
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

(define read-string
  (case-lambda
    ((len) (read-string len (current-input-port)))
    ((len port) (get-string-n port len))))

(define (output-port-open? port)
  (port-open? port))

(define (input-port-open? port)
  (port-open? port))

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

(define (square z)
  (* z z))

(define (string-map4/itr! v pos len proc a b c d)
  (unless (= pos len)
    (string-set! v pos
                 (proc (string-ref a pos)
                       (string-ref b pos)
                       (string-ref c pos)
                       (string-ref d pos)))
    (string-map4/itr! v (+ pos 1) len proc a b c d)))

(define (string-map4 proc a b c d)
  (let* ((len (min (string-length a) (string-length b)
                   (string-length c) (string-length d)))
         (v (make-string len)))
    (string-map4/itr! v 0 len proc a b c d)
    v))

(define (string-map3/itr! v pos len proc a b c)
  (unless (= pos len)
    (string-set! v pos
                 (proc (string-ref a pos)
                       (string-ref b pos)
                       (string-ref c pos)))
    (string-map3/itr! v (+ pos 1) len proc a b c)))

(define (string-map3 proc a b c)
  (let* ((len (min (string-length a) (string-length b) (string-length c)))
         (v (make-string len)))
    (string-map3/itr! v 0 len proc a b c)
    v))

(define (string-map2/itr! v pos len proc a b)
  (unless (= pos len)
    (string-set! v pos
                 (proc (string-ref a pos)
                       (string-ref b pos)))
    (string-map2/itr! v (+ pos 1) len proc a b)))

(define (string-map2 proc a b)
  (let* ((len (min (string-length a) (string-length b)))
         (v (make-string len)))
    (string-map2/itr! v 0 len proc a b)
    v))

(define (string-map1/itr! v pos len proc a)
  (unless (= pos len)
    (string-set! v pos
                 (proc (string-ref a pos)))
    (string-map1/itr! v (+ pos 1) len proc a)))

(define (string-map1 proc a)
  (let* ((len (string-length a))
         (v (make-string len)))
    (string-map1/itr! v 0 len proc a)
    v))

(define (string-map proc a . args)
  (if (null? args)
    (string-map1 proc a)
    (let ((b (car args))
          (bb (cdr args)))
      (if (null? bb)
        (string-map2 proc a b)
        (let ((c (car bb))
              (cc (cdr bb)))
          (if (null? cc)
            (string-map3 proc a b c)
            (let ((d (car cc))
                  (dd (cdr cc)))
              (if (null? dd)
                (string-map4 proc a b c d)
                (error "Too many...")))))))))

(define (vector-map4/itr! v pos len proc a b c d)
  (unless (= pos len)
    (vector-set! v pos
                 (proc (vector-ref a pos)
                       (vector-ref b pos)
                       (vector-ref c pos)
                       (vector-ref d pos)))
    (vector-map4/itr! v (+ pos 1) len proc a b c d)))

(define (vector-map4 proc a b c d)
  (let* ((len (min (vector-length a) (vector-length b)
                   (vector-length c) (vector-length d)))
         (v (make-vector len)))
    (vector-map4/itr! v 0 len proc a b c d)
    v))

(define (vector-map3/itr! v pos len proc a b c)
  (unless (= pos len)
    (vector-set! v pos
                 (proc (vector-ref a pos)
                       (vector-ref b pos)
                       (vector-ref c pos)))
    (vector-map3/itr! v (+ pos 1) len proc a b c)))

(define (vector-map3 proc a b c)
  (let* ((len (min (vector-length a) (vector-length b) (vector-length c)))
         (v (make-vector len)))
    (vector-map3/itr! v 0 len proc a b c)
    v))

(define (vector-map2/itr! v pos len proc a b)
  (unless (= pos len)
    (vector-set! v pos
                 (proc (vector-ref a pos)
                       (vector-ref b pos)))
    (vector-map2/itr! v (+ pos 1) len proc a b)))

(define (vector-map2 proc a b)
  (let* ((len (min (vector-length a) (vector-length b)))
         (v (make-vector len)))
    (vector-map2/itr! v 0 len proc a b)
    v))

(define (vector-map1/itr! v pos len proc a)
  (unless (= pos len)
    (vector-set! v pos
                 (proc (vector-ref a pos)))
    (vector-map1/itr! v (+ pos 1) len proc a)))

(define (vector-map1 proc a)
  (let* ((len (vector-length a))
         (v (make-vector len)))
    (vector-map1/itr! v 0 len proc a)
    v))

(define (vector-map proc a . args)
  (if (null? args)
    (vector-map1 proc a)
    (let ((b (car args))
          (bb (cdr args)))
      (if (null? bb)
        (vector-map2 proc a b)
        (let ((c (car bb))
              (cc (cdr bb)))
          (if (null? cc)
            (vector-map3 proc a b c)
            (let ((d (car cc))
                  (dd (cdr cc)))
              (if (null? dd)
                (vector-map4 proc a b c d)
                (error "Too many...")))))))))

(define (map4/itr! cur proc a b c d)
  (unless (or (null? a) (null? b) (null? c) (null? d))
    (let ((p (cons (proc (car a) (car b) (car c) (car d)) '())))
     (set-cdr! cur p)
     (map4/itr! p proc (cdr a) (cdr b) (cdr c) (cdr d)))))

(define (map4 proc a b c d)
  (if (or (null? a) (null? b) (null? c) (null? d))
    '()
    (let ((p (cons (proc (car a) (car b) (car c) (car d)) '())))
     (map4/itr! p proc (cdr a) (cdr b) (cdr c) (cdr d))
     p)))

(define (map3/itr! cur proc a b c)
  (unless (or (null? a) (null? b) (null? c))
    (let ((p (cons (proc (car a) (car b) (car c)) '())))
     (set-cdr! cur p)
     (map3/itr! p proc (cdr a) (cdr b) (cdr c)))))

(define (map3 proc a b c)
  (if (or (null? a) (null? b) (null? c))
    '()
    (let ((p (cons (proc (car a) (car b) (car c)) '())))
     (map3/itr! p proc (cdr a) (cdr b) (cdr c))
     p)))

(define (map2/itr! cur proc a b)
  (unless (or (null? a) (null? b))
    (let ((c (cons (proc (car a) (car b)) '())))
     (set-cdr! cur c)
     (map2/itr! c proc (cdr a) (cdr b)))))

(define (map2 proc a b)
  (if (or (null? a) (null? b))
    '()
    (let ((c (cons (proc (car a) (car b)) '())))
     (map2/itr! c proc (cdr a) (cdr b))
     c)))

(define (map1/itr! cur proc a)
  (unless (null? a)
    (let ((c (cons (proc (car a)) '())))
     (set-cdr! cur c)
     (map1/itr! c proc (cdr a)))))

(define (map1 proc a)
  (if (null? a)
    '()
    (let ((c (cons (proc (car a)) '())))
     (map1/itr! c proc (cdr a))
     c)))

(define (map proc a . args)
  (if (null? args)
    (map1 proc a)
    (let ((b (car args))
          (bb (cdr args)))
      (if (null? bb)
        (map2 proc a b)
        (let ((c (car bb))
              (cc (cdr bb)))
          (if (null? cc)
            (map3 proc a b c)
            (let ((d (car cc))
                  (dd (cdr cc)))
              (if (null? dd)
                (map4 proc a b c d)
                (error "Too many...")))))))))

(define (for-each4 proc a b c d)
  (unless (or (null? a) (null? b) (null? c) (null? d))
    (proc (car a) (car b) (car c) (car d))
    (for-each4 proc (cdr a) (cdr b) (cdr c) (cdr d))))

(define (for-each3 proc a b c)
  (unless (or (null? a) (null? b) (null? c))
    (proc (car a) (car b) (car c))
    (for-each3 proc (cdr a) (cdr b) (cdr c))))

(define (for-each2 proc a b)
  (unless (or (null? a) (null? b))
    (proc (car a) (car b))
    (for-each2 proc (cdr a) (cdr b))))

(define (for-each1 proc a)
  (unless (null? a)
    (proc (car a))
    (for-each1 proc (cdr a))))

(define (for-each proc a . args)
  (if (null? args)
    (for-each1 proc a)
    (let ((b (car args))
          (bb (cdr args)))
      (if (null? bb)
        (for-each2 proc a b)
        (let ((c (car bb))
              (cc (cdr bb)))
          (if (null? cc)
            (for-each3 proc a b c)
            (let ((d (car cc))
                  (dd (cdr cc)))
              (if (null? dd)
                (for-each4 proc a b c d)
                (error "Too many...")))))))))

;; Take from R7RS small
(define-syntax define-values
  (syntax-rules ()
    ((define-values () expr)
      (define dummy
        (call-with-values (lambda () expr)                
        (lambda args #f))))
    ((define-values (var) expr)
      (define var expr))
    ((define-values (var0 var1 ... varn) expr)
      (begin
        (define var0
          (call-with-values (lambda () expr) list))
        (define var1
          (let ((v (cadr var0)))
            (set-cdr! var0 (cddr var0))
          v)) ...
        (define varn
          (let ((v (cadr var0)))
            (set! var0 (car var0)) v))))
    ((define-values (var0 var1 ... . varn) expr)
     (begin
       (define var0 (call-with-values (lambda () expr) list))
       (define var1 (let ((v (cadr var0))) (set-cdr! var0 (cddr var0)) v)) ...
       (define varn (let ((v (cdr var0))) (set! var0 (car var0)) v))))
    ((define-values var expr)
      (define var (call-with-values (lambda () expr) list)))))
)
