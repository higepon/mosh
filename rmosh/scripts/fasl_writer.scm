;; Write Rust VM Ops as binary.
(import (match))
(import (mosh control))
(import (mosh test))
(import (mosh file))
(import (only (mosh) format))
(import (only (rnrs) set! bytevector-s64-native-set! bytevector-u32-native-set! bytevector-u16-native-set! open-bytevector-output-port put-u8 put-bytevector))
(import (only (rnrs) open-string-output-port string-titlecase bytevector->u8-list command-line))
(import (rnrs arithmetic flonums))
(import (only (rnrs r5rs) modulo))
(import (only (srfi :1) take drop))
(import (only (srfi :13) string-delete string-join))
(import (rnrs arithmetic fixnums))
(import (scheme base))
(import (scheme case-lambda))
(import (scheme file))
(import (scheme process-context))
(import (scheme read))
(import (scheme write))

(define TAG_FIXNUM 0)
(define TAG_TRUE   1)
(define TAG_FALSE  2)
(define TAG_NIL    3)
(define TAG_CHAR   4)
(define TAG_SYMBOL 5)
(define TAG_STRING 6)
(define TAG_PAIR   7)
(define TAG_VECTOR 8)
(define TAG_COMPILER_INSN 9)

(define (put-s64 port n)
  (let1 bv (make-bytevector 8)
     (bytevector-s64-native-set! bv 0 n)
     (put-bytevector port bv)))

(define (put-u32 port n)
  (let1 bv (make-bytevector 4)
     (bytevector-u32-native-set! bv 0 n)
     (put-bytevector port bv)))

(define (put-u16 port n)
  (let1 bv (make-bytevector 2)
     (bytevector-u16-native-set! bv 0 n)
     (put-bytevector port bv)))

(define write-sexp
  (case-lambda
    [(port c)
      (match c
        [('*insn* (? number? op))
          (put-u8 port TAG_COMPILER_INSN)
          (put-u8 port op)]      
        [('*compiler-insn* (? number? op))
          (put-u8 port TAG_COMPILER_INSN)
          (put-u8 port op)]
        [(? char? c)
          (put-u8 port TAG_CHAR)
          (put-u32 port (char->integer c))]
        [(? fixnum? n)
          (put-u8 port TAG_FIXNUM)
          (put-s64 port n)]
        [(? symbol? s)
          (put-u8 port TAG_SYMBOL)
          (let1 str (symbol->string s)
            (put-u16 port (string-length str))
            (for-each
              (lambda (c)
                (put-u32 port (char->integer c)))
              (string->list str)))]
        [(? string? str)
          (put-u8 port TAG_STRING)
          (put-u16 port (string-length str))
          (for-each
            (lambda (c)
              (put-u32 port (char->integer c)))
            (string->list str))]
        [(first . second)
          (put-u8 port TAG_PAIR)
          (write-sexp port first)
          (write-sexp port second)]
        [#t
          (put-u8 port TAG_TRUE)]
        [#f
          (put-u8 port TAG_FALSE)]
        [()
          (put-u8 port TAG_NIL)]
        [(? vector? v)
          (put-u8 port TAG_VECTOR)
          (put-u16 port (vector-length v))
          (for-each
            (lambda (o)
              (write-sexp port o))
            (vector->list v))]
        [(? flonum? f)
          ;; TODO
          (write-sexp port 0)]
        [any
          (error (format "unknown sexp = ~a" any))]
      )]
    [(c)
      (let-values ([(p get) (open-bytevector-output-port)])
        (write-sexp p c)
        (get))]))

(define (run-all-tests)
  (test-equal #vu8(0 3 0 0 0 0 0 0 0) (write-sexp 3))
  (test-equal #vu8(1) (write-sexp #t))
  (test-equal #vu8(2) (write-sexp #f))
  (test-equal #vu8(3) (write-sexp '()))
  (test-equal #vu8(4 97 0 0 0) (write-sexp #\a))
  (test-equal #vu8(5 5 0 104 0 0 0 101 0 0 0 108 0 0 0 108 0 0 0 111 0 0 0) (write-sexp 'hello))
  (test-equal #vu8(6 3 0 97 0 0 0 98 0 0 0 99 0 0 0) (write-sexp "abc"))
  (test-equal #vu8(7 5 1 0 97 0 0 0 3) (write-sexp '(a)))
  (test-results))

(define rewrite-insn*
  (case-lambda
   [(all-insn* insn*)
    (let-values ([(port get) (open-bytevector-output-port)])
      (rewrite-insn* all-insn* insn* 0 port)
      (let* ([u8* (get)]
             [u8* (bytevector->u8-list u8*)])
        u8*))]
   [(all-insn* insn* idx port)
     (for-each (lambda (sexp) (write-sexp port sexp)) insn*)]))

(define (for-each-with-index proc lst)
  (do ((i 1 (+ i 1)) ; start with 1
       (lst lst (cdr lst)))
      ((null? lst))
    (proc i (car lst))))  

(define (main args)
  (match args
    [(_ "tests")
      (run-all-tests)]
    [else
      (let-values ([(port get) (open-string-output-port)])
        (let* ([op-file (cadr args)]
               [sexp* (file->sexp-list op-file)]
               [insn* (vector->list (car sexp*))]
               [u8* (rewrite-insn* insn* insn*)])
          (display "pub static BIN_COMPILER: &[u8] = &[\n")
          (for-each-with-index
            (lambda (i u8)
              (format #t "~a," u8)
              (if (and (not (zero? i)) (zero? (modulo i 50)))
                (newline)))
            u8*)
          (display "];\n")))]))

(main (command-line))
