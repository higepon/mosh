(import (rnrs)
        (mosh ffi)
        (mosh)
        (mosh test))

(define (calc-signed-max size-in-byte)
  (- (expt 2 (- (* size-in-byte 8) 1)) 1))

(define (calc-max size-in-byte)
  (- (expt 2 (* size-in-byte 8)) 1))
(define (calc-signed-min size-in-byte)
  (- (expt 2 (- (* size-in-byte 8) 1))))

;; Originally from R6RS Test suite start
(define (good-enough? x y)
  ;; relative error should be with 0.1%, but greater
  ;; relative error is allowed when the expected value
  ;; is near zero.
  (cond ((not (number? x)) #f)
        ((not (number? y)) #f)
        ((or (not (real? x))
             (not (real? y)))
         (and (good-enough? (real-part x) (real-part  y))
              (good-enough? (imag-part x) (imag-part  y))))
        ((infinite? x)
         (=   x (* 2.0 y)))
        ((infinite? y)
         (= (* 2.0 x) y))
        ((nan? y)
         (nan? x))
        ((> (magnitude y) 1e-6)
         (< (/ (magnitude (- x y))
               (magnitude y))
            1e-3))
        (else
         (< (magnitude (- x y)) 1e-6))))
;; Originally from R6RS Test suite end


(when (ffi-supported?)
  (let ()
    (define libffitest (open-shared-library "./libffitest.so.1.0"))

    (define sub (c-function libffitest int sub int int))
    (define sub3 (c-function libffitest int sub3 int int int))
    (define subf2 (c-function libffitest double subf2 double double))
    (define string_length (c-function libffitest int string_length char*))
    (define return_pointer_string (c-function libffitest void* return_pointer_string))
    (define return_pointer_null (c-function libffitest char* return_pointer_null))
    (define return_pointer_string_as_char (c-function libffitest char* return_pointer_string))
    (define return_array_of_pointer_string (c-function libffitest void* return_array_of_pointer_string))
    (define return_struct (c-function libffitest void* return_struct))
    (define return_double_struct (c-function libffitest void* return_double_struct))
    (define return_longlong_struct (c-function libffitest void* return_longlong_struct))
    (define return_uint8_t_struct (c-function libffitest void* return_uint8_t_struct))
    (define return_uint64_t_struct (c-function libffitest void* return_uint64_t_struct))
    (define append_hello (c-function libffitest char* append_hello char*))
    (define struct_ref (c-function libffitest char struct_ref void*))
    (define change_errno (c-function libffitest void change_errno))
    (define abc (c-function libffitest void abc void*))
    (define sub-2 (pointer->c-function (lookup-shared-library libffitest 'sub) 'int 'sub '(int int)))
    (test-equal (sub-2 3 2) 1)

    (test-equal (sub 3 2) 1)
    (test-equal (subf2 1.0 0.0) 1.0)
    (test-equal (sub3 3 2 -5) 6)
    (test-equal (string_length "1234567") 7)
    (test-true  (pointer? (return_pointer_string)))
    (test-equal "hello" (return_pointer_string_as_char))
    (test-true (pointer-null? (return_pointer_null)))
    (test-equal (pointer->string (return_pointer_string)) "hello")
    (test-equal (pointer->string (pointer-ref-c-pointer (return_array_of_pointer_string) 0)) "hello")
    (test-equal (pointer->string (pointer-ref-c-pointer (return_array_of_pointer_string) size-of-pointer)) "world")
    (test-equal "Yeah hello" (append_hello "Yeah "))


   (let ([p (return_struct)])
     (test-eq -1 (struct_ref p)))

    (test-eq -1 (pointer-ref-c-signed-char (return_struct) 0))
    (test-eq 255 (pointer-ref-c-unsigned-char (return_struct) 1))
    (test-true (fl=? 3.14 (pointer-ref-c-double (return_double_struct) 0)))
    (test-equal 123456789123456789 (pointer-ref-c-unsigned-long-long (return_longlong_struct) 0))
    (let ([p (return_struct)])
      (pointer-set-c-char! p 0 127)
      (test-eq 127 (pointer-ref-c-signed-char p 0))
      (test-error assertion-violation? (pointer-set-c-char! p 0 300)))

    (let ([p (return_longlong_struct)])
      (pointer-set-c-long-long! p 0 123456789123456780)
      (test-equal 123456789123456780 (pointer-ref-c-signed-long-long p 0)))

    (let ([p (return_double_struct)])
      (pointer-set-c-double! p 0 1.234)
      (test-true (fl=? 1.234 (pointer-ref-c-double p 0))))

    (let ([p (return_uint8_t_struct)])
      (test-true (pointer=? p p p))
      (pointer-set-c-int8! p 0 127)
      (test-eq 127 (pointer-ref-c-uint8 p 0))
      (test-error assertion-violation? (pointer-set-c-int8! p 0 300)))


    (let ([p (return_uint64_t_struct)])
      (test-true (pointer? p))
      (pointer-set-c-uint8! p 0 255)
      (test-eq 255 (pointer-ref-c-uint8 p 0))
      (pointer-set-c-uint16! p 0 255)
      (test-eq 255 (pointer-ref-c-uint16 p 0))
      (pointer-set-c-uint32! p 0 255)
      (test-eq 255 (pointer-ref-c-uint32 p 0))
      (pointer-set-c-uint64! p 0 255)
      (test-eq 255 (pointer-ref-c-uint64 p 0)))

    ;; min, max, out of range
    (let ([p (return_uint64_t_struct)])
      ;; uint8
      (let* ([ceiling (expt 2 8)]
             [max (- ceiling 1)])
        (pointer-set-c-uint8! p 0 0) ;; min
        (test-eqv 0 (pointer-ref-c-uint8 p 0))
        (pointer-set-c-uint8! p 0 max) ;; max
        (test-eqv max (pointer-ref-c-uint8 p 0))
        (test-error assertion-violation? (pointer-set-c-uint8! p 0 -1)) ;; out of range
        (test-error assertion-violation? (pointer-set-c-uint8! p 0 ceiling)))

      ;; uint16
      (let* ([ceiling (expt 2 16)]
             [max (- ceiling 1)])
        (pointer-set-c-uint16! p 0 0) ;; min
        (test-eqv 0 (pointer-ref-c-uint16 p 0))
        (pointer-set-c-uint16! p 0 max) ;; max
        (test-eqv max (pointer-ref-c-uint16 p 0))
        (test-error assertion-violation? (pointer-set-c-uint16! p 0 -1)) ;; out of range
        (test-error assertion-violation? (pointer-set-c-uint16! p 0 ceiling)))

      ;; uint32
      (let* ([ceiling (expt 2 32)]
             [max (- ceiling 1)])
        (pointer-set-c-uint32! p 0 0) ;; min
        (test-eqv 0 (pointer-ref-c-uint32 p 0))
        (pointer-set-c-uint32! p 0 max) ;; max
        (test-eqv max (pointer-ref-c-uint32 p 0))
        (test-error assertion-violation? (pointer-set-c-uint32! p 0 -1)) ;; out of range
        (test-error assertion-violation? (pointer-set-c-uint32! p 0 ceiling)))

      ;; uint64
      (let* ([ceiling (expt 2 64)]
             [max (- ceiling 1)])
        (pointer-set-c-uint64! p 0 0) ;; min
        (test-eqv 0 (pointer-ref-c-uint64 p 0))
        (pointer-set-c-uint64! p 0 max) ;; max
        (test-eqv max (pointer-ref-c-uint64 p 0))
        (test-error assertion-violation? (pointer-set-c-uint64! p 0 -1)) ;; out of range
        (test-error assertion-violation? (pointer-set-c-uint64! p 0 ceiling)))

      ;; int8
      (let* ([min (* -1 (expt 2 (- 8 1)))]
             [max (- (expt 2 (- 8 1)) 1)])
        (pointer-set-c-int8! p 0 min) ;; min
        (test-eqv min (pointer-ref-c-int8 p 0))
        (pointer-set-c-int8! p 0 max) ;; max
        (test-eqv max (pointer-ref-c-int8 p 0))
        (test-error assertion-violation? (pointer-set-c-int8! p 0 (- min 1))) ;; out of range
        (test-error assertion-violation? (pointer-set-c-int8! p 0 (+ max 1))))

      ;; int16
      (let* ([min (* -1 (expt 2 (- 16 1)))]
             [max (- (expt 2 (- 16 1)) 1)])
        (pointer-set-c-int16! p 0 min) ;; min
        (test-eqv min (pointer-ref-c-int16 p 0))
        (pointer-set-c-int16! p 0 max) ;; max
        (test-eqv max (pointer-ref-c-int16 p 0))
        (test-error assertion-violation? (pointer-set-c-int16! p 0 (- min 1))) ;; out of range
        (test-error assertion-violation? (pointer-set-c-int16! p 0 (+ max 1))))

      ;; int32
      (let* ([min (* -1 (expt 2 (- 32 1)))]
             [max (- (expt 2 (- 32 1)) 1)])
        (pointer-set-c-int32! p 0 min) ;; min
        (test-eqv min (pointer-ref-c-int32 p 0))
        (pointer-set-c-int32! p 0 max) ;; max
        (test-eqv max (pointer-ref-c-int32 p 0))
        (test-error assertion-violation? (pointer-set-c-int32! p 0 (- min 1))) ;; out of range
        (test-error assertion-violation? (pointer-set-c-int32! p 0 (+ max 1))))

      ;; int64
      (let* ([min (* -1 (expt 2 (- 64 1)))]
             [max (- (expt 2 (- 64 1)) 1)])
        (pointer-set-c-int64! p 0 min) ;; min
        (test-eqv min (pointer-ref-c-int64 p 0))
        (pointer-set-c-int64! p 0 max) ;; max
        (test-eqv max (pointer-ref-c-int64 p 0))
        (test-error assertion-violation? (pointer-set-c-int64! p 0 (- min 1))) ;; out of range
        (test-error assertion-violation? (pointer-set-c-int64! p 0 (+ max 1))))
      )
   (begin
     (change_errno)
     (test-eq 3 (shared-errno))
     (shared-errno 4)
     (test-eq 4 (shared-errno)))

   (let ([buffer (make-bytevector 4)]) ;; for abc\0
     (abc buffer)
     (test-eq #\a (integer->char (bytevector-u8-ref buffer 0)))
     (test-eq #\b (integer->char (bytevector-u8-ref buffer 1)))
     (test-eq #\c (integer->char (bytevector-u8-ref buffer 2))))

   (let ([buffer (malloc 4)]) ;; for abc\0
     (abc buffer)
     (test-equal "abc" (pointer->string buffer)))

   (let ([p (malloc 3)])
     (test-true (pointer? p))
     (free p))
   )
  (let ()
    (define libffitest (open-shared-library "./libffitest.so.1.0"))
    (define callCallback (c-function libffitest int callCallback0 void*))
    (let ([callback (make-c-callback-trampoline #x00 "" (lambda () 10))])
      (test-true (pointer? callback))
      (test-eq 10 (callCallback callback))
      (free-c-callback callback)))

  (let ()
    (define libffitest (open-shared-library "./libffitest.so.1.0"))
    (define callCallback (c-function libffitest int callCallback1 void*))
    (let ([callback (make-c-callback-trampoline #x00 "q" (lambda (i) 10))])
      (test-true (pointer? callback))
      (test-eq 10 (callCallback callback))
      (free-c-callback callback)))

  (let ()
    (define libffitest (open-shared-library "./libffitest.so.1.0"))
    (define callCallback (c-function libffitest int callCallback2 void*))
    (let ([callback (make-c-callback-trampoline #x00 "qq" (lambda (i j) (- i j)))])
      (test-true (pointer? callback))
      (test-eq 1 (callCallback callback))
      (free-c-callback callback)))

  (let ()
    (define libffitest (open-shared-library "./libffitest.so.1.0"))
    (define callCallback (c-function libffitest int callCallback3 void*))
    (let ([callback (make-c-callback-trampoline #x00 "dq" (lambda (i j) (- (exact (* i 10)) j)))])
      (test-true (pointer? callback))
      (test-eq 1134 (callCallback callback))
      (free-c-callback callback)))

  (let ()
    (define libffitest (open-shared-library "./libffitest.so.1.0"))
    (define callCallback (c-function libffitest int callCallback3 void*))
    (let ([callback (make-c-callback-trampoline #x00 (make-callback-signature 'callCallback3 'int '(double int) callCallback) (lambda (i j) (- (exact (* i 10)) j)))])
      (test-true (pointer? callback))
      (test-eq 1134 (callCallback callback))
      (free-c-callback callback)))

  (let ()
    (define libffitest (open-shared-library "./libffitest.so.1.0"))
    (define callCallback (c-function libffitest int callCallback3 void*))
    (let ([callback (make-c-callback 'int '(double int) (lambda (i j) (- (exact (* i 10)) j)))])
      (test-true (pointer? callback))
      (test-eq 1134 (callCallback callback))
      (free-c-callback callback)))

  (let ()
    (define libffitest (open-shared-library "./libffitest.so.1.0"))
    (define callCallback (c-function libffitest int callCallback3 void*))
    (let ([callback (c-callback int (double int) (lambda (i j) (- (exact (* i 10)) j)))])
      (test-true (pointer? callback))
      (test-eq 1134 (callCallback callback))
      (free-c-callback callback)))

  (let ()
    (define libffitest (open-shared-library "./libffitest.so.1.0"))
    (define callCallback (c-function libffitest double callCallback4 callback))
    (let ([callback (c-callback double (double double) (lambda (i j) (* i j)))])
      (test-true (pointer? callback))
      (test-equal 1235.0 (callCallback callback))
      (free-c-callback callback)))

;;   (let ()
;;     (define libffitest (open-shared-library "./libffitest.so.1.0"))
;;     (define callCallback (c-function libffitest int64_t callCallback5 void*))
;;     (let ([callback (c-callback int64_t (int64_t) (lambda (i) (+ i 1)))])
;;       (test-true (pointer? callback))
;;       (test-equal #x1234567887654322 (callCallback callback))))

;;   (let ()
;;     (define libffitest (open-shared-library "./libffitest.so.1.0"))
;;     (define callCallback (c-function libffitest int64_t callCallback5 callback))
;;     (let ([callback (c-callback int64_t (int64_t) (lambda (i) (+ i 1)))])
;;       (test-true (pointer? callback))
;;       (test-equal #x1234567887654322 (callCallback callback))))

  (test-equal "ABC" (null-terminated-utf8->string '#vu8(65 66 67 0 65 66 67)))
  (test-equal "ABC" (null-terminated-bytevector->string '#vu8(65 66 67 0 65 66 67) (native-transcoder)))


;
) ;; when

;; Tests of callout return types.
(when (ffi-supported?)
(let ([undef (if #f #t)])
  (define-syntax return-type-test
    (lambda (x)
      (syntax-case x ()
        [(_ expected-value return-type lib name)
         #'(test-equal expected-value ((c-function lib return-type name)))])))
  (define libffitest (open-shared-library "./libffitest.so.1.0"))

  ;; void
  (return-type-test undef void libffitest return_void)

  ;; bool
  (return-type-test #t bool libffitest return_bool_true)
  (return-type-test #f bool libffitest return_bool_false)

  ;; char
  (return-type-test -128  char libffitest return_char_min)
  (return-type-test 127   char libffitest return_char_max)

  ;; size_t
  (return-type-test 0    size_t libffitest return_size_t_min)
  (return-type-test (calc-max size-of-size_t) size_t libffitest return_size_t_max)

  ;; short
  (return-type-test (calc-signed-min size-of-short) short libffitest return_short_min)
  (return-type-test (calc-signed-max size-of-short) short libffitest return_short_max)

  ;; int
  (return-type-test (calc-signed-min size-of-int) int libffitest return_int_min)
  (return-type-test (calc-signed-max size-of-int) int libffitest return_int_max)

  ;; long
  (return-type-test (calc-signed-min size-of-long) long libffitest return_long_min)
  (return-type-test (calc-signed-max size-of-long) long libffitest return_long_max)

  ;; long long
  (return-type-test (calc-signed-min size-of-long-long) long-long libffitest return_long_long_min)
  (return-type-test (calc-signed-max size-of-long-long) long-long libffitest return_long_long_max)

  ;; unsigned short
  (return-type-test 0    unsigned-short libffitest return_unsigned_short_min)
  (return-type-test (calc-max size-of-unsigned-short) unsigned-short libffitest return_unsigned_short_max)

  ;; unsigned int
  (return-type-test 0    unsigned-int libffitest return_unsigned_int_min)
  (return-type-test (calc-max size-of-unsigned-int) unsigned-int libffitest return_unsigned_int_max)

  ;; unsigned long
  (return-type-test 0    unsigned-long libffitest return_unsigned_long_min)
  (return-type-test (calc-max size-of-unsigned-long) unsigned-long libffitest return_unsigned_long_max)

  ;; unsigned long long
  (return-type-test 0    unsigned-long-long libffitest return_unsigned_long_long_min)
  (return-type-test (calc-max size-of-unsigned-long-long) unsigned-long-long libffitest return_unsigned_long_long_max)

  ;; int8_t
  (return-type-test (calc-signed-min 1) int8_t libffitest return_int8_t_min)
  (return-type-test (calc-signed-max 1) int8_t libffitest return_int8_t_max)

  ;; int16_t
  (return-type-test (calc-signed-min 2) int16_t libffitest return_int16_t_min)
  (return-type-test (calc-signed-max 2) int16_t libffitest return_int16_t_max)

  ;; int32_t
  (return-type-test (calc-signed-min 4) int32_t libffitest return_int32_t_min)
  (return-type-test (calc-signed-max 4) int32_t libffitest return_int32_t_max)

  ;; int64_t
  (return-type-test (calc-signed-min 8) int64_t libffitest return_int64_t_min)
  (return-type-test (calc-signed-max 8) int64_t libffitest return_int64_t_max)

  ;; uint8_t
  (return-type-test 0    uint8_t libffitest return_uint8_t_min)
  (return-type-test (calc-max 1) uint8_t libffitest return_uint8_t_max)

  ;; uint16_t
  (return-type-test 0    uint16_t libffitest return_uint16_t_min)
  (return-type-test (calc-max 2) uint16_t libffitest return_uint16_t_max)

  ;; uint32_t
  (return-type-test 0    uint32_t libffitest return_uint32_t_min)
  (return-type-test (calc-max 4) uint32_t libffitest return_uint32_t_max)

  ;; uint64_t
  (return-type-test 0    uint64_t libffitest return_uint64_t_min)
  (return-type-test (calc-max 8) uint64_t libffitest return_uint64_t_max)

  ;; float/double
  (test-true (good-enough? 3.14 ((c-function libffitest float return_float))))
  (test-true (good-enough? 3.14 ((c-function libffitest double return_double))))

  ;; void*
  (let ([p ((c-function libffitest void* return_void_star))])
    (test-true (pointer? p))
    (test-equal #x12345678 (pointer->integer p)))

  ;; char*
  (return-type-test "higepon" char* libffitest return_char_star)
  (let ([p ((c-function libffitest char* return_char_star_null))])
    (test-true (pointer? p))
    (test-true (pointer-null? p)))

)) ;; when

;; Tests of callout argument
(when (ffi-supported?)
  (let [(libffitest (open-shared-library "./libffitest.so.1.0"))]

    (test-equal (calc-signed-max size-of-int)
                ((c-function libffitest int return_first_int_int int int) (calc-signed-max size-of-int) (calc-signed-max size-of-int)))
    (test-equal (calc-signed-max size-of-int)
                ((c-function libffitest int return_second_int_int int int) (calc-signed-max size-of-int) (calc-signed-max size-of-int)))

    (test-equal (calc-signed-max size-of-int)
                ((c-function libffitest int return_first_int_int64_t int int64_t) (calc-signed-max size-of-int) (calc-signed-max 8)))
    (test-equal (calc-signed-max 8)
                ((c-function libffitest int64_t return_second_int_int64_t int int64_t) (calc-signed-max size-of-int) (calc-signed-max 8)))

    (test-equal (calc-signed-max size-of-int)
                ((c-function libffitest int return_first_int_voidstar int void*) (calc-signed-max size-of-int) (integer->pointer (calc-max size-of-unsigned-int))))
    (test-equal (integer->pointer (calc-max size-of-unsigned-int))
                ((c-function libffitest void* return_second_int_voidstar int void*) (calc-signed-max size-of-int) (integer->pointer (calc-max size-of-unsigned-int))))

    (test-equal (calc-signed-max size-of-int)
                ((c-function libffitest int return_first_int_float int float) (calc-signed-max size-of-int) 3.14))
    (test-true (good-enough? 3.14 ((c-function libffitest float return_second_int_float int float) (calc-signed-max size-of-int) 3.14)))

    (test-equal (calc-signed-max size-of-int)
                ((c-function libffitest int return_first_int_double int double) (calc-signed-max size-of-int) 3.14))
    (test-equal 3.14
                ((c-function libffitest double return_second_int_double int double) (calc-signed-max size-of-int) 3.14))

    (test-equal (calc-signed-max size-of-int)
                ((c-function libffitest int return_first_int_charstar int char*) (calc-signed-max size-of-int) "higepon"))
    (test-equal "higepon"
                ((c-function libffitest char* return_second_int_charstar int char*) (calc-signed-max size-of-int) "higepon"))

    (test-equal (calc-signed-max 8)
                ((c-function libffitest int64_t return_first_int64_t_int int64_t int) (calc-signed-max 8) (calc-signed-max size-of-int)))
    (test-equal (calc-signed-max size-of-int)
                ((c-function libffitest int return_second_int64_t_int int64_t int) (calc-signed-max 8) (calc-signed-max size-of-int)))

    (test-equal (calc-signed-max 8)
                ((c-function libffitest int64_t return_first_int64_t_int64_t int64_t int64_t) (calc-signed-max 8) (calc-signed-max 8)))
    (test-equal (calc-signed-max 8)
                ((c-function libffitest int64_t return_second_int64_t_int64_t int64_t int64_t) (calc-signed-max 8) (calc-signed-max 8)))

    (test-equal (calc-signed-max 8)
                ((c-function libffitest int64_t return_first_int64_t_voidstar int64_t void*) (calc-signed-max 8) (integer->pointer (calc-max size-of-unsigned-int))))
    (test-equal (integer->pointer (calc-max size-of-unsigned-int))
                ((c-function libffitest void* return_second_int64_t_voidstar int64_t void*) (calc-signed-max 8) (integer->pointer (calc-max size-of-unsigned-int))))

    (test-equal (calc-signed-max 8)
                ((c-function libffitest int64_t return_first_int64_t_float int64_t float) (calc-signed-max 8) 3.14))
    (test-true (good-enough? 3.14
                             ((c-function libffitest float return_second_int64_t_float int64_t float) (calc-signed-max 8) 3.14)))

    (test-equal (calc-signed-max 8)
                ((c-function libffitest int64_t return_first_int64_t_double int64_t double) (calc-signed-max 8) 3.14))
    (test-equal 3.14
                ((c-function libffitest double return_second_int64_t_double int64_t double) (calc-signed-max 8) 3.14))

    (test-equal (calc-signed-max 8)
                ((c-function libffitest int64_t return_first_int64_t_charstar int64_t char*) (calc-signed-max 8) "higepon"))
    (test-equal "higepon"
                ((c-function libffitest char* return_second_int64_t_charstar int64_t char*) (calc-signed-max 8) "higepon"))

    (test-equal (integer->pointer (calc-max size-of-unsigned-int))
                ((c-function libffitest void* return_first_voidstar_int void* int) (integer->pointer (calc-max size-of-unsigned-int)) (calc-signed-max size-of-int)))
    (test-equal (calc-signed-max size-of-int)
                ((c-function libffitest int return_second_voidstar_int void* int) (integer->pointer (calc-max size-of-unsigned-int)) (calc-signed-max size-of-int)))

    (test-equal (integer->pointer (calc-max size-of-unsigned-int))
                ((c-function libffitest void* return_first_voidstar_int64_t void* int64_t) (integer->pointer (calc-max size-of-unsigned-int)) (calc-signed-max 8)))
    (test-equal (calc-signed-max 8)
                ((c-function libffitest int64_t return_second_voidstar_int64_t void* int64_t) (integer->pointer (calc-max size-of-unsigned-int)) (calc-signed-max 8)))

    (test-equal (integer->pointer (calc-max size-of-unsigned-int))
                ((c-function libffitest void* return_first_voidstar_voidstar void* void*) (integer->pointer (calc-max size-of-unsigned-int)) (integer->pointer (calc-max size-of-unsigned-int))))
    (test-equal (integer->pointer (calc-max size-of-unsigned-int))
                ((c-function libffitest void* return_second_voidstar_voidstar void* void*) (integer->pointer (calc-max size-of-unsigned-int)) (integer->pointer (calc-max size-of-unsigned-int))))

    (test-equal (integer->pointer (calc-max size-of-unsigned-int))
                ((c-function libffitest void* return_first_voidstar_float void* float) (integer->pointer (calc-max size-of-unsigned-int)) 3.14))
    (test-true (good-enough? 3.14
                             ((c-function libffitest float return_second_voidstar_float void* float) (integer->pointer (calc-max size-of-unsigned-int)) 3.14)))

    (test-equal (integer->pointer (calc-max size-of-unsigned-int))
                ((c-function libffitest void* return_first_voidstar_double void* double) (integer->pointer (calc-max size-of-unsigned-int)) 3.14))
    (test-equal 3.14
                ((c-function libffitest double return_second_voidstar_double void* double) (integer->pointer (calc-max size-of-unsigned-int)) 3.14))

    (test-equal (integer->pointer (calc-max size-of-unsigned-int))
                ((c-function libffitest void* return_first_voidstar_charstar void* char*) (integer->pointer (calc-max size-of-unsigned-int)) "higepon"))
    (test-equal "higepon"
                ((c-function libffitest char* return_second_voidstar_charstar void* char*) (integer->pointer (calc-max size-of-unsigned-int)) "higepon"))

    (test-true (good-enough? 3.14
                             ((c-function libffitest float return_first_float_int float int) 3.14 (calc-signed-max size-of-int))))
    (test-equal (calc-signed-max size-of-int)
                ((c-function libffitest int return_second_float_int float int) 3.14 (calc-signed-max size-of-int)))

    (test-true (good-enough? 3.14
                             ((c-function libffitest float return_first_float_int64_t float int64_t) 3.14 (calc-signed-max 8))))
    (test-equal (calc-signed-max 8)
                ((c-function libffitest int64_t return_second_float_int64_t float int64_t) 3.14 (calc-signed-max 8)))

    (test-true (good-enough? 3.14
                             ((c-function libffitest float return_first_float_voidstar float void*) 3.14 (integer->pointer (calc-max size-of-unsigned-int)))))
    (test-equal (integer->pointer (calc-max size-of-unsigned-int))
                ((c-function libffitest void* return_second_float_voidstar float void*) 3.14 (integer->pointer (calc-max size-of-unsigned-int))))

    (test-true (good-enough? 3.14
                             ((c-function libffitest float return_first_float_float float float) 3.14 3.14)))
    (test-true (good-enough? 3.14
                             ((c-function libffitest float return_second_float_float float float) 3.14 3.14)))

    (test-true (good-enough? 3.14
                             ((c-function libffitest float return_first_float_double float double) 3.14 3.14)))
    (test-equal 3.14
                ((c-function libffitest double return_second_float_double float double) 3.14 3.14))

    (test-true (good-enough? 3.14
                             ((c-function libffitest float return_first_float_charstar float char*) 3.14 "higepon")))
    (test-equal "higepon"
                ((c-function libffitest char* return_second_float_charstar float char*) 3.14 "higepon"))

    (test-equal 3.14
                ((c-function libffitest double return_first_double_int double int) 3.14 (calc-signed-max size-of-int)))
    (test-equal (calc-signed-max size-of-int)
                ((c-function libffitest int return_second_double_int double int) 3.14 (calc-signed-max size-of-int)))

    (test-equal 3.14
                ((c-function libffitest double return_first_double_int64_t double int64_t) 3.14 (calc-signed-max 8)))
    (test-equal (calc-signed-max 8)
                ((c-function libffitest int64_t return_second_double_int64_t double int64_t) 3.14 (calc-signed-max 8)))

    (test-equal 3.14
                ((c-function libffitest double return_first_double_voidstar double void*) 3.14 (integer->pointer (calc-max size-of-unsigned-int))))
    (test-equal (integer->pointer (calc-max size-of-unsigned-int))
                ((c-function libffitest void* return_second_double_voidstar double void*) 3.14 (integer->pointer (calc-max size-of-unsigned-int))))

    (test-equal 3.14
                ((c-function libffitest double return_first_double_float double float) 3.14 3.14))
    (test-true (good-enough? 3.14
                             ((c-function libffitest float return_second_double_float double float) 3.14 3.14)))

    (test-equal 3.14
                ((c-function libffitest double return_first_double_double double double) 3.14 3.14))
    (test-equal 3.14
                ((c-function libffitest double return_second_double_double double double) 3.14 3.14))

    (test-equal 3.14
                ((c-function libffitest double return_first_double_charstar double char*) 3.14 "higepon"))
    (test-equal "higepon"
                ((c-function libffitest char* return_second_double_charstar double char*) 3.14 "higepon"))

    (test-equal "higepon"
                ((c-function libffitest char* return_first_charstar_int char* int) "higepon" (calc-signed-max size-of-int)))
    (test-equal (calc-signed-max size-of-int)
                ((c-function libffitest int return_second_charstar_int char* int) "higepon" (calc-signed-max size-of-int)))

    (test-equal "higepon"
                ((c-function libffitest char* return_first_charstar_int64_t char* int64_t) "higepon" (calc-signed-max 8)))
    (test-equal (calc-signed-max 8)
                ((c-function libffitest int64_t return_second_charstar_int64_t char* int64_t) "higepon" (calc-signed-max 8)))

    (test-equal "higepon"
                ((c-function libffitest char* return_first_charstar_voidstar char* void*) "higepon" (integer->pointer (calc-max size-of-unsigned-int))))
    (test-equal (integer->pointer (calc-max size-of-unsigned-int))
                ((c-function libffitest void* return_second_charstar_voidstar char* void*) "higepon" (integer->pointer (calc-max size-of-unsigned-int))))

    (test-equal "higepon"
                ((c-function libffitest char* return_first_charstar_float char* float) "higepon" 3.14))
    (test-true (good-enough? 3.14
                             ((c-function libffitest float return_second_charstar_float char* float) "higepon" 3.14)))

    (test-equal "higepon"
                ((c-function libffitest char* return_first_charstar_double char* double) "higepon" 3.14))
    (test-equal 3.14
                ((c-function libffitest double return_second_charstar_double char* double) "higepon" 3.14))

    (test-equal "higepon"
                ((c-function libffitest char* return_first_charstar_charstar char* char*) "higepon" "higepon"))
    (test-equal "higepon"
                ((c-function libffitest char* return_second_charstar_charstar char* char*) "higepon" "higepon"))

    ;; invalid argument
    (test-error assertion-violation?
                ((c-function libffitest double return_first_double_double double double) 4 3.0))

    (test-equal (* (calc-signed-max size-of-short) 2)
                ((c-function libffitest int add_short_short short short) (calc-signed-max size-of-short) (calc-signed-max size-of-short)))

    (test-false ((c-function libffitest bool return_not bool) #t))
    (test-true  ((c-function libffitest bool return_not bool) #f))

    ;; amd64: stack argument
    (test-equal 36 ((c-function libffitest int add8 int int int int int int int int) 1 2 3 4 5 6 7 8))

    (test-equal 5.0 ((c-function libffitest double double10_2 double double double double double double double double double double)
                     10.0 9.0 8.0 7.0 6.0 5.0 4.0 3.0 2.0 1.0))

))

(test-results)
