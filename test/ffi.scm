(import (rnrs)
        (mosh ffi)
        (mosh)
        (mosh test))


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
    (test-equal  (subf2 1 0) 1.0)
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

   (let ()
      (define libmysqlclient (guard [c (#t #f)] (open-shared-library "libmysqlclient.so.15.0.0")))
    (when libmysqlclient
      (let ()
        (define NULL pointer-null)
        (define mysql-init         (c-function libmysqlclient void* mysql_init         void*))
        (define mysql-real-connect (c-function libmysqlclient void* mysql_real_connect void* char* char* char* char* int char* int))
        (define mysql-query        (c-function libmysqlclient void* mysql_query        void* char*))
        (define mysql-store-result (c-function libmysqlclient void* mysql_store_result void*))
        (define mysql-num-rows     (c-function libmysqlclient int   mysql_num_rows     void*))
        (define mysql-fetch-row    (c-function libmysqlclient void* mysql_fetch_row    void*))
        (define mysql-close        (c-function libmysqlclient void* mysql_close        void*))
        (define mysql-free-result  (c-function libmysqlclient void* mysql_free_result  void*))
        (let ([mysql-obj (mysql-init NULL)])
          (cond
           [(pointer-null? (mysql-real-connect mysql-obj "127.0.0.1" "root" "root" "mysql" 3306 "/var/run/mysqld/mysqld.sock" 0))
            (display "mysql connect failed\n" (current-error-port))]
           [else
            (mysql-query mysql-obj "select User from user;")
            (let* ([result (mysql-store-result mysql-obj)]
                   [count  (mysql-num-rows result)])
              (let loop ([i 0]
                         [record (mysql-fetch-row result)])
                (cond
                 [(= i count) '()]
                 [else
                  (test-true  (string? (pointer->string (pointer-ref-c-pointer record 0))))
                  (loop (+ i 1) (mysql-fetch-row result))]))
              (mysql-close mysql-obj)
              (mysql-free-result result))])))))
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

(test-results)
