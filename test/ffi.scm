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
    (define return_pointer_string (c-function libffitest int return_pointer_string))
    (define return_array_of_pointer_string (c-function libffitest void* return_array_of_pointer_string))
    (define return_struct (c-function libffitest void* return_struct))
    (define return_double_struct (c-function libffitest void* return_double_struct))
    (define return_longlong_struct (c-function libffitest void* return_longlong_struct))
    (define return_uint8_t_struct (c-function libffitest void* return_uint8_t_struct))

    (test-equal (sub 3 2) 1)
    (test-equal (subf2 1.0 0.0) 1.0)
    (test-equal  (subf2 1 0) 1.0)
    (test-equal (sub3 3 2 -5) 6)
    (test-equal (string_length "1234567") 7)
    (test-true  (integer? (return_pointer_string)))
    (test-equal (pointer->string (return_pointer_string)) "hello")
    (test-equal (pointer->string (pointer->integer (pointer-ref-c-pointer (integer->pointer (return_array_of_pointer_string)) 0))) "hello")
    (test-equal (pointer->string (pointer->integer (pointer-ref-c-pointer (integer->pointer (return_array_of_pointer_string)) 1))) "world")

    (test-eq -1 (pointer-ref-c-signed-char (integer->pointer (return_struct)) 0))
    (test-eq 255 (pointer-ref-c-unsigned-char (integer->pointer (return_struct)) 1))
    (test-true (fl=? 3.14 (pointer-ref-c-double (integer->pointer (return_double_struct)) 0)))
    (test-equal 123456789123456789 (pointer-ref-c-unsigned-long-long (integer->pointer (return_longlong_struct)) 0))
    (let ([p (integer->pointer (return_struct))])
      (pointer-set-c-char! p 0 127)
      (test-eq 127 (pointer-ref-c-signed-char p 0))
      (test-error assertion-violation? (pointer-set-c-char! p 0 300)))

    (let ([p (integer->pointer (return_longlong_struct))])
      (pointer-set-c-long-long! p 0 123456789123456780)
      (test-equal 123456789123456780 (pointer-ref-c-signed-long-long p 0)))

    (let ([p (integer->pointer (return_double_struct))])
      (pointer-set-c-double! p 0 1.234)
      (fl=? 1.234 (pointer-ref-c-double p 0)))

    (let ([p (integer->pointer (return_uint8_t_struct))])
      (test-true (pointer=? p p p))
      (pointer-set-c-int8! p 0 127)
      (test-eq 127 (pointer-ref-c-uint8 p 0))
      (test-error assertion-violation? (pointer-set-c-int8! p 0 300)))


    (let ()
      (define libmysqlclient (guard [c (#t #f)] (open-shared-library "libmysqlclient.so.15.0.0")))
    (when libmysqlclient
      (let ()
        (define NULL 0)
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
           [(zero? (mysql-real-connect mysql-obj "127.0.0.1" "root" "root" "mysql" 3306 "/var/run/mysqld/mysqld.sock" 0))
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
                  (test-true  (string? (pointer->string (pointer->integer (pointer-ref-c-pointer (integer->pointer record) 0)))))
                  (loop (+ i 1) (mysql-fetch-row result))]))
              (mysql-close mysql-obj)
              (mysql-free-result result))])))))))

(test-results)
