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

    (test* (sub 3 2) 1)
    (test* (subf2 1.0 0.0) 1.0)
    (test*  (subf2 1 0) 1.0)
    (test* (sub3 3 2 -5) 6)
    (test* (string_length "1234567") 7)
    (test* (integer? (return_pointer_string)) #t)
    (test* (pointer->string (return_pointer_string)) "hello")
    (test* (pointer->string (pointer-ref (return_array_of_pointer_string) 0)) "hello")
    (test* (pointer->string (pointer-ref (return_array_of_pointer_string) 1)) "world")

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
           [(zero? (mysql-real-connect mysql-obj "127.0.0.1" "root" "" "mysql" 3306 "/var/run/mysqld/mysqld.sock" 0))
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
                  (test* (string? (pointer->string (pointer-ref record))) #t)
                  (loop (+ i 1) (mysql-fetch-row result))]))
              (mysql-close mysql-obj)
              (mysql-free-result result))])))))))

(test-end)
