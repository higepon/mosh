(import (rnrs)
        (system)
        (mosh))

(define (alist->eq-hash-table alist)
  (let ([hashtable (make-eq-hashtable)])
    (for-each (lambda (x) (hashtable-set! hashtable (car x) (cdr x)))
              alist)
    hashtable))

(define stub-ht (alist->eq-hash-table (list (cons 'void* %ffi-call->void*)
                                            (cons 'int %ffi-call->int))))
(define checker-ht (alist->eq-hash-table (list (cons 'void* integer?) (cons 'int integer?) (cons 'char* string?)
                                              )))

(define (make-c-function lib ret-type name arg-types)
  (let ([func (%ffi-lookup lib name)]
        [stub (hashtable-ref stub-ht ret-type #f)]
        [checkers (map (lambda (type) (hashtable-ref checker-ht type #f)) arg-types)])
    (unless stub
      (assertion-violation 'make-c-function "wrong ret type" ret-type))
    (unless func
      (assertion-violation 'make-c-function "c-function not found" name))
    (lambda args
      (unless (= (length arg-types) (length args))
        (assertion-violation name "wrong arguments number" args))
      (for-each
       (lambda (checker arg)
         (unless (checker arg)
           (assertion-violation name "wrong argument " arg))
         )
       checkers
       args)
      (apply stub func args))))

(define libmysqlclient     (%ffi-open "libmysqlclient.so.15.0.0"))
(define mysql-init         (make-c-function libmysqlclient 'void* "mysql_init"         '(void*)))
(define mysql-real-connect (make-c-function libmysqlclient 'void* "mysql_real_connect" '(void* char* char* char* char* int char* int)))
(define mysql-query        (make-c-function libmysqlclient 'void* "mysql_query"        '(void* char*)))
(define mysql-store-result (make-c-function libmysqlclient 'void* "mysql_store_result" '(void*)))
(define mysql-num-rows     (make-c-function libmysqlclient 'int   "mysql_num_rows"     '(void*)))
(define mysql-fetch-row    (make-c-function libmysqlclient 'void* "mysql_fetch_row"    '(void*)))
(define NULL 0)

(let* ([handle (%ffi-open "libmysqlclient.so.15.0.0")]
       [mysql-obj (mysql-init NULL)])
  (mysql-real-connect mysql-obj "127.0.0.1" "root" "" "mysql" 3306 "/var/run/mysqld/mysqld.sock" 0)
  (mysql-query mysql-obj "select User from user;")
  (let* ([result (mysql-store-result mysql-obj)]
         [count  (mysql-num-rows result)])
    (let loop ([i 0]
               [record (mysql-fetch-row result)])
      (if (= i count)
          (display "====================\n")
          (begin
            (format #t "=> ~a\n" (%ffi-void*->string (%ffi-pointer-ref record)))
            (loop (+ i 1) (mysql-fetch-row result)))))
    ;; TODO:後片付け
    ))

