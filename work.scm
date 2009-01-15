(import (rnrs)
        (system)
        (mosh ffi)
        (mosh test))

(define libffitest (open-shared-library "./libffitest.so.1.0"))

(define sub (c-function libffitest int sub int int))
(define sub3 (c-function libffitest int sub3 int int int))
(define string_length (c-function libffitest int string_length char*))
(define return_pointer_string (c-function libffitest int return_pointer_string))
(define return_array_of_pointer_string (c-function libffitest void* return_array_of_pointer_string))

(test* (sub 3 2) 1)
(test* (sub3 3 2 -5) 6)
(test* (string_length "1234567") 7)
(test* (integer? (return_pointer_string)) #t)
(test* (pointer->string (return_pointer_string)) "hello")
(test* (pointer->string (pointer-ref (return_array_of_pointer_string) 0)) "hello")
(test* (pointer->string (pointer-ref (return_array_of_pointer_string) 1)) "world")


;; (define libmysqlclient     (%ffi-open "libmysqlclient.so.15.0.0"))
;; (define mysql-init         (make-c-function libmysqlclient 'void* "mysql_init"         '(void*)))
;; (define mysql-real-connect (make-c-function libmysqlclient 'void* "mysql_real_connect" '(void* char* char* char* char* int char* int)))
;; (define mysql-query        (make-c-function libmysqlclient 'void* "mysql_query"        '(void* char*)))
;; (define mysql-store-result (make-c-function libmysqlclient 'void* "mysql_store_result" '(void*)))
;; (define mysql-num-rows     (make-c-function libmysqlclient 'int   "mysql_num_rows"     '(void*)))
;; (define mysql-fetch-row    (make-c-function libmysqlclient 'void* "mysql_fetch_row"    '(void*)))
;; (define NULL 0)

;; (let ([mysql-obj (mysql-init NULL)])
;;   (mysql-real-connect mysql-obj "127.0.0.1" "root" "" "mysql" 3306 "/var/run/mysqld/mysqld.sock" 0)
;;   (mysql-query mysql-obj "select User from user;")
;;   (let* ([result (mysql-store-result mysql-obj)]
;;          [count  (mysql-num-rows result)])
;;     (let loop ([i 0]
;;                [record (mysql-fetch-row result)])
;;       (if (= i count)
;;           (display "====================\n")
;;           (begin
;;             (format #t "=> ~a\n" (%ffi-void*->string (%ffi-pointer-ref record)))
;;             (loop (+ i 1) (mysql-fetch-row result)))))
;;     ;; TODO:後片付け
;;     ))

