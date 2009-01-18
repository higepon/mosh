 (library (mysql)
  (export mysql-init mysql-real-connect NULL mysql-query mysql-store-result
          mysql-num-rows mysql-fetch-row mysql-row-ref mysql-close mysql-free-result mysql-affected-rows
          mysql-get-client-info mysql-autocommit mysql-change-user mysql-character-set-name
          mysql-commit mysql-data-seek mysql-dump-debug-info mysql-errno mysql-error mysql-fetch-field
          mysql-field-name)
  (import (only (rnrs) define guard apply define-syntax syntax-case ... cond lambda syntax else)
          (mosh ffi))

(define NULL 0)

(define-syntax c-function-wrap
  (lambda (x)
    (syntax-case x ()
      [(_ lib more ...)
       #'(cond
          [lib
           (c-function lib more ...)]
          [else
           "libmysqlclient not found"])])))

(define libmysqlclient (guard [c (#t #f)] (open-shared-library "libmysqlclient.so.15.0.0")))


(define %mysql-init        (c-function-wrap libmysqlclient void* mysql_init void*))

;; attempts to establish a connection to a MySQL database engine running on host.
;; .form (real-connect obj host user password db port sock client-flag)
;; .returns obj
(define mysql-real-connect (c-function-wrap libmysqlclient void* mysql_real_connect void* char* char* char* char* int char* int))

;; Executes the SQL statement
;; .form (query obj query)
;; .returns Zero if the statement was successful. Non-zero if an error occurred.
(define mysql-query        (c-function-wrap libmysqlclient void* mysql_query        void* char*))

;; After invoking query, you must call store-result() for every statement that successfully produces a result set (SELECT, SHOW, DESCRIBE, EXPLAIN, CHECK TABLE, and so forth).
;; .form (store-result obj)
;; .returns A MYSQL_RES result structure with the results. NULL (0) if an error occurred.
(define mysql-store-result (c-function-wrap libmysqlclient void* mysql_store_result void*))

;; Returns the number of rows in the result set.
;; .form (num-rows result)
;; .returns the number of rows
(define mysql-num-rows     (c-function-wrap libmysqlclient int mysql_num_rows void*))

;; Retrieves the next row of a result set.
;; .form (fetch-row result)
;; .returns A MYSQL_ROW structure for the next row. NULL if there are no more rows to retrieve or if an error occurred.
(define mysql-fetch-row    (c-function-wrap libmysqlclient void* mysql_fetch_row void*))

;; Retrieves the index field
;; .form (row-ref row . index)
;; .returns Field as string
(define (mysql-row-ref row . index)
  (pointer->string (apply pointer-ref row index)))

;; Closes a previously opened connection
;; .form (close obj)
;; .returns none
(define mysql-close        (c-function-wrap libmysqlclient void mysql_close void*))

;; Frees the memory allocated for a result set
;; .form (free-result result)
;; .returns none
(define mysql-free-result  (c-function-wrap libmysqlclient void mysql_free_result  void*))

;; .form (get-client-info)
;; .returns Returns a string that represents the client library version. 
(define mysql-get-client-info (c-function-wrap libmysqlclient char* mysql_get_client_info))

;; After executing a statement with query returns the number of rows changed (for UPDATE), deleted (for DELETE), or inserted (for INSERT).
;; .form (affected-rows (obj))
;; .returns An integer greater than zero indicates the number of rows affected or retrieved. Zero indicates that no records were updated for an UPDATE statement, no rows matched the WHERE clause in the query or that no query has yet been executed. -1 indicates that the query returned an error or that, for a SELECT query.
(define mysql-affected-rows (c-function-wrap libmysqlclient int mysql_affected_rows void*))

;; Sets autocommit mode on if mode is 1, off if mode is 0. 
;; .form (autocommit mysql-obj 0 or 1)
;; .returns Zero if successful. Non-zero if an error occurred. 
(define mysql-autocommit (c-function-wrap libmysqlclient int mysql_autocommit void* int))

;; Changes the user and causes the database specified by db to become the default (current) database on the connection specified by mysql. In subsequent queries, this database is the default for table references that do not include an explicit database specifier. 
;; .form (change-user mysql-obj user password db)
;; .returns Zero for success. Non-zero if an error occurred.
(define mysql-change-user (c-function-wrap libmysqlclient int mysql_change_user void* char* char* char*))

;; Returns the default character set name for the current connection. 
;; .form (character-set-name mysql-obj)
;; .returns The default character set name 
(define mysql-character-set-name (c-function-wrap libmysqlclient char* mysql_character_set_name void*))

;; Commits the current transaction.
;; .form (commit mysql-obj)
;; .returns Zero if successful. Non-zero if an error occurred.
(define mysql-commit (c-function-wrap libmysqlclient int mysql_commit void*))

;; Seeks to an arbitrary row in a query result set. The offset value is a row number and should be in the range from 0 to (mysql-num-rows result) -1.
;; .form (data-seek mysql-obj offset)
(define mysql-data-seek (c-function-wrap libmysqlclient void mysql_data_seek void* int))

;; Instructs the server to write some debug information to the log. For this to work, the connected user must have the SUPER privileg
;; .form (dump-debug-info mysql-obj)
;; Zero if the command was successful. Non-zero if an error occurred. 
(define mysql-dump-debug-info (c-function-wrap libmysqlclient int mysql_dump_debug_info void*))

;; returns the error code for the most recently invoked API function that can succeed or fail.
;; .form (errno mysql-obj)
(define mysql-errno (c-function-wrap libmysqlclient int mysql_errno void*))

;; Null-terminated string containing the error message for the most recently invoked API function that failed.
;; .form (error mysql-obj)
(define mysql-error (c-function-wrap libmysqlclient char* mysql_error void*))

;; Returns the definition of one column of a result set as a MYSQL_FIELD structure. Call this function repeatedly to retrieve information about all columns in the result set. mysql_fetch_field()  returns NULL when no more fields
;; .form (fetch-field result)
(define mysql-fetch-field (c-function-wrap libmysqlclient void* mysql_fetch_field void*))

;; Returns the field name of field. TODO: this depends on MYSQL_FIELD structure.
;; .form (field-name result)
(define (mysql-field-name field) (pointer->string (pointer-ref field 0)))

;; Initialize MySQL client
;; .form (init)
;; .returns obj
(define (mysql-init) (%mysql-init NULL))













)
