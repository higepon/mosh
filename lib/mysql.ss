 (library (mysql)
  (export mysql-init mysql-real-connect NULL mysql-query mysql-store-result
          mysql-num-rows mysql-fetch-row mysql-row-ref mysql-close mysql-free-result mysql-affected-rows
          mysql-get-client-info mysql-autocommit mysql-change-user mysql-character-set-name
          mysql-commit mysql-data-seek mysql-dump-debug-info mysql-errno mysql-error mysql-fetch-field
          mysql-field-name mysql-fetch-field-direct mysql-fetch-fields mysql-fetch-lengths
          mysql-field-count mysql-field-seek mysql-field-tell mysql-get-client-version mysql-get-host-info
          mysql-get-proto-info mysql-get-server-info mysql-get-server-version mysql-get-ssl-cipher
          mysql-hex-string mysql-info mysql-insert-id mysql-library-end mysql-library-init
          mysql-list-dbs mysql-list-processes mysql-list-tables mysql-more-results mysql-next-result
          mysql-num-fields mysql-options mysql-ping mysql-real-escape-string mysql-real-query
          mysql-refresh mysql-reload
          )
  (import (only (rnrs) define guard apply define-syntax syntax-case ... cond lambda syntax else set!)
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

;; Given a field number fieldnr for a column within a result set, returns that column's field definition as a MYSQL_FIELD structure.
;; .form (mysql-fetch-field-direct result fieldnr)
;; .returns The MYSQL_FIELD structure for the specified column.
(define mysql-fetch-field-direct (c-function-wrap libmysqlclient void* mysql_fetch_field_direct void* int))

;; Returns an array of all MYSQL_FIELD  structures for a result set. Each structure provides the field definition for one column of the result set.
;; .form (mysql-fetch-fields result)
;; .returns An array of MYSQL_FIELD structures for all columns of a result set.
(define mysql-fetch-fields (c-function-wrap libmysqlclient void* mysql_fetch_fields void*))

;; Returns the lengths of the columns of the current row within a result set. If you plan to copy field values, this length information is also useful for optimization, because you can avoid calling strlen().
;; .form (mysql-fetch-fields result)
;; .returns An array of unsigned long integers representing the size of each column (not including any terminating null characters). NULL if an error occurred.
(define mysql-fetch-lengths (c-function-wrap libmysqlclient void* mysql_fetch_lengths void*))

;; Returns the number of columns for the most recent query on the connection.
;; .form (mysql-field-count mysql-obj)
;; .returns An unsigned integer representing the number of columns in a result set.
(define mysql-field-count (c-function-wrap libmysqlclient int mysql_field_count void*))

;;  Sets the field cursor to the given offset. The next call to mysql-fetch-field retrieves the field definition of the column associated with that offset.
;;  To seek to the beginning of a row, pass an offset value of zero.
;; .form (mysql-field-seek result offset)
;; .returns The previous value of the field cursor.
(define mysql-field-seek (c-function-wrap libmysqlclient int mysql_field_seek void* int))

;; Returns the position of the field cursor used for the last
;; .form (mysql-field-tell result)
;; .returns The current offset of the field cursor.
(define mysql-field-tell (c-function-wrap libmysqlclient int mysql_field_tell void*))

;; not supported
;;(define mysql-get-character-set-info )


;; Returns an integer that represents the client library version.
;; .form ((mysql-get-client-info)
;; .returns Returns an integer that represents the client library version.
(define mysql-get-client-version (c-function-wrap libmysqlclient int mysql_get_client_version))

;; Returns a string describing the type of connection in use, including the server host name.
;; .form (mysql-get-host-info mysql-obj)
(define mysql-get-host-info (c-function-wrap libmysqlclient char* mysql_get_host_info void*))

;; Returns the protocol version used by current connection.
;; .form (mysql-get-proto-info mysql-obj)
;; .returns An unsigned integer representing the protocol version used by the current connection.
(define mysql-get-proto-info (c-function-wrap libmysqlclient int mysql_get_proto_info void*))

;; Returns a string that represents the server version number.
;; .form (mysql-get-server-info mysql-obj)
;; .returns A character string that represents the server version number.
(define mysql-get-server-info (c-function-wrap libmysqlclient char* mysql_get_server_info void*))

;; Returns the version number of the server as an integer.
;; .form (mysql-get-srever-version mysql-obj)
;; .returns A number that represents the MySQL server.
(define mysql-get-server-version (c-function-wrap libmysqlclient int mysql_get_server_version void*))

;; mysql-get-ssl-cipher returns the SSL cipher used for the given connection to the server. mysql is the connection handler returned from mysql-init.
;; .form (mysql-get-ssl-cipher mysql-obj)
;; .returns A string naming the SSL cipher used for the connection, or NULL if no cipher is being used.
(define mysql-get-ssl-cipher (c-function-wrap libmysqlclient char* mysql_get_ssl_cipher void*))

;; This function is used to create a legal SQL string that you can use in an SQL statement. See Section 8.1.1, "Strings" on MySQL Manual.
;; .form (mysql-hex-string bv-to str-from len)
;; .returns The length of the value placed into to, not including the terminating null character.
(define mysql-hex-string (c-function-wrap libmysqlclient int mysql_hex_string char* char* int))

;; Retrieves a string providing information about the most recently executed statement, but only for the statements listed here.
;; .form (mysql-info mysql-obj)
;; .returns A character string representing additional information about the most recently executed statement. NULL if no information is available for the statement.
(define mysql-info (c-function-wrap libmysqlclient char* mysql_info void*))

;; Returns the value generated for an AUTO_INCREMENT column by the previous INSERT or UPDATE statement. TODO: May overflow?
;; .form (mysql-insert-id mysql-obj)
;; .returns Described in the preceding discussion.
(define mysql-insert-id (c-function-wrap libmysqlclient void* mysql_insert_id void*)) ;; use void* as return value

;; Initialize MySQL client
;; .form (init)
;; .returns obj
(define (mysql-init) (%mysql-init NULL))

;; This function finalizes the MySQL library. You should call it when you are done using the library (for example, after disconnecting from the server).This function was added in MySQL 5.0.3.
;; .form (mysql-library-end)
;; .returns #f if client doen't supported this function
(define mysql-library-end (guard (c [#t (lambda x #f)])
                               (c-function-wrap libmysqlclient void mysql_library_end)))

;; This function should be called to initialize the MySQL library before you call any other MySQL function, whether your application is a regular client program or uses the embedded server. In a non-multi-threaded environment, the call to mysql_library_init() may be omitted, because mysql_init()  will invoke it automatically as necessary.
;; .form (mysql-library-init argc argv groups)
;; .returns Zero if successful. Non-zero if an error occurred.
(define mysql-library-init (guard (c [#t (lambda x #f)])
                               (c-function-wrap libmysqlclient int mysql_library_init int void* void*)))


;; Returns a result set consisting of database names on the server that match the simple regular expression specified by the wild parameter. wild may contain the wildcard characters “%” or “_”, or may be a NULL pointer to match all databases. Calling mysql-list-dbs is similar to executing the query SHOW DATABASES [LIKE wild].
;; .form (mysql-list-dbs mysql-obj wild)
;; .returns A MYSQL_RES result set for success. NULL if an error occurred.
(define mysql-list-dbs (c-function-wrap libmysqlclient void* mysql_list_dbs void* char*))

;; Returns a result set describing the current server threads.
;; .form (mysql-list-processes mysql-obj)
;; .returns A MYSQL_RES result set for success. NULL if an error occurred.
(define mysql-list-processes (c-function-wrap libmysqlclient void* mysql_list_processes void*))

;; Returns a result set consisting of table names in the current database that match the simple regular expression specified by the wild parameter. wild  may contain the wildcard characters “%” or “_”, or may be a NULL pointer to match all tables. 
;; .form (mysql-list-tables mysql-obj wild)
;; .returns A MYSQL_RES result set for success. NULL if an error occurred.
(define mysql-list-tables (c-function-wrap libmysqlclient void* mysql_list_tables void* char*))

;; This function is used when you execute multiple statements specified as a single statement string, or when you execute CALL statements, which can return multiple result sets.
;; .form (mysql-more-results mysql-obj)
;; .returns TRUE (1) if more results exist. FALSE (0) if no more results exist.
(define mysql-more-results (c-function-wrap libmysqlclient int mysql_more_results void*))

;; If more statement results exist, mysql_next_result() reads the next statement result and returns the status back to the application. I
;; .form (mysql-next-result mysql-obj)
;; .returns 0:Successful and there are more results, -1:Successful and there are no more results, >0:An error occurred
(define mysql-next-result (c-function-wrap libmysqlclient int mysql_next_result void*))

;; Returns the number of columns in a result set. 
;; .form (mysql-num-fields result)
;; .returns An unsigned integer representing the number of columns in a result set.
(define mysql-num-fields (c-function-wrap libmysqlclient int mysql_num_fields void*))

;; Can be used to set extra connect options and affect behavior for a connection. This function may be called multiple times to set several options.
;; .form (mysql-options mysql-obj option arg)
;; .returns Zero for success. Non-zero if you specify an unknown option.
(define mysql-options (c-function-wrap libmysqlclient int mysql_options void* int char*))

;; Checks whether the connection to the server is working. If the connection has gone down and auto-reconnect is enabled an attempt to reconnect is made.
;; .form (mysql-ping mysql-obj)
;; .returns Zero if the connection to the server is alive. Non-zero if an error occurred.
(define mysql-ping (c-function-wrap libmysqlclient int mysql_ping void*))

;; This function is used to create a legal SQL string that you can use in an SQL statement. The string in from is encoded to an escaped SQL string, taking into account the current character set of the connection. The result is placed in to and a terminating null byte is appended. Characters encoded are NUL (ASCII 0), “\n”, “\r”, “\”, “'”, “"”, and Control-Z (see Section 8.1, “Literal Values”). (Strictly speaking, MySQL requires only that backslash and the quote character used to quote the string in the query be escaped. This function quotes the other characters to make them easier to read in log files.)
;; .form (mysql-real-escape-string mysql-obj to-bv from len)
;; .returns The length of the value placed into to, not including the terminating null character.
(define mysql-real-escape-string (c-function-wrap libmysqlclient int mysql_real_escape_string void* char* char* int))

;; Executes the SQL statement pointed to by stmt_str, which should be a string length bytes long.
;; .form (mysql-real-query mysql-obj stmt int)
;; .returns Zero if the statement was successful. Non-zero if an error occurred.
(define mysql-real-query (c-function-wrap libmysqlclient int mysql_real_query void* char* int))

;; This function flushes tables or caches, or resets replication server information. The connected user must have the RELOAD privilege.
;; .form (mysql-refresh mysql-obj options)
;; .returns
(define mysql-refresh (c-function-wrap libmysqlclient int mysql_refresh void* int))

;;  Asks the MySQL server to reload the grant tables. The connected user must have the RELOAD  privilege. This function is deprecated. It is preferable to use mysql_query() to issue an SQL FLUSH PRIVILEGES statement instead.
;; .form (mysql-reload mysql-obj)
;; .returns Zero for success. Non-zero if an error occurred. 
(define mysql-reload (guard (c [#t (lambda x #f)])
                               (c-function-wrap libmysqlclient int mysql_reload void*)))
;; ;;
;; ;; .form ()
;; ;; .returns
;; (define  (c-function-wrap libmysqlclient ))
;; ;; 
;; ;; .form ()
;; ;; .returns
;; (define  (c-function-wrap libmysqlclient ))
;; ;; 
;; ;; .form ()
;; ;; .returns
;; (define  (c-function-wrap libmysqlclient ))
;; ;; 
;; ;; .form ()
;; ;; .returns
;; (define  (c-function-wrap libmysqlclient ))
;; ;; 
;; ;; .form ()
;; ;; .returns
;; (define  (c-function-wrap libmysqlclient ))



)
