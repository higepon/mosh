;; generated from src/ext/sqlite3/Library.scm DO NOT EDIT!!
(library (nmosh stubs sqlite3)
(export
  sqlite3_finalize
  sqlite3_column_blob
  sqlite3_column_int
  sqlite3_step
  sqlite3_bind_text
  sqlite3_prepare_v2
  sqlite3_reset
  sqlite3_close
  sqlite3_exec
  sqlite3_open
  SQLITE_ERROR
  SQLITE_DONE
  SQLITE_ROW
  SQLITE_OK
  SQLITE_TRANSIENT)
(import
  (mosh ffi)
  (rnrs)
  (nmosh ffi pffi)
  (nmosh ffi stublib))


(define-ffi-library %library sqlite3 sqlite3)

(define SQLITE_ERROR 1)
(define SQLITE_DONE 101)
(define SQLITE_ROW 100)
(define SQLITE_OK 0)
(define SQLITE_TRANSIENT (integer->pointer -1))

(define
  sqlite3_open
  (pffi-c-function
    %library
    int
    sqlite3_open
    char*
    void*))
(define
  sqlite3_exec
  (pffi-c-function
    %library
    int
    sqlite3_exec
    void*
    char*
    callback
    void*
    void*))
(define
  sqlite3_close
  (pffi-c-function
    %library
    int
    sqlite3_close
    void*))
(define
  sqlite3_reset
  (pffi-c-function
    %library
    int
    sqlite3_reset
    void*))
(define
  sqlite3_prepare_v2
  (pffi-c-function
    %library
    int
    sqlite3_prepare_v2
    void*
    char*
    int
    void*
    void*))
(define
  sqlite3_bind_text
  (pffi-c-function
    %library
    int
    sqlite3_bind_text
    void*
    int
    void*
    int
    callback))
(define
  sqlite3_step
  (pffi-c-function %library int sqlite3_step void*))
(define
  sqlite3_column_int
  (pffi-c-function
    %library
    int
    sqlite3_column_int
    void*
    int))
(define
  sqlite3_column_blob
  (pffi-c-function
    %library
    int
    sqlite3_column_blob
    void*
    int))
(define
  sqlite3_finalize
  (pffi-c-function
    %library
    int
    sqlite3_finalize
    void*))
)
