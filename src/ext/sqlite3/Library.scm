(sqlite
  c-function-table
  (header: "sqlite3.h")
  (pkg-config: sqlite3)
  (libname: sqlite3)
  #(ret name args)
  (int sqlite3_open (char* sqlite3**))
  (int sqlite3_exec (sqlite3* char* fn void* char**))
  (int sqlite3_close (sqlite3*))
  (int sqlite3_reset (sqlite3_stmt*))
  (int sqlite3_prepare_v2 (sqlite3* char* int sqlite3_stmt** char**))
  (int sqlite3_bind_text (sqlite3_stmt* int void* int fn))
  (int sqlite3_step (sqlite3_stmt*))
  (int sqlite3_column_int (sqlite3_stmt* int))
  (int sqlite3_column_blob (sqlite3_stmt* int))
  (int sqlite3_finalize (sqlite3_stmt*)))

(sqlite-constants
  constant-table
  #(name value type)
  (SQLITE_TRANSIENT -1 void*)
  (SQLITE_OK 0)
  (SQLITE_ROW 100)
  (SQLITE_DONE 101)
  (SQLITE_ERROR 1))

(sqlite-objects
  object-table
  #(name)
  (sqlite3)
  (sqlite3_stmt))

