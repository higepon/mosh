(let* ([handle (%ffi-open "/usr/local/mysql/lib/libmysqlclient.dylib")])
       #f)
;       [p (%ffi-lookup handle "pointer")])
;  (display (number->string (%ffi-call->void* p) 16)))

