

(let* ([handle (%ffi-open "./libffitest.so.1.0")]
       [upper (%ffi-lookup handle "my_upper")]
       [bv    (u8-list->bytevector (map char->integer (string->list "hello")))])
  (%ffi-call->void upper bv 5)
  (list->string (map integer->char (bytevector->u8-list bv))))
