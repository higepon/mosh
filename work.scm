(let* ([handle (%ffi-open "./libffitest.so.1.0")]
       [p (%ffi-lookup handle "pointer")])
  (display (number->string (%ffi-call->void* p) 16)))
