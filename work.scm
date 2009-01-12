

(let* ([handle (%ffi-open "./libffitest.so.1.0")]
       [return3 (%ffi-lookup handle "string_length")])
  (print (%ffi-call->int return3 "12345")))
