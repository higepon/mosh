

(let* ([handle (%ffi-open "./libffitest.so.1.0")]
       [return3 (%ffi-lookup handle "return3")])
  (print (%ffi-call->int return3)))
