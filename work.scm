(let* ([handle (%ffi-open "./libffitest.so.1.0")]
            [sub (%ffi-lookup handle 'sub)])
       (display (%ffi-call->int sub 5 -11)))
