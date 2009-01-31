(let* ([handle (%ffi-open "./libffitest.so.1.0")]
           [sub (%ffi-lookup handle 'subf2)])
      (display (%ffi-call->double sub 6.0 2.0)))
