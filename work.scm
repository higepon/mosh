(let* ([handle (%ffi-open "./libffitest.so.1.0")]
                   [func (%ffi-lookup handle 'double10_2)])
              (display (%ffi-call->double func 10.0 9.4 8.0 7.0 6.0 5.0 4.0 3.0 2.0 1.5)))
