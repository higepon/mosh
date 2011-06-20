(library (nmosh win32 misc)
         (export
           win32_get_processor_count
           win32_get_ansi_codepage
           win32_multibyte_to_widechar
           win32_measure_multibyte_to_widechar
           win32_mypath)
         (import (rnrs)
                 (nmosh ffi box)
                 (prefix (nmosh stubs win32-misc) stub:))
(define (win32_get_processor_count)
  (stub:win32_get_processor_count))

(define (win32_get_ansi_codepage)
  (stub:win32_get_ansi_codepage))

(define (win32_multibyte_to_widechar cp input output)
  (let ((output-size (make-int-box)))
    (let ((ret (stub:win32_multibyte_to_widechar cp
                                                 input
                                                 (bytevector-length input)
                                                 output
                                                 (bytevector-length output)
                                                 output-size)))
      (if (= ret 1)
        (int-box-ref output-size)
        (assertion-violation 'win32_multibyte_to_widechar
                             "something wrong with MultiByteToWideChar")))))

(define (win32_measure_multibyte_to_widechar cp input)
  (let ((ret (stub:win32_measure_multibyte_to_widechar 
               cp
               input
               (bytevector-length input))))
    ret))

(define (win32_mypath bv size)
  (let ((ret (stub:win32_mypath bv size)))
    (unless (= ret 1)
      (assertion-violation 'win32_mypath
                           "something wrong with win32_mypath"))))

)
