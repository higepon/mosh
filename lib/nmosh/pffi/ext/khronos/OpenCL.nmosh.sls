(library (nmosh pffi ext khronos OpenCL)
         (export 
           mcl_platform_count
           mcl_get_platforms
           mcl_platform_profile
           mcl_platform_version
           mcl_platform_name
           mcl_platform_vendor
           mcl_platform_extensions
           )
         (import (rnrs)
                 (shorten)
                 (yuni core)
                 (srfi :42)
                 (nmosh ffi box)
                 (nmosh pffi interface)
                 (nmosh pffi util)
                 (prefix (nmosh stubs mosh-opencl) stub:))

(define-syntax define-platform-accessor
  (syntax-rules ()
    ((_ name xSize xGetter)
     (define (name p)
       (let* ((size (xSize p))
              (out-size (make-int-box))
              (buf (make-bytevector size)))
         (let ((r (xGetter p buf size out-size)))
           (cond
             ((= r 0) ;; CL_SUCCESS
              (utf8/null->string buf))
             (else ""))))))))

(define-platform-accessor
  mcl_platform_profile
  stub:mcl_platform_profile_size
  stub:mcl_platform_profile)

(define-platform-accessor
  mcl_platform_version
  stub:mcl_platform_version_size
  stub:mcl_platform_version)

(define-platform-accessor
  mcl_platform_name
  stub:mcl_platform_name_size
  stub:mcl_platform_name)

(define-platform-accessor
  mcl_platform_vendor
  stub:mcl_platform_vendor_size
  stub:mcl_platform_vendor)

(define-platform-accessor
  mcl_platform_extensions
  stub:mcl_platform_extensions_size
  stub:mcl_platform_extensions)

(define platform-id-ref
  (case (stub:mcl_platform_id_size)
    ((4)
     (^(bv pos)
       (integer->pointer (bytevector-u32-native-ref bv (* 4 pos)))))
    ((8)
     (^(bv pos)
       (integer->pointer (bytevector-u32-native-ref bv (* 8 pos)))))
    (else
      (assertion-violation
        'id-ref
        "unsupported platform"
        (stub:mcl_platform_id_size)))))

(define mcl_platform_count stub:mcl_platform_count)
(define (mcl_get_platforms)
  (let* ((count (stub:mcl_platform_count))
         (siz (* (stub:mcl_platform_id_size)
                 count))
         (buf (make-bytevector siz))
         (out-count (make-int-box)))
    (let ((r (stub:mcl_clGetPlatformIDs count
                                        buf
                                        out-count)))
      (cond 
        ((= r 0) ;; CL_SUCCESS
         (list-ec (: i (int-box-ref out-count))
                  (platform-id-ref buf i)))
        (else '())))))

(define (mcl_platform_profile_size p) ;; => int/#f
  (let ((r (stub:mcl_platform_profile_size p)))
    (and (not (= r -1))
         r)))

)
