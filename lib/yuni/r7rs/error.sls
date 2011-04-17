(library (yuni r7rs error)
         (export error)
         (import (except (rnrs) error))

(define (error msg . args)
  (assertion-violation #f msg args))

)
