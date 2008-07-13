(import (rnrs) (cgi) (srfi-8)
        )

(receive (get-parameter get-request-method) (init)
  (display (get-parameter "hige")))

