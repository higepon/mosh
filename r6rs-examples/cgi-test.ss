(import (rnrs) (cgi) (srfi-8)
        )

(receive (get-parameter get-request-method) (init "hige=hage")
  (display (get-parameter "hige")))

