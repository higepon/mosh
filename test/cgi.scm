(import (rnrs)
        (mosh test)
        (mosh cgi))

(test-equal "あああ" (decode "%E3%81%82%E3%81%82%E3%81%82"))

(test-results)
