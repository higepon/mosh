(import (rnrs)
        (mosh test)
        (mosh cgi))

(test-equal "あああ" (decode "%E3%81%82%E3%81%82%E3%81%82"))
(test-equal "TopPage" (decode "%54%6f%70%50%61%67%65"))
(test-equal "まげまげ" (decode "%e3%81%be%e3%81%92%e3%81%be%e3%81%92"))
(test-results)
