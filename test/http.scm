(import (rnrs)
        (mosh socket)
        (only (srfi :13) string-contains)
        (http)
        (json)
        (mosh test))

(when (ssl-supported?)
  (test-equal "19292868552" (cdr (vector-ref (json-read (open-string-input-port (http-get "https://graph.facebook.com/19292868552"))) 0))))

(test-true (string-contains (http-get "http://www.hatena.ne.jp") "</html>"))
(test-true (string-contains (http-get "http://www.hatena.ne.jp:80") "</html>"))
(test-true (string-contains (http-get "http://www.hatena.ne.jp:80/") "</html>"))

(write (http-get "http://ow.ly/3BKpg"))
(test-true (string-contains (http-get "http://ow.ly/3BKpg") "mona"))

(test-results)
