(import (rnrs)
        (dbi)
        (mosh test)
        (clos core))

(define conn (dbi-connect "dbi:mysql:mysql:127.0.0.1:3306" "root" ""))

;; conn is sub-class of <connection>
(test/t (if (memq <connection> (class-direct-supers (class-of conn))) #t #f))

;; (define query (dbi-prepare conn "select * from user"))

;; ;; conn is <query> or sub-class of query
;; (test/t (or (eq? (class-of query) <query>)
;;             (memq <query> (class-direct-supers (class-of query)))))
