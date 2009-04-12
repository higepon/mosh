(import (rnrs)
        (mosh dbi)
        (mosh)
        (mosh test)
        (clos core))

(test-begin "dbi")

;; (let ([conn (guard (c (#t #;(display "mysql not supporeted\n") #f))
;;                       (dbi-connect "dbi:mysql:mysql:127.0.0.1:3306" "root" ""))])
;;   (when conn
;;     ;; conn is sub-class of <connection>
;;     (test-true (if (memq <connection> (class-direct-supers (class-of conn))) #t #f))

;;     (let ([query (dbi-prepare conn "select * from user where user = ?")])
;;       ;; query is <query> or sub-class of query
;;       (test-true (or (eq? (class-of query) <query>)
;;                   (if (memq <query> (class-direct-supers (class-of query))) #t #f)))

;;       (let ([result (dbi-execute query "root")])

;;         ;; result is <result> or sub-class of result
;;         (test-true (or (eq? (class-of result) <result>)
;;                     (if (memq <result> (class-direct-supers (class-of result))) #t #f)))

;;         (let ([getter (dbi-getter result)])
;;           (for-each
;;            (lambda (row)
;;              (test-true (string? (getter row "host")))
;;              (test-true (string? (getter row "User"))))
;;            (dbi-result->list result))
;;           (dbi-close conn))))))


(test-end)

;; dangerous
;; (let ([conn (guard (c (#t (display "mysql not supporeted\n") #f))
;;                       (dbi-connect "dbi:mysql:mysql:127.0.0.1:3306" "root" ""))])
;;   (when conn
;;    (dbi-do conn "create table hoge (a text, b integer);")
;;    ;; insert
;;    (let ([query (dbi-prepare conn "insert into hoge values(?, ?)")])
;;      (do ([i 0 (+ i 1)])
;;          ((= i 5) #f)
;;        (dbi-execute query (format "hoge~a" i) i)))
;;    ;; check
;;    (let ([result (dbi-do conn "select * from hoge")])
;;      (let ([getter (dbi-getter result)])
;;        (for-each
;;         (lambda (row)
;;           (test-true (string? (getter row "a")))
;;           (test-true (string? (getter row "b"))))
;;         (dbi-result->list result))))
;;    (dbi-do conn "drop table hoge")
;;    (dbi-close conn)))
