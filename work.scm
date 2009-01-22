(import (rnrs)
        (mosh string)
        (mysql)
        (mosh ffi)
        (mosh test))

(define mysql (mysql-init))

(define (test/mysql-result result)
  (when (zero? result)
    (assertion-violation 'mysql-store-result "failed"))
  (test/t (integer? (mysql-fetch-lengths result)))
  (test/t (integer? (mysql-field-count mysql)))
  (test/t (integer? (mysql-num-fields result)))
  (test/t (integer? (mysql-row-seek result 0)))
  (test/t (integer? (mysql-row-tell result)))
  (let loop ([row (mysql-fetch-row result)])
    (cond
     [(= row NULL) '()]
     [else
      (test/t (string? (mysql-row-ref row)))
      (loop (mysql-fetch-row result))]))
  (mysql-free-result result))

(when (zero? (mysql-real-connect mysql "127.0.0.1" "root" "" "mysql" 3306 NULL 0))
  (assertion-violation 'mysql-real-connect "failed"))

(unless (zero? (mysql-query mysql "select Host, User from user"))
  (assertion-violation 'mysql-query "failed"))

(test/t (integer? (mysql-set-character-set mysql "utf8")))

(test/t (integer? (mysql-select-db mysql "mysql")))
(let* ([result (mysql-store-result mysql)])
  (when (zero? result)
    (assertion-violation 'mysql-store-result "failed"))
  (format #t "result count=~d\n" (mysql-num-rows result))
  (test/t (integer? (mysql-fetch-lengths result)))
  (test/t (integer? (mysql-field-count mysql)))
  (test/t (integer? (mysql-set-server-option mysql 0)))
  (let loop ([row (mysql-fetch-row result)])
    (cond
     [(= row NULL) '()]
     [else
      (format #t "row=>~a\n" (mysql-row-ref row))
      (loop (mysql-fetch-row result))]))

  (let loop ([field (mysql-fetch-field result)])
    (cond
     [(= field NULL) '()]
     [else
      (test/t (string? (mysql-field-name field)))
      (loop (mysql-fetch-field result))]))
  (test/t (string? (mysql-field-name (mysql-fetch-field-direct result 1))))
  (test/t (string? (mysql-error mysql)))
  (mysql-data-seek result 1)
  (test/t (integer? (mysql-errno mysql)))
  (test/t (zero? (mysql-dump-debug-info mysql)))
  (test/t (string? (mysql-get-client-info)))
  (test/t (number? (mysql-affected-rows mysql)))
  (test/t (zero? (mysql-autocommit mysql 1)))
  (mysql-free-result result)
  (test/t (zero? (mysql-change-user mysql "root" "" "information_schema")))
  (test/t (string? (mysql-character-set-name mysql)))
  (test/t (zero? (mysql-commit mysql)))
  (test/t (integer? (mysql-get-client-version)))
  (test/t (string? (mysql-get-host-info mysql)))
  (test/t (integer? (mysql-get-proto-info mysql)))
  (test/t (string? (mysql-get-server-info mysql)))
  (test/t (integer? (mysql-get-server-version mysql)))
  (test/t (zero? (mysql-get-ssl-cipher mysql)))
  (let* ([from "abcd"]
         [len (string-length from)]
         [to-bv (make-bytevector (+ (* len 2) 1))])
    (mysql-hex-string to-bv from len)
    (test/t (equal? "61626364" (utf8->string to-bv))))
  (test/t (integer? (mysql-info mysql)))
  (test/t (integer? (mysql-insert-id mysql)))

  (test/mysql-result (mysql-list-dbs mysql NULL))
  (test/mysql-result (mysql-list-processes mysql))
  (test/mysql-result (mysql-list-tables mysql NULL))

  (test/t (= 0 (mysql-more-results mysql)))
  (test/t (integer? (mysql-next-result mysql)))
  (test/t (zero? (mysql-options mysql 2 NULL)))
  (test/t (zero? (mysql-ping mysql)))
  (let* ([from "hoge"]
         [len (string-length from)]
         [to-bv (make-bytevector (+ (* len 2) 1))])
    (mysql-real-escape-string mysql to-bv from len)
    (test/t (string=? "hoge" (utf8->string to-bv))))
  (test/t (integer? (mysql-real-query mysql "select Host, User from user" 28)))
  (test/t (integer? (mysql-refresh mysql NULL)))
  (mysql-reload mysql)
  (test/t (integer? (mysql-rollback mysql)))
  ;; This works, but don't do this in test.
  ;; (mysql-shutdwon mysql NULL)
  (display (mysql-sqlstate mysql))
  (mysql-close mysql)





;  (mysql-library-end)
  '())
