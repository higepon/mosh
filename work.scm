(import (rnrs)
        (mosh string)
        (mysql)
        (mosh test))

(define mysql (mysql-init))

(when (zero? (mysql-real-connect mysql "127.0.0.1" "root" "" "mysql" 3306 NULL 0))
  (assertion-violation 'mysql-real-connect "failed"))

(unless (zero? (mysql-query mysql "select Host, User from user"))
  (assertion-violation 'mysql-query "failed"))

(let* ([result (mysql-store-result mysql)])
  (when (zero? result)
    (assertion-violation 'mysql-store-result "failed"))
  (format #t "result count=~d\n" (mysql-num-rows result))

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
 (test/t (string? (mysql-error mysql)))
 (mysql-data-seek result 1)
 (test/t (zero? (mysql-errno mysql)))
 (test/t (zero? (mysql-dump-debug-info mysql)))
 (test/t (string? (mysql-get-client-info)))
 (test/t (number? (mysql-affected-rows mysql)))
 (test/t (zero? (mysql-autocommit mysql 1)))
 (mysql-free-result result)
 (test/t (zero? (mysql-change-user mysql "root" "" "information_schema")))
 (test/t (string? (mysql-character-set-name mysql)))
 (test/t (zero? (mysql-commit mysql)))
 (mysql-close mysql)
 '())
