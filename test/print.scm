(import (rnrs)
        (mosh)
        (only (mosh pp) pp)
        (mosh socket)
        (mosh test))

(define-syntax test-print
  (lambda (x)
    (syntax-case x ()
      [(_ expected expr write-proc)
       (regexp? (syntax->datum #'expected))
       #'(test-true (expected (call-with-values open-string-output-port (lambda (port proc) (write-proc expr port) (proc)))))]
      [(_ expected expr write-proc)
       #'(test-equal expected (call-with-values open-string-output-port (lambda (port proc) (write-proc expr port) (proc))))])))

(define-syntax test-print*
  (lambda (x)
    (syntax-case x ()
      [(_ (expr expected) more ...)
       #'(test-print* (expr expected expected expected) more ...)]
      [(_ (expr display-expected write-expected) more ...)
       #'(test-print* (expr display-expected write-expected write-expected) more ...)]
      [(_ (expr display-expected write-expected pp-expected) more ...)
       #'(begin
           (test-print display-expected expr display)
           (test-print write-expected expr write)
           (test-print (if (string? pp-expected) (string-append pp-expected "\n") pp-expected) expr pp)
           (test-print* more ...))]
      [(_) #'#f])))


(test-print* ['(a b) "(a b)"]
             ['(a . b) "(a . b)"]
             [1 "1"]
             [222222222222222222222222222 "222222222222222222222222222"]
             [3.141592 "3.141592"]
             [#\a "a" "#\\a"]
             ['#(a b c) "#(a b c)"]
             ["abc" "abc" "\"abc\""]
             [(open-file-input-port "./mosh") "<binary-input-port ./mosh>" "<binary-input-port ./mosh>" "#[input-port]"]
             [(open-input-file "./mosh") "<transcoded-textual-input-port <binary-input-port ./mosh>>" "<transcoded-textual-input-port <binary-input-port ./mosh>>" "#[input-port]"]
             [(open-string-output-port) "<string-output-port>" "<string-output-port>" "#[output-port]"]
             [(make-custom-textual-output-port
               "custom out"
               (lambda (str start count) #f)
               (lambda () #f)
               (lambda (pos) #f)
               (lambda () 'ok)) "<custom-textual-output-port custom out>" "<custom-textual-output-port custom out>" "#[output-port]"]
             [(lambda (x) #f) #/#<closure \d+>/ #/#<closure \d+>/ "#[procedure]"]
             [(make-client-socket "www.google.co.jp" "http") #/<socket client .*>/ #/<socket client .*>/ "#[socket]"]
             [car #/<subr car>/ #/<subr car>/ "#[procedure]"]
             ['a "a"]
             [(make-eq-hashtable) "#<eq-hashtable>" "#<eq-hashtable>" "#[hashtable]"]
             [(make-eqv-hashtable) "#<eqv-hashtable>" "#<eqv-hashtable>" "#[hashtable]"]
             [(make-hashtable (lambda () '()) eqv?) "#<hashtable>" "#<hashtable>" "#[hashtable]"]
             [#vu8(1 2 3) "#vu8(1 2 3)"]
             [#t "#t"]
             [#f "#f"]
             [#/1/ "#/1/" "#/1/" "#[regexp]"]
             [(#/\d+/ "123") "#<reg-match>" "#<reg-match>" "#[procedure]"]
             [(utf-8-codec) "#<codec utf-8-codec>" "#<codec utf-8-codec>" "#[unknown]"]
             [(make-error) "#<record &error>" "#<record &error>" "#[record]"]
             [1/2 "1/2"]
             [1+2i "1+2i"]
             ['() "()"]
             [#'a #/.*/ #/.*/ "#[identifier]"]
             ['(quote 3) "'3"]
             ['(QUOTE 3) "'3"]
             ['(quasiquote 3) "`3"]
             ['(QUASIQUOTE 3) "`3"]
             ['(unquote 3) ",3"]
             ['(UNQUOTE 3) ",3"]
             ['(unquote-splicing 3) ",@3"]
             ['(UNQUOTE-SPLICING 3) ",@3"]
;;              ['(syntax a) "#'a"]
;;              ['(SYNTAX a) "#'a"]
;;              ['(quasisyntax 3) "#`3"]
;;              ['(QUASISYNTAX 3) "#`3"]
;;              ['(unsyntax a) "#,a"]
;;              ['(UNSYNTAX a) "#,a"]
;;              ['(unsyntax-splicing a) "#,@a"]
;;              ['(UNSYNTAX-SPLICING a) "#,@a"]
             [(eof-object) "#<eof-object>" "#<eof-object>" "#[eof-object]"]
;             [1.0e99 "1e99"]
             [(condition '()) "#<compound-condition ()>" "#<compound-condition ()>" "#[condition]"]
             [(make-record-type-descriptor 'hoge #f #f #f #f '#(a b c)) "#<record-type-descriptor>" "#<record-type-descriptor>" "#[record-type-descriptor]"]
             [(if #f #t) "#<unspecified>" "#<unspecified>" "#[unspecified]"] ;; unspecified

)

(test-equal "+inf.0" (number->string +inf.0))
(test-equal "-inf.0" (number->string -inf.0))
(test-equal "+nan.0" (number->string +nan.0))

(test-results)
