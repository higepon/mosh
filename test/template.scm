(import (rnrs)
        (mosh test)
        (shorten)
        (mosh file)
        (template))

(define temp-file "template-temp")

(define (test-equal-template expected template vars)
  (when (file-exists? temp-file)
    (delete-file temp-file))
  (with-output-to-file temp-file
    (^()
     (eval-template template vars)))
  (test-equal expected (file->string temp-file))
  (delete-file temp-file))

(test-equal-template "hige" "<%= a %>" '((a . "hige")))
(test-equal-template "hige" "<% (display a) %>" '((a . "hige")))
(test-equal-template "<html></html>" "<html></html>" '((a . "hige")))
(test-equal-template "<html>hoge</html>" "<html><%= a %></html>" '((a . "hoge")))
(test-equal-template "<html>\nhoge</html>" "<html>\n<%= a %></html>" '((a . "hoge")))
(test-equal-template "<li>hige</li><li>hage</li>" "<% (for-each (lambda (x) %><li><%= x %></li><% ) b) %>" '((b . '("hige" "hage"))))
(test-equal-template "" "" '())
(test-equal 3 (ref '((a . 3)) a))

;; todo: works on only nmosh
;(test-equal-template "#t" "<%= (hashtable? a) %>" `((a . ,(make-eq-hashtable))))
(test-results)
