(import (rnrs)
        (mosh test)
        (mosh pp)
        (yuni util files)
        (yuni text config reader))

(define file (file->string-list "test.cfg"))
(define cfg (string-list->config file))
(define cfg-data
  (quote ((("test") ("hoge" . "fuga")) 
          (("testsection" "sectionname") ("name" . "true")))))


(test-equal cfg-data cfg)

(test-results)

