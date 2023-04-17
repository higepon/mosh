
(import (rnrs))
(import (mosh))
(import (mosh test))

(test-true (regexp? #/abc/))
(test-false (regexp? "abc"))
(test-equal #/abc/ (string->regexp "abc"))
(test-equal "abc" (regexp->string #/abc/))

(test-equal 1 (rxmatch-start (rxmatch #/\d+/ "a345a")))
(test-equal 2 (rxmatch-start (rxmatch #/\d+/ "ab345a")))
(test-equal 5 (rxmatch-start (rxmatch #/\d+/ "abあ345a")))
(test-equal 1 (rxmatch-start (rxmatch #/(\d+)(a)/ "a345a") 1))
(test-equal 4 (rxmatch-start (rxmatch #/(\d+)(a)/ "a345a") 2))

(test-false (rxmatch-start (#/\d+/ "aaaa")))
(test-equal 1 (rxmatch-start (#/\d+/ "a345a")))
(test-equal 2 (rxmatch-start (#/\d+/ "ab345a")))
(test-equal 5 (rxmatch-start (#/\d+/ "abあ345a")))
(test-equal 1 (rxmatch-start (#/(\d+)(a)/ "a345a") 1))
(test-equal 4 (rxmatch-start (#/(\d+)(a)/ "a345a") 2))

(test-equal 4 (rxmatch-end  (rxmatch #/\d+/ "a345a")))
(test-equal 4 (rxmatch-end  (rxmatch #/(\d+)(a)/ "a345a") 1))
(test-equal 5 (rxmatch-end  (rxmatch #/(\d+)(a)/ "a345a") 2))
(test-false (rxmatch-end (rxmatch #/\d+/ "aaaa")))
(test-equal 4 (rxmatch-end (#/\d+/ "a345a")))
(test-equal 4 (rxmatch-end (#/(\d+)(a)/ "a345a") 1))
(test-equal 5 (rxmatch-end (#/(\d+)(a)/ "a345a") 2))

(test-equal "def" (rxmatch-after (#/abc/ "123abcdef")))
(test-equal "あいうえ" (rxmatch-after (#/abc/ "123abcあいうえ")))
(test-equal "あいうえ" (rxmatch-after (#/かきく/ "123かきくあいうえ")))
(test-equal "あいうえ" (rxmatch-after (#/かきく/ "あ123かきくあいうえ")))

(test-equal "345" (rxmatch-substring (#/\d+/ "a345a")))
(test-equal "345" (rxmatch-substring (#/(\d+)(a)/ "a345a") 1))
(test-equal "a" (rxmatch-substring (#/(\d+)(a)/ "a345a") 2))
(test-false (rxmatch-substring (#/\d+/ "aaaa")))

(test-equal "123" (rxmatch-before (#/abc/ "123abcdef")))
(test-equal "あいう" (rxmatch-before (#/abc/ "あいうabcdef")))

(test-false (rxmatch #/123/ "12"))
(test-true (if (rxmatch #/123/ "123") #t #f))


(test-false (#/123/ "12"))
(test-true (if (#/^abc/ "abc") #t #f))

;(test-equal "あ" ((#/あ/ "あ")))
(test-results)
