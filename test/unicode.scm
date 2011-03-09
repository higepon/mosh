(import (rnrs)
        (uri)
        (mosh test))

(test-equal "\x41;" "A")
(test-equal "\x41;\x42;C\x44;" "ABCD")
(test-equal (string-normalize-nfd "a\x0308;\x0323;か\x3099;e\x304;\x301;\x323;") "a\x0323;\x0308;か\x3099;e\x323;\x304;\x301;")

(test-equal "ABC\x0;ABC" (utf8->string '#vu8(65 66 67 0 65 66 67)))
(test-equal #vu8(65 66 67 0 65 66 67) (string->utf8 "ABC\x0;ABC"))

(let ([org "起業"])
  (test-equal "%e8%b5%b7%e6%a5%ad" (uri-encode org))
  (test-equal org (uri-decode (uri-encode org))))

(test-equal "a%0ab" (uri-encode "a\nb"))

(test-results)
