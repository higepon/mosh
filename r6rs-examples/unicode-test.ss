(import (rnrs)
        (mosh test))

(test* "\x41;" "A")
(test* "\x41;\x42;C\x44;" "ABCD")
