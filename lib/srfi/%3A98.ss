; SRFI 98: An interface to access environment variables
; http://srfi.schemers.org/srfi-98/srfi-98.html

(library (srfi :98)
         (export get-environment-variable get-environment-variables)
         (import (system))
)
