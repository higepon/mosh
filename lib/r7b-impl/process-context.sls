#!r6rs
(library (r7b-impl process-context)
         (export
;; from R7RS draft 4
command-line exit get-environment-variable get-environment-variables
)
         (import (rnrs) (srfi i98)))
