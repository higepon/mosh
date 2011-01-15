(library (library-config)
         (export library-config)
         (import (rnrs))

(define library-config
  '(
    (legend: 
      (alias-name => "name of library")
      (library-name => "library true name"))
    #(alias-name library-name interface-level)

    ;; SRFI-97 definitions
    [(srfi :1) (srfi :1 lists) srfi]
    [(srfi :2) (srfi :2 and-let*) srfi]
    ;[(srfi :5) (srfi :5 let) srfi]
    [(srfi :6) (srfi :6 basic-string-ports) srfi]
    [(srfi :8) (srfi :8 receive) srfi]
    [(srfi :9) (srfi :9 records) srfi]
    [(srfi :11) (srfi :11 let-values) srfi]
    [(srfi :13) (srfi :13 strings) srfi]
    [(srfi :14) (srfi :14 char-sets) srfi]
    [(srfi :16) (srfi :16 case-lambda) srfi]
    ;[(srfi :17) (srfi :17 generalized-set!) srfi]
    ;[(srfi :18) (srfi :18 multithreading) srfi]
    [(srfi :19) (srfi :19 time) srfi]
    ;[(srfi :21) (srfi :21 real-time-multithreading) srfi]
    [(srfi :23) (srfi :23 error) srfi]
    ;[(srfi :25) (srfi :25 multi-dimensional-arrays) srfi]
    [(srfi :26) (srfi :26 cut) srfi]
    [(srfi :27) (srfi :27 random-bits) srfi]
    ;[(srfi :28) (srfi :28 basic-format-strings) srfi]
    [(srfi :29) (srfi :29 localization) srfi]
    [(srfi :31) (srfi :31 rec) srfi]
    [(srfi :38) (srfi :38 with-shared-structure) srfi]
    [(srfi :39) (srfi :39 parameters) srfi]
    [(srfi :41) [(srfi :41 streams)
                 (srfi :41 streams primitive)
                 (srfi :41 streams derived)] srfi]
    [(srfi :42) (srfi :42 eager-comprehensions) srfi]
    [(srfi :43) (srfi :43 vectors) srfi]
    ;[(srfi :44) (srfi :44 collections) srfi]
    ;[(srfi :45) (srfi :45 lazy) srfi]
    ;[(srfi :46) (srfi :46 syntax-rules) srfi]
    ;[(srfi :47) (srfi :47 arrays) srfi]
    [(srfi :48) (srfi :48 intermediate-format-strings) srfi]
    ;[(srfi :51) (srfi :51 rest-values) srfi]
    ;[(srfi :54) (srfi :54 cat) srfi]
    ;[(srfi :57) (srfi :57 vectors) srfi]
    ;[(srfi :59) (srfi :59 vicinities) srfi]
    ;[(srfi :60) (srfi :60 integer-bits) srfi]
    [(srfi :61) (srfi :61 cond) srfi]
    ;[(srfi :63) (srfi :63 arrays) srfi]
    [(srfi :64) (srfi :64 testing) srfi]
    ;[(srfi :66) (srfi :66 octet-vectors) srfi]
    [(srfi :67) (srfi :67 compare-procedures) srfi]
    ;[(srfi :69) (srfi :69 basic-hash-tables) srfi]
    ;[(srfi :71) (srfi :71 let) srfi]
    ;[(srfi :74) (srfi :74 blobs) srfi]
    [(srfi :78) (srfi :78 lightweight-testing) srfi]
    ;[(srfi :86) (srfi :86 mu-and-nu) srfi]
    ;[(srfi :87) (srfi :87 case) srfi]
    ;[(srfi :95) (srfi :95 sorting-and-merging) srfi]

    ;; SRFI
    [(srfi :98) (srfi :98 os-environment-variables) srfi]
    [(srfi :99) (srfi :99 records) srfi]
    [(srfi :99 records) [(srfi :99 records procedural)
                         (srfi :99 records inspection)
                         (srfi :99 records syntactic)] srfi]

    ;; SRFI-0
    [(srfi :0) (srfi :0 cond-expand) srfi-port]

    ;; SRFI-37
    [(srfi :37) (srfi :37 args-fold) srfi-port]

    ;; rnrs
    [(rnrs) [(rnrs base)
             (rnrs control)
             (rnrs lists)
             (rnrs syntax-case)
             (rnrs io simple)
             (rnrs unicode)
             (rnrs sorting)
             (rnrs records procedural)
             (rnrs records inspection)
             (rnrs files)
             (rnrs arithmetic fixnums)
             (rnrs arithmetic flonums)
             (rnrs arithmetic bitwise)
             (rnrs records syntactic)
             (rnrs io ports)
             (rnrs exceptions)
             (rnrs conditions)
             (rnrs bytevectors)
             (rnrs hashtables)
             (rnrs programs)
             (rnrs enums)] rnrs]


    ;; mosh/nmosh distribution
    [(shorten helper) #f internal]

    )

)
