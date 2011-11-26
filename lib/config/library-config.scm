(mosh/nmosh
  ;; mosh/nmosh common
  (legend: 
    (alias-name => "name of library"))
  #(alias-name interface-level)
  ;; mosh/nmosh distribution
  ;; clos
  [(clos user) user]
  [(clos core) user]
  [(clos helpers) user]
  [(clos slot-access) user]
  [(clos std-protocols *) user]
  [(clos introspection) user]
  [(clos private *) internal]

  ;; config
  [(config *) internal]

  ;; mosh specific API
  [(mosh c-type) user]
  [(mosh c-type helper) internal]
  [(mosh cgi) user]
  [(mosh concurrent) user]
  ;[(mosh config) user]
  [(mosh control) user]
  [(mosh dbd mysql) user]
  [(mosh dbi) user]
  [(mosh ffi) user]
  [(mosh file) user]
  [(mosh io conditions) user]
  [(mosh irc client) user]
  ;[(mosh mysql) user]
  [(mosh pp) user]
  [(mosh process) user]
  [(mosh queue) user]
  [(mosh shell repl) user]
  [(mosh socket) user]
  [(mosh test) user]

  ;; mosh specific API (staging)
  [(nmosh debugger) internal]
  [(nmosh debugger condition-printer) internal]
  [(nmosh debugger core) internal]
  [(nmosh ffi pffi) internal]
  [(nmosh ffi stublib) internal]
  [(nmosh global-flags) internal]
  [(nmosh process) internal]
  [(nmosh process win32) internal]
  [(nmosh process mosh) internal]
  [(nmosh stubs *) internal]
  [(nmosh ui deco) internal]
  [(nmosh win32 handle) internal]
  [(nmosh win32 named-pipe) internal]
  [(nmosh win32 util) internal]
  [(nmosh) user*]

  ;; yuni/nmosh specific
  [(yuni *) internal]

  ;; lambda
  [(lambda wiki) user]

  ;; public-misc
  [(irregex) user]
  [(json) user]
  [(packrat) user]

  ;; public-misc (mosh original)
  [(rbtree) user]
  [(shorten) user]
  [(shorten helper) internal]
  [(monapi) user]
  [(facebook) user]
  [(http) user]
  [(template) user]
  [(uri) user]

)

(srfi-97
  (legend: 
    (alias-name => "name of library")
    (library-name => "library true name"))
  #(alias-name library-name interface-level)

  ;; SRFI-97 definitions
  [(srfi :1) (srfi :1 lists) srfi]
  [(srfi :2) (srfi :2 and-let*) srfi]
  [(srfi :5) (srfi :5 let) srfi]
  [(srfi :6) (srfi :6 basic-string-ports) srfi]
  [(srfi :8) (srfi :8 receive) srfi]
  [(srfi :9) (srfi :9 records) srfi]
  [(srfi :11) (srfi :11 let-values) srfi]
  [(srfi :13) (srfi :13 strings) srfi]
  [(srfi :14) (srfi :14 char-sets) srfi]
  [(srfi :16) (srfi :16 case-lambda) srfi]
  [(srfi :17) (srfi :17 generalized-set!) srfi]
  [(srfi :18) (srfi :18 multithreading) srfi]
  [(srfi :19) (srfi :19 time) srfi]
  [(srfi :21) (srfi :21 real-time-multithreading) srfi]
  [(srfi :23) (srfi :23 error) srfi]
  [(srfi :25) (srfi :25 multi-dimensional-arrays) srfi]
  [(srfi :26) (srfi :26 cut) srfi]
  [(srfi :27) (srfi :27 random-bits) srfi]
  [(srfi :28) (srfi :28 basic-format-strings) srfi]
  [(srfi :29) (srfi :29 localization) srfi]
  [(srfi :31) (srfi :31 rec) srfi]
  [(srfi :38) (srfi :38 with-shared-structure) srfi]
  [(srfi :39) (srfi :39 parameters) srfi]
  [(srfi :41) (srfi :41 streams) srfi]
  [(srfi :41 streams) [(srfi :41 streams primitive)
                       (srfi :41 streams derived)] srfi]
  [(srfi :42) (srfi :42 eager-comprehensions) srfi]
  [(srfi :43) (srfi :43 vectors) srfi]
  [(srfi :44) (srfi :44 collections) srfi]
  [(srfi :45) (srfi :45 lazy) srfi]
  [(srfi :46) (srfi :46 syntax-rules) srfi]
  [(srfi :47) (srfi :47 arrays) srfi]
  [(srfi :48) (srfi :48 intermediate-format-strings) srfi]
  [(srfi :51) (srfi :51 rest-values) srfi]
  [(srfi :54) (srfi :54 cat) srfi]
  [(srfi :57) (srfi :57 vectors) srfi]
  [(srfi :59) (srfi :59 vicinities) srfi]
  [(srfi :60) (srfi :60 integer-bits) srfi]
  [(srfi :61) (srfi :61 cond) srfi]
  [(srfi :63) (srfi :63 arrays) srfi]
  [(srfi :64) (srfi :64 testing) srfi]
  [(srfi :66) (srfi :66 octet-vectors) srfi]
  [(srfi :67) (srfi :67 compare-procedures) srfi]
  [(srfi :69) (srfi :69 basic-hash-tables) srfi]
  [(srfi :71) (srfi :71 let) srfi]
  [(srfi :74) (srfi :74 blobs) srfi]
  [(srfi :78) (srfi :78 lightweight-testing) srfi]
  [(srfi :86) (srfi :86 mu-and-nu) srfi]
  [(srfi :87) (srfi :87 case) srfi]
  [(srfi :95) (srfi :95 sorting-and-merging) srfi]
)

(srfi
  (legend: 
    (alias-name => "name of library")
    (library-name => "library true name"))
  #(alias-name library-name interface-level)
  ;; SRFI
  [(srfi :98) (srfi :98 os-environment-variables) srfi]
  [(srfi :99) (srfi :99 records) srfi]
  [(srfi :99 records) [(srfi :99 records procedural)
                       (srfi :99 records inspection)
                       (srfi :99 records syntactic)] srfi]
)

(srfi-extra
  ;; libraries not defined in SRFI-97
  (legend: 
    (alias-name => "name of library")
    (library-name => "library true name"))
  #(alias-name library-name interface-level)
  ;; SRFI-0
  [(srfi :0) (srfi :0 cond-expand) srfi-extra]

  ;; SRFI-37
  [(srfi :37) (srfi :37 args-fold) srfi-extra]
)

(srfi-r7b
  ;; SRFIs for R7RS bridge
  (legend: 
    (alias-name => "name of library")
    (library-name => "library true name"))
  #(alias-name library-name interface-level)
  [(srfi i0) (srfi :0)]
  [(srfi i6) (srfi :6)]
  [(srfi i9) (srfi :9)]
  [(srfi i19) (srfi :19)]
  [(srfi i23) (srfi :23)]
  [(srfi i39) (srfi :39)]
  [(srfi i98) (srfi :98)]
)

(r7rs-bridge
  (legend: 
    (alias-name => "name of library")
    (library-name => "library true name"))
  #(alias-name library-name interface-level)
  [(scheme base) (r7b-impl base)]
  [(scheme case-lambda) (r7b-impl case-lambda)]
  [(scheme char) (r7b-impl char)]
  [(scheme char normalization) (r7b-impl char normalization)]
  [(scheme complex) (r7b-impl complex)]
  [(scheme division) (r7b-impl division)]
  [(scheme eval) (r7b-impl eval)]
  [(scheme file) (r7b-impl file)]
  [(scheme inexact) (r7b-impl inexact)]
  [(scheme lazy) (r7b-impl lazy)]
  [(scheme load) (r7b-impl load)]
  [(scheme process-context) (r7b-impl process-context)]
  [(scheme read) (r7b-impl read)]
  [(scheme repl) (r7b-impl repl)]
  [(scheme time) (r7b-impl time)]
  [(scheme write) (r7b-impl write)]
)

(rnrs
  (legend: 
    (alias-name => "name of library")
    (library-name => "library true name"))
  #(alias-name library-name interface-level)
  ;; rnrs composite
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
)


