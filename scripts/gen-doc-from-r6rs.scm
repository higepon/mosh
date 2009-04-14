(import (rnrs)
        (mosh)
        (except (srfi :1) assoc cons* filter find fold-right for-each map member partition remove)
        (srfi :26)
        (srfi :98))

(include "./scripts/r6rs-symbols.dat")

(define r6rs-dir (string-append (get-environment-variable "HOME") "/Desktop/r6rs/document/"))
(define out-dir (string-append (get-environment-variable "HOME") "/mosh/doc/text/"))

(define files '(("base.tex"
                 1
                 "Base Library"
                 ("(rnrs base (6))")
                 "The (rnrs base (6)) library exports many of the procedure and syntax bindings that are traditionally associated with Scheme.")
                ("bytevector.tex"
                 20
                 "Bytevectors"
                 ("(rnrs bytevectors (6))")
                 "Hhe (rnrs bytevectors (6))library provides a single type for blocks of binary data with multiple ways to access that data. It deals with integers and floating-point representations in various sizes with specified endianness.")
                ("list.tex"
                 30
                 "List utilities"
                 ("(rnrs lists (6))")
                 "The (rnrs lists (6)) library contains various useful procedures that operate on lists.")
                ("sort.tex"
                 40
                 "Sorting"
                 ("(rnrs sorting (6))")
                 "The (rnrs sorting (6)) library for sorting lists and vectors.")
                ("control.tex"
                 50
                 "Control structures"
                 ("(rnrs control (6))")
                 "The (rnrs control (6)) library provides useful control structures.")
                ("records.tex"
                 60
                 "Records"
                 ("(rnrs records syntactic (6))" "(rnrs records procedural (6))" "(rnrs records inspection (6))")
                 "Abstractions for creating new data types representing records.")
                ("exc.tex"
                 70
                 "Exceptions and conditions"
                 ("(rnrs exceptions (6))")
                 "The (rnrs exceptions (6))library provides exception-handling and exception-raising constructs.")
                ("iocond.tex"
                 81
                 "Condition types"
                 #f
                 "I/O condtion types.")
                ("portio.tex"
                 82
                 "Port I/O"
                 ("(rnrs io ports (6))")
                 "The (rnrs io ports (6)) library defines an I/O layer for conventional, imperative buffered input and output.")
                ("convio.tex"
                 83
                 "Simple I/O"
                 ("(rnrs io simple (6))")
                 "The (rnrs io simple (6)) library provides a somewhat more convenient interface for performing textual I/O on ports.")
                ("files.tex"
                 90
                 "File system"
                 ("(rnrs files (6))")
                 "The (rnrs files (6)) library for operations on the file system.")
                ("programlib.tex"
                 100
                 "Command-line access and exit values"
                 ("(rnrs programs (6))")
                 "Command-line access and exit values")
                ("arith.tex"
                 110
                 "Arithmetic"
                 #f
                 "More specialized numerical operations: fixnum and flonum arithmetic, as well as bitwise operations on exact integer objects.")
                ("syntax-case.tex"
                 120
                 "syntax-case"
                 ("(rnrs syntax-case (6))")
                 "The (rnrs syntax-case (6))library provides support for writing low-level macros in a high-level style, with automatic syntax checking, input destructuring, output restructuring, maintenance of lexical scoping and referential transparency (hygiene), and support for controlled identifier capture.")
                ("hashtable.tex"
                 130
                 "Hashtables"
                 ("(rnrs hashtables (6))")
                 "The (rnrs hashtables (6)) library provides a set of operations on hashtables.")
                ("enum.tex"
                 140
                 "Enumerations"
                 ("(rnrs enums (6))")
                 "The (rnrs enums (6)) library for dealing with enumerated values and sets of enumerated values.")
                ("eval.tex"
                 160
                 "eval"
                 ("(rnrs eval (6))")
                 "The (rnrs eval (6)) library allows a program to create Scheme expressions as data at run time and evaluate them.")
                ("setcar.tex"
                 170
                 "Mutable pairs"
                 ("(rnrs mutable-pairs (6))")
                 "The (rnrs mutable-pairs (6)) library allow new values to be assigned to the car and cdr fields of previously allocated pairs.")
                ("stringset.tex"
                 180
                 "Mutable strings"
                 ("(rnrs mutable-strings (6))")
                 "The string-set! procedure provided by the (rnrs mutable-strings (6)) library allows mutating the characters of a string in-place.")
                ("r5rscompat.tex"
                 190
                 "R5RS compatibility"
                 ("(rnrs r5rs (6))")
                 "The (rnrs r5rs (6)) library provides some functionality of the preceding revision of this report that was omitted from the main part of the current report.")
                ))


(define (adjust-args args-string)
  (if args-string
      (map string->symbol
           (map (lambda (x)
                  (cond
                   [(string=? x "\\dotsfoo") "..."]
                   [(string=? x "$\\ldots$") "..."]
                   ;; hyper{test} => test
                   ;; hyperi{exp} => exp1
                   ;; hyperii{exp} => exp2
                   [(#/hyper(i*){(.*)}/ x) => (lambda (m)
                                                (if (m 1) ;; count i
                                                    (format "~a~a" (m 2) (string-length (m 1)))
                                                    (m 2)))]
                   [(#/var(i*){(.*)}/ x) => (lambda (m)
                                              (if (m 1) ;; count i
                                                  (format "~a~a" (m 2) (string-length (m 1)))
                                                  (m 2)))]
                   [(#/varn{(.*)}/ x) => (lambda (m) (format "~an" (m 1)))]
                   [else x]))
                (remp (cut string=? <> "") (string-split args-string #\space))))
      '()))

(define (file->prot* file)
  (let ([line* (file->list file)])
     (map
      (lambda (x)
        (list (string->symbol (x 1))
              (adjust-args (x 2))))
      (remq #f (append (map #/proto{([^}]+)}{(.*)}{procedure}/ line*)
                       (map #/proto{([^}]+)}{(.*)}{.+exprtype}/ line*))

     ))))

(for-each
 (lambda (file)
   (format (current-error-port) "~a\n" (first file))
   (with-output-to-file (string-append out-dir (first file) ".txt")
     (lambda ()
   (let ([proto* (file->prot* (string-append r6rs-dir (first file)))])
     (format #t "Title: ~a\n\n~a\n\n" (third file) (fifth file))
     (if (fourth file) ;; library-name
         (for-each (lambda (x) (format #t "library: ~a\n\n" x)) (fourth file))
         (format (current-error-port) "~a : library name not specified\n" (first file)))
     (for-each
      (lambda (prot)
        (format (current-error-port) "~a\n" prot)
        (format #t
"Function: ~a

See <~a>

Prototype:
> ~a\n\n\n"
        (car prot) 
        (hashtable-ref symbols (car prot) #f)
        `(,(car prot) ,@(cadr prot)))
        )
      proto*)
     ))))
 files)
