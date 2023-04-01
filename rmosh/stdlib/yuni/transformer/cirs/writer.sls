(library (yuni transformer cirs writer)
         (export cirs-write)
         (import (rnrs)
                 (srfi :8)
                 (shorten))

(define (cirs-write obj p)
  (define (cirs-exp->string obj)

    (define binops ;; (OP X Y Z ...)
      ;; * / % + - << >> bit-xor bit-and bit-or and or =
      ;; (* X Y Z ...) => (X * Y * Z) 
      ;; (pref X Y Z ...) => X->Y->Z ..
      ;; (fref X Y Z ...) => X.Y.Z
      '((* . " * ") (/ . " / ") (% . " % ") (+ . " + ")
                    (- . " - ") (<< . " << ") (>> . " >> ") (bit-xor . " ^ ")
                    (bit-and . " & ") (bit-or . " | ") (and . " && ")
                    (or . " || ") (= . " = ")
                    (pref . "->") (fref . ".")))

    (define compops ;; (OP X Y)
      ;; < > <= >= == !=  
      ;; *= /= %= add= sub= <<= >>= bit-and= bit-xor= bit-or=
      ;; (< X Y) => (X < Y)
      '((< . " < ") (> . " > ") (<= . " <= ") (>= . " >= ") (== . " == ")
                    (!= . " != ") (*= . " *= ") (/= . " /= ") (%= . " %= ") (add= . " += ")
                    (sub= . " -= ") (<<= . " <<= ") (>>= . " >>= ") (bit-xor= . " ^= ")
                    (bit-and= . " &= ") (bit-or= . " |= ")))

    ;; inc dec ++ -- not bit-not
    ;; (inc X) => X++
    ;; FIXME: (++ X) => ++X
    ;; (mref X) => (*(X))
    ;; (ptr X) => (&(X))
    (define uniops/pre
      '(
        #| ;; we cannot do this in R6RS...
        (++ . "++")
        (-- . "--")
        |#
        (mref . "*")
        (ptr . "&")))
    (define uniops/post
      '((inc . "++")
        (dec . "--")))
    (define (output-op l p)
      (define (fold-ops str rest*)
        (define (itr rest)
          (when (pair? rest)
            (display str p)
            (display (car rest) p)
            (itr (cdr rest))))
        (cond
          ((pair? rest*)
           (display "(" p)
           (display (car rest*) p)
           (itr (cdr rest*))
           (display ")" p))
          (else
            (display "()" p))))
      (let ((op (car l))
            (rest (cdr l)))
        (cond
          ((assoc op binops) 
           => (^x (let ((str (cdr x)))
                    (fold-ops 
                      str
                      (map cirs-exp->string rest)))))
          ((assoc op compops) 
           => (^x (let ((str (cdr x)))
                    ;; FIXME: check op length here..
                    (fold-ops 
                      str
                      (map cirs-exp->string rest)))))
          ((assoc op uniops/pre) 
           => (^x (let ((str (cdr x)))
                    (display str p)
                    (display (cirs-exp->string (car rest)) p))))
          ((assoc op uniops/post) 
           => (^x (let ((str (cdr x)))
                    (display (cirs-exp->string (car rest)) p)
                    (display str p))))
          ;; ((quote X) Y Z ...) => X(Y, Z); // function call
          ((and (list? op) (eq? 'quote (car op)))
           (let ((name (cirs-exp->string (cadr op)))
                 (args rest))
             (display name p)
             (fold-ops ", " (map cirs-exp->string args))))
          (else
            (case op
              ;; (comment X) => /* X */ 
              ((comment)
               (display " /* " p)
               (display (car rest) p)
               (display " */ " p))
              ;; (aref X Y Z ...) => (X)[Y][Z]
              ((aref)
               (let ((X (cirs-exp->string (car rest)))
                     (refs (map (^e (string-append "[" e "]"))
                                (map cirs-exp->string (cdr rest)))))
                 (display (string-append X refs) p)))
              ;; (seq X Y Z ...) => X , Y , Z
              ((seq)
               (fold-ops ", " (map cirs-exp->string rest)))
              ;; (? X Y Z) => X ? Y : Z
              ((?)
               (let ((X (cirs-exp->string (car rest)))
                     (Y (cirs-exp->string (cadr rest)))
                     (Z (cirs-exp->string (caddr rest))))
                 (display (string-append "(" X " ? " Y " : " Z ")") p)))
              ;; (sizeof X) => (sizeof(X))
              ((sizeof)
               (display op p)
               (display (string-append "(" (cirs-exp->string (car rest)) ")") p))
              ;; (cast X Y) => ((X)Y)
              ;; (cast (X Y Z) W) => ((X Y Z) W)
              ((cast)
               (let ((type (car rest))
                     (value (cirs-exp->string (cadr rest))))
                 (display "((" p)
                 (cond
                   ((list? type)
                    (for-each (^e (display e p))
                              (map cirs-exp->string type)))
                   (else
                     (display (cirs-exp->string type))))
                 (display ") " p)
                 (display value p)
                 (display ") " p)))
              (else
                (assertion-violation 'cirs-exp->string "invalid op" op)))))))

    (receive (p str) (open-string-output-port)
      ;; write
      (cond
        ((string? obj)
         (write obj p))
        ((or (symbol? obj) (number? obj))
         (display obj p))
        ((list? obj)
         (output-op obj p))
        (else
          (assertion-violation 'cirs-exp->string "invalid datum" obj)))
      ;; output
      (let ((out (str)))
        (close-port p)
        out)))
  (let ((current-indent 0))
    (define (indent+) (set! current-indent (+ 1 current-indent)))
    (define (indent-) (set! current-indent (- current-indent 1)))
    (define (out str)
      (display str p))
    (define (indent)
      (define (itr rest)
        (unless (= rest 0)
          (out "    ")
          (itr (- rest 1))))
      (itr current-indent))
    (define (outi str)
      (indent)
      (out str))
    (define (line str)
      (indent)
      (out str)
      (out "\n"))
    (define (out-exp obj)
      (out (cirs-exp->string obj)))
    (define (emit-block body-k)
      (out " {\n")
      (indent+)
      (body-k)
      (indent-)
      (indent)
      (out "} "))
    (define (put-form l)
      ;; struct/union members
      ;; (name type)
      ;; (name type ... (init value))
      ;; (name (bit pos) type ...)
      ;; (name (bit pos) type ... (init value))
      (define (emit-members m)
        (define (emit-member x)
          (indent)
          (let ((name (car x))
                (args (cdr x))
                (init #f)
                (bit #f))
            ;; consume args
            (for-each (^e 
                        (cond
                          ((symbol? e)
                           (out (symbol->string e))
                           (out " "))
                          ((list? e)
                           (let ((op (car e))
                                 (arg (cadr e)))
                             (case op
                               ((bit)
                                (set! bit arg))
                               ((init)
                                (set! init arg))
                               (else
                                 (assertion-violation 'emit-members "invalid op"
                                                      e)))))))
                      args)
            ;; emit name
            (out (symbol->string name))
            ;; emit bit
            (when bit
              (out ":")
              (out (number->string bit)))
            ;; emit init
            (when init
              (out " = ")
              (put-form init))
            (out ";\n")))
        (for-each emit-member m))
      (define (put-begin obj)
        (for-each put-form obj))
      (define (complain x)
        (assertion-violation 'put-form "invalid form" x))
      (if (pair? l)
        (let ((op (car l))
              (rest (cdr l)))
          (case op
            ((if)
             (outi "if (")
             (out-exp (car rest))
             (out ")")
             (case (length rest)
               ;; (if P X) => if (P) { X }
               ((2)
                (emit-block (^[] (put-form (cadr rest)))))
               ;; (if P X Y) => if (P) { X } else { Y }
               ((3)
                (emit-block (^[] (put-form (cadr rest))))
                (out " else ")
                (emit-block (^[] (put-form (caddr rest)))))
               (else (complain l))))
            ((begin)
             (put-begin rest))

            ;; (cond (P X) (P Y) ... (else Z))
            ((cond)
             (for-each (^e
                         (if (list? e)
                           (let ((op (car e))
                                 (code (cdr e)))
                             (cond 
                               ((eq? op 'else)
                                (out " else ")
                                (emit-block (^[] (put-begin code))))
                               (else
                                 (out " else if (")
                                 (out-exp op)
                                 (out ")")
                                 (emit-block (^[] (put-begin code))))))
                           (complain e)))
                       rest))

            ;; (switch P (X Y ...) (X Y ...) ... (default Y ...))
            ;;   => switch (P) { case X: Y ,,, ... default: Y ,,, }
            ((switch)
             (let ((pred (car rest))
                   (cases (cdr rest)))
               (outi "switch (")
               (out-exp pred)
               (out ")")
               (emit-block (^[]
                             (for-each (^e (if (list? e)
                                             (let ((op (car e))
                                                   (code (cdr e)))
                                               (if (eq? op 'default)
                                                 (line "default:")
                                                 (line (string-append
                                                         (cirs-exp->string op)
                                                         ":")))
                                               (put-begin code))
                                             (complain e)))
                                       cases)))))

            ;; (while P ...) => while (P) { ... }
            ((while)
             (outi "while (")
             (out-exp (car rest))
             (out ")")
             (emit-block (^[] (put-begin (cdr rest)))))

            ;; (do-while P ...) => do { ... } while (P) ;
            ((do-while)
             (outi "do ")
             (emit-block (^[] (put-begin (cdr rest))))
             (out " while (")
             (out-exp (car rest))
             (out ");\n"))

            ;; (for (X Y Z) ...) => for ( X ; Y ; Z ) { ... }
            ;; (for (#f #f #f) ...) => for ( ;; ) { ... }
            ((for)
             (let ((cntl (car rest))
                   (code (cdr rest)))
               (if (= (length cntl) 3)
                 (let ((x (car cntl))
                       (y (cadr cntl))
                       (z (caddr cntl)))
                   (outi "for (")
                   (when x (out-exp x))
                   (out " ; ")
                   (when y (out-exp y))
                   (out " ; ")
                   (when z (out-exp z))
                   (out ")")
                   (emit-block (^[] (put-begin code))))
                 (complain cntl))))

            ;; (goto X) => goto X;
            ((goto)
             (outi "goto ")
             (out-exp (car rest))
             (out ";\n"))

            ;; (continue) => continue;
            ((continue)
             (outi "continue;\n"))

            ;; (break) => break;
            ((break)
             (outi "break;\n"))

            ;; (return) => return;
            ;; (return X) => return(X);
            ((return)
             (cond
               ((null? rest)
                (outi "return;\n"))
               (else
                 (outi "return ")
                 (out-exp (car rest))
                 (out ";\n"))))

            ;; (label X) => X:
            ((label)
             ;; labels won't be indented
             (out-exp (car rest))
             (out " :\n"))

            ;; struct / union
            ;; FIXME: (struct TAG (DECL ...) MEMBER ...)
            ;; (struct TAG DECL MEMBER ...)
            ;;  => struct TAG { ... } DECL ... ;
            ((struct union)
             (let ((name op)
                   (tag (car rest))
                   (decl (cadr rest))
                   (members (cddr rest)))
               (case name
                 ((struct)
                  (outi "struct "))
                 ((union)
                  (outi "union ")))
               (emit-block (^[] (emit-members members)))
               (out-exp decl)
               (out "; \n")))

            ;; (def NAME DEF attribute ...)
            ;; (def NAME DEF attribute ... (init x))
            ;; (decl NAME DEF attribute ...)
            ;; (decl NAME struct attribute ...)
            ;; (decl NAME union attribute ...)
            ((def decl)
             (let ((definition (cadr rest))
                   (name (car rest))
                   (body (cddr rest)))
               (cond
                 ((symbol? definition)
                  ;; forward definition of union/struct cannot contain any
                  ;; attributes
                  (case definition
                    ((struct union)
                     (outi (symbol->string definition))
                     (out " ")
                     (out (symbol->string name))
                     (out ";\n"))))
                 (else
                   (let ((init #f))
                     (for-each (^e (out (symbol->string e))
                                   (out " "))
                               definition)
                     (out (symbol->string name))
                     (for-each (^e 
                                 (let ((op (car e))
                                       (args (cdr e)))
                                   (case op
                                     ((init)
                                      (set! init (car args))))))
                               body)
                     (when init
                       (out " = ")
                       (out (cirs-exp->string init)))
                     (out "; \n"))))))

            ;; (defn NAME RETTYPE DEF attributes ... body ...)
            ;; DEF: ((NAME def ...) ...)
            ((defn)
             (let ((name (car rest))
                   (rettype (cadr rest))
                   (def (caddr rest))
                   (body (cdddr rest)))
               (define (out-types l)
                 (cond 
                   ((list? l)
                    (for-each (^e 
                                (out (symbol->string e))
                                (out " "))
                              l))
                   (else
                     (out (symbol->string l))
                     (out " "))))
               (define (out-def r)
                 (define (out-entry e)
                   (let ((name (car e))
                         (types (cdr e)))
                     (out-types types)
                     (out (symbol->string name))))
                 (let ((head (car r))
                       (tail (cdr r)))
                   (out-entry head)
                   (unless (null? tail)
                     (out ", ")
                     (out-def tail))))

               ;; output
               (indent)
               (out-types rettype)
               (out "\n")
               (outi (symbol->string name))
               (cond
                 ((null? def)
                  (out "(void)"))
                 (else
                   (out "(")
                   (out-def def)
                   (out ")")))
               (emit-block (^[] (put-begin body)))
               (out "\n")))
            #|
            ;; (deftype NAME DEF) => typedef DEF name;
            ((deftype)
             )
            |#

            (else
              (indent)
              (out-exp l)
              (out ";\n")
              )))
        (out (cirs-exp->string l))))
    (for-each put-form obj)))
    ;; === keywords ===
    ;; (array X Y Z ...) => { X , Y , Z , ... }
    ;; (attribute A X) => UNSPEC
    ;; (cpp-if P X Y)
    ;; (cpp-cond (P X) (P Y) ... (else Z))
    ;; (cpp-ifdef S X Y)
    ;; (cpp-ifndef S X Y)
    ;; (cpp-include FN)

)
