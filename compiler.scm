(cond-expand
 [gauche
  (use srfi-1)
  (use util.match)
  (load "./free-vars-decl.scm")
  (define dd display)
  (define pp print)
  (define open-string-input-port open-input-string)
  (define make-eq-hashtable make-hash-table)
  (define hashtable-set! hash-table-put!)
  (define hashtable-ref hash-table-get)
  (define find10 find)
  (define foldr2 fold-right)
  (define source-info debug-source-info)
  (define syntax-error error)
  (define (set-source-info! a b) a)]
 [vm?
  (define-macro (import-only module . syms)
    `(begin
       ,@(map (lambda (sym) `(define ,sym (with-module ,module ,sym))) syms)))
  (import-only gauche.internal extended-pair? extended-cons extended-list pair-attribute-get pair-attribute-set! pair-attributes)
  (define *command-line-args* '())
  (define (command-line) *command-line-args*)
  (define make-eq-hashtable make-hash-table)
  (define hashtable-set! hash-table-put!)
  (define hashtable-ref hash-table-get)
  (define dd (lambda a '()))
  (define pp (lambda a '()))
  (define syntax-error error)
  (define find10 find)
  (define append2 append)
  (define appendA append)
  (define memq2 memq)
  (define (source-info p) (let1 src (debug-source-info p) (if (pair? src) (cons (sys-basename (car src)) (cdr src)) src)))
  (define (make-list-with-src-slot lst) (apply extended-list lst))
  (define (set-source-info! a b)
    (cond
     [(extended-pair? a)
       (pair-attribute-set! a 'source-info b)
       a]
     [else
      a]))]
 [vm-outer?
  (define dd (lambda a '()))
  (define pp (lambda a '()))
  (include "./free-vars-decl.scm")
  (define find10 find)
  (define syntax-error error)
  (define (command-line) *command-line-args*)]
[mosh
  (define dd display)
  (define pp print)
  (include "./free-vars-decl.scm")
  (define-macro (make-list-with-src-slot lst) lst)
  (define (command-line) *command-line-args*)])

(define (eq-hashtable-copy ht)
  (let1 ret (make-eq-hashtable)
    (hashtable-for-each
     (lambda (key value)
       (hashtable-set! ret key value))
     ht)
    ret))

(define ($for-each1-with-rindex proc lst)
  (let loop ([i (- (length lst) 1)]
             [lst lst])
    (cond
     [(null? lst) '()]
     [else
      (proc i (car lst))
      (loop (- i 1) (cdr lst))])))

(define-macro (begin0 form . forms)
  (let ((var (gensym)))
    `(let ((,var ,form)) ,@forms ,var)))

(define-macro (first o)
  `(car ,o))

(define-macro (second o)
  `(cadr ,o))

(define-macro (third o)
  `(caddr ,o))

(define-macro (dolist a . body)
  `(begin (for-each (lambda (,(first a)) ,@body) ,(second a)) '()))

(define-macro (do . sexp)
  (match sexp
    [(((var init step ...) ...)
         (test expr ...)
       command ...)
     `(letrec
       ((loop
         (lambda (,@var)
           (if ,test
               (begin
                 #f ; avoid empty begin
                 ,@expr)
               (begin
                 ,@command
                 (loop ,@(map (lambda (v s) `(do "step" ,v ,@s)) var step)))))))
        (loop ,@init))]
    [("step" x)
     x]
    [("step" x y)
     y]
    [else
     (syntax-error "malformed do")]))

(define-macro (acond . clauses)
  (if (null? clauses)
      '()
      (let ((cl1 (car clauses))
            (sym (gensym)))
        `(let ((,sym ,(car cl1)))
           (if ,sym
               (let ((it ,sym)) ,@(cdr cl1))
               (acond ,@(cdr clauses)))))))

(define-macro (aif test-form then-form . else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,@else-form)))

(define (syntax-error msg)
  (raise (format "syntax error: ~a" msg)))

(define (acons obj1 obj2 obj3) (cons (cons obj1 obj2) obj3))

(define (libname->symbol name)
  (let loop ([name name]
             [ret  ""])
    (if (null? name)
        (string->symbol ret)
        (loop (cdr name) (string-append ret (symbol->string (car name)) " ")))))

;; moved to freeproc.cpp
;; N.B. this procedure is still required by vm.scm
;; (define (%set-union l1 l2)
;;   (define (set-cons x lst)
;;     (if (memq x lst)
;;         lst
;;         (cons x lst)))
;;   (define (rec lst1 lst2)
;;     (cond
;;      [(null? lst1) lst2]
;;      [(null? lst2) lst1]
;;      [else
;;       (rec (cdr lst1) (set-cons (car lst1) lst2))]))
;;   (rec l1 l2))

;; (define (%set-intersect lst1 lst2)
;;   (if (null? lst1)
;;       '()
;;       (if (memq2 (car lst1) lst2)
;;           (cons (car lst1) (%set-intersect (cdr lst1) lst2))
;;           (%set-intersect (cdr lst1) lst2))))

;;--------------------------------------------------------------------
;;
;; Generic
;;
;; (define (log . msg)
;;   (display msg (current-error-port)))

(define ($map1 f l)
  (if (null? l)
      l
      (cons (f (car l)) ($map1 f (cdr l)))))

(define ($filter-map1 f l)
  (if (null? l)
      l
      (aif (f (car l))
           (cons it ($filter-map1 f (cdr l)))
           ($filter-map1 f (cdr l)))))

(define ($map1-with-tail f l)
  (if (null? l)
      l
      (cons (f (car l) (null? (cdr l))) ($map1-with-tail f (cdr l)))))

(define-macro ($append-map1 f l)
  `(apply append ($map1 ,f ,l)))

(define (uniq lst)
  (let loop ([lst lst]
             [ret '()])
    (cond
     [(null? lst)
      ret]
     [else
      (if (memq (car lst) ret)
          (loop (cdr lst) ret)
          (loop (cdr lst) (cons (car lst) ret)))])))

;;  apply proc for each pair
;;    lst (a b c d)
;;    => ((proc a b) (proc b c) (proc c d))
(define (apply-each-pair proc lst)
  (if (null? (cdr lst))
      '()
      (cons (list proc (car lst) (cadr lst))
            (apply-each-pair proc (cdr lst)))))

(define (remove-tail lst pred)
  (let loop ([lst lst]
             [ret '()])
    (cond
     [(null? (cdr lst))
      (reverse
       (if (pred (car lst))
           ret
           (cons (car lst) ret)))]
     [else
      (loop (cdr lst) (cons (car lst) ret))])))

;;--------------------------------------------------------------------
;;
;; IForm Structs
;;
;; Do not edit. This file is generated by ./gen-struct.scm

;; struct $const
(define $CONST 0)
(define ($const val)
  `#(,$CONST ,val ))

(define-macro ($const.val iform) `(vector-ref ,iform 1))
(define-macro ($const.set-val! iform val) `(vector-set! ,iform 1 ,val))

;; struct $lvar
(define $LVAR 1)
(define ($lvar sym init-val ref-count set-count)
  `#(,$LVAR ,sym ,init-val ,ref-count ,set-count ))

(define-macro ($lvar.sym iform) `(vector-ref ,iform 1))
(define-macro ($lvar.init-val iform) `(vector-ref ,iform 2))
(define-macro ($lvar.ref-count iform) `(vector-ref ,iform 3))
(define-macro ($lvar.set-count iform) `(vector-ref ,iform 4))
(define-macro ($lvar.set-sym! iform sym) `(vector-set! ,iform 1 ,sym))
(define-macro ($lvar.set-init-val! iform init-val) `(vector-set! ,iform 2 ,init-val))
(define-macro ($lvar.set-ref-count! iform ref-count) `(vector-set! ,iform 3 ,ref-count))
(define-macro ($lvar.set-set-count! iform set-count) `(vector-set! ,iform 4 ,set-count))

(define (pp-lvars lvars)
  (print (map (lambda (x) ($lvar.sym x)) lvars)))

;; struct $let
(define $LET 2)
(define ($let type lvars inits body tail? src)
  `#(,$LET ,type ,lvars ,inits ,body ,tail? ,src))

(define-macro ($let.type iform) `(vector-ref ,iform 1))
(define-macro ($let.lvars iform) `(vector-ref ,iform 2))
(define-macro ($let.inits iform) `(vector-ref ,iform 3))
(define-macro ($let.body iform) `(vector-ref ,iform 4))
(define-macro ($let.tail? iform) `(vector-ref ,iform 5))
(define-macro ($let.src iform) `(vector-ref ,iform 6))
(define-macro ($let.set-type! iform type) `(vector-set! ,iform 1 ,type))
(define-macro ($let.set-lvars! iform lvars) `(vector-set! ,iform 2 ,lvars))
(define-macro ($let.set-inits! iform inits) `(vector-set! ,iform 3 ,inits))
(define-macro ($let.set-body! iform body) `(vector-set! ,iform 4 ,body))
(define-macro ($let.set-tail?! iform tail?) `(vector-set! ,iform 5 ,tail?))
(define-macro ($let.set-src! iform src) `(vector-set! ,iform 6 ,src))

;; struct $seq
(define $SEQ 3)
(define ($seq body tail?)
  `#(,$SEQ ,body ,tail? ))

(define-macro ($seq.body iform) `(vector-ref ,iform 1))
(define-macro ($seq.tail? iform) `(vector-ref ,iform 2))
(define-macro ($seq.set-body! iform body) `(vector-set! ,iform 1 ,body))
(define-macro ($seq.set-tail?! iform tail?) `(vector-set! ,iform 2 ,tail?))

;; struct $lambda
(define $LAMBDA 4)
(define ($lambda src name reqargs optarg lvars body flag calls)
  `#(,$LAMBDA ,src ,name ,reqargs ,optarg ,lvars ,body ,flag ,calls ))

(define-macro ($lambda.src iform) `(vector-ref ,iform 1))
(define-macro ($lambda.name iform) `(vector-ref ,iform 2))
(define-macro ($lambda.reqargs iform) `(vector-ref ,iform 3))
(define-macro ($lambda.optarg iform) `(vector-ref ,iform 4))
(define-macro ($lambda.lvars iform) `(vector-ref ,iform 5))
(define-macro ($lambda.body iform) `(vector-ref ,iform 6))
(define-macro ($lambda.flag iform) `(vector-ref ,iform 7))
(define-macro ($lambda.calls iform) `(vector-ref ,iform 8))
(define-macro ($lambda.set-src! iform src) `(vector-set! ,iform 1 ,src))
(define-macro ($lambda.set-name! iform name) `(vector-set! ,iform 2 ,name))
(define-macro ($lambda.set-reqargs! iform reqargs) `(vector-set! ,iform 3 ,reqargs))
(define-macro ($lambda.set-optarg! iform optarg) `(vector-set! ,iform 4 ,optarg))
(define-macro ($lambda.set-lvars! iform lvars) `(vector-set! ,iform 5 ,lvars))
(define-macro ($lambda.set-body! iform body) `(vector-set! ,iform 6 ,body))
(define-macro ($lambda.set-flag! iform flag) `(vector-set! ,iform 7 ,flag))
(define-macro ($lambda.set-calls! iform calls) `(vector-set! ,iform 8 ,calls))

;; struct $local-ref
(define $LOCAL-REF 5)
(define ($local-ref lvar)
  `#(,$LOCAL-REF ,lvar ))

(define-macro ($local-ref.lvar iform) `(vector-ref ,iform 1))
(define-macro ($local-ref.set-lvar! iform lvar) `(vector-set! ,iform 1 ,lvar))


;; struct $local-assign
(define $LOCAL-ASSIGN 6)
(define ($local-assign lvar val)
  `#(,$LOCAL-ASSIGN ,lvar ,val ))

(define-macro ($local-assign.lvar iform) `(vector-ref ,iform 1))
(define-macro ($local-assign.val iform) `(vector-ref ,iform 2))
(define-macro ($local-assign.set-lvar! iform lvar) `(vector-set! ,iform 1 ,lvar))
(define-macro ($local-assign.set-val! iform val) `(vector-set! ,iform 2 ,val))

;; struct $global-ref
(define $GLOBAL-REF 7)
(define ($global-ref libname sym)
  `#(,$GLOBAL-REF ,libname ,sym ))

(define-macro ($global-ref.libname iform) `(vector-ref ,iform 1))
(define-macro ($global-ref.sym iform) `(vector-ref ,iform 2))
(define-macro ($global-ref.set-libname! iform libname) `(vector-set! ,iform 1 ,libname))
(define-macro ($global-ref.set-sym! iform sym) `(vector-set! ,iform 2 ,sym))

;; struct $global-assign
(define $GLOBAL-ASSIGN 8)
(define ($global-assign libname sym val)
  `#(,$GLOBAL-ASSIGN ,libname ,sym ,val ))

(define-macro ($global-assign.libname iform) `(vector-ref ,iform 1))
(define-macro ($global-assign.sym iform) `(vector-ref ,iform 2))
(define-macro ($global-assign.val iform) `(vector-ref ,iform 3))
(define-macro ($global-assign.set-libname! iform libname) `(vector-set! ,iform 1 ,libname))
(define-macro ($global-assign.set-sym! iform sym) `(vector-set! ,iform 2 ,sym))
(define-macro ($global-assign.set-val! iform val) `(vector-set! ,iform 3 ,val))

;; struct $undef
(define $UNDEF 9)
(define ($undef) (make-vector 1 $UNDEF))

;; struct $if
(define $IF 10)
(define ($if test then else)
  `#(,$IF ,test ,then ,else ))

(define-macro ($if.test iform) `(vector-ref ,iform 1))
(define-macro ($if.then iform) `(vector-ref ,iform 2))
(define-macro ($if.else iform) `(vector-ref ,iform 3))
(define-macro ($if.set-test! iform test) `(vector-set! ,iform 1 ,test))
(define-macro ($if.set-then! iform then) `(vector-set! ,iform 2 ,then))
(define-macro ($if.set-else! iform else) `(vector-set! ,iform 3 ,else))

;; struct $asm
(define $ASM 11)
(define ($asm insn args)
  `#(,$ASM ,insn ,args ))

(define-macro ($asm.insn iform) `(vector-ref ,iform 1))
(define-macro ($asm.args iform) `(vector-ref ,iform 2))
(define-macro ($asm.set-insn! iform insn) `(vector-set! ,iform 1 ,insn))
(define-macro ($asm.set-args! iform args) `(vector-set! ,iform 2 ,args))

;; struct $define
(define $DEFINE 12)
(define ($define libname sym val)
  `#(,$DEFINE ,libname ,sym ,val ))

(define-macro ($define.libname iform) `(vector-ref ,iform 1))
(define-macro ($define.sym iform) `(vector-ref ,iform 2))
(define-macro ($define.val iform) `(vector-ref ,iform 3))
(define-macro ($define.set-libname! iform libname) `(vector-set! ,iform 1 ,libname))
(define-macro ($define.set-sym! iform sym) `(vector-set! ,iform 2 ,sym))
(define-macro ($define.set-val! iform val) `(vector-set! ,iform 3 ,val))

;; struct $call-cc
(define $CALL-CC 13)
(define ($call-cc proc tail?)
  `#(,$CALL-CC ,proc ,tail? ))

(define-macro ($call-cc.proc iform) `(vector-ref ,iform 1))
(define-macro ($call-cc.tail? iform) `(vector-ref ,iform 2))
(define-macro ($call-cc.set-proc! iform proc) `(vector-set! ,iform 1 ,proc))
(define-macro ($call-cc.set-tail?! iform tail?) `(vector-set! ,iform 2 ,tail?))

;; struct $call
(define $CALL 14)
(define ($call proc args tail? type)
  `#(,$CALL ,proc ,args ,tail? ,type ))

(define-macro ($call.proc iform) `(vector-ref ,iform 1))
(define-macro ($call.args iform) `(vector-ref ,iform 2))
(define-macro ($call.tail? iform) `(vector-ref ,iform 3))
(define-macro ($call.type iform) `(vector-ref ,iform 4))
(define-macro ($call.set-proc! iform proc) `(vector-set! ,iform 1 ,proc))
(define-macro ($call.set-args! iform args) `(vector-set! ,iform 2 ,args))
(define-macro ($call.set-tail?! iform tail?) `(vector-set! ,iform 3 ,tail?))
(define-macro ($call.set-type! iform type) `(vector-set! ,iform 4 ,type))

;; struct $label
(define $LABEL 15)
(define ($label label body)
  `#(,$LABEL ,label ,body ))

(define-macro ($label.label iform) `(vector-ref ,iform 1))
(define-macro ($label.body iform) `(vector-ref ,iform 2))
(define-macro ($label.set-label! iform label) `(vector-set! ,iform 1 ,label))
(define-macro ($label.set-body! iform body) `(vector-set! ,iform 2 ,body))
(define-macro (make-label) `($label #f #f))
(define-macro (ref-label l) l)

;; struct $list
(define $LIST 16)
(define ($list args)
  `#(,$LIST ,args ))

(define-macro ($list.args iform) `(vector-ref ,iform 1))
(define-macro ($list.set-args! iform args) `(vector-set! ,iform 1 ,args))


;; struct $library
(define $LIBRARY 17)
(define ($library name export-syms import-syms import macro body compiled-body)
  `#(,$LIBRARY ,(libname->symbol name) ,export-syms ,import-syms ,import ,macro ,body ,compiled-body ))

(define-macro ($library.name iform) `(vector-ref ,iform 1))
(define-macro ($library.export-syms iform) `(vector-ref ,iform 2))
(define-macro ($library.import-syms iform) `(vector-ref ,iform 3))
(define-macro ($library.import iform) `(vector-ref ,iform 4))
(define-macro ($library.macro iform) `(vector-ref ,iform 5))
(define-macro ($library.body iform) `(vector-ref ,iform 6))
(define-macro ($library.compiled-body iform) `(vector-ref ,iform 7))
(define-macro ($library.set-name! iform name) `(vector-set! ,iform 1 ,name))
(define-macro ($library.set-export-syms! iform export-syms) `(vector-set! ,iform 2 ,export-syms))
(define-macro ($library.set-import-syms! iform import-syms) `(vector-set! ,iform 3 ,import-syms))
(define-macro ($library.set-import! iform import) `(vector-set! ,iform 4 ,import))
(define-macro ($library.set-macro! iform macro) `(vector-set! ,iform 5 ,macro))
(define-macro ($library.set-body! iform body) `(vector-set! ,iform 6 ,body))
(define-macro ($library.set-compiled-body! iform compiled-body) `(vector-set! ,iform 7 ,compiled-body))


;; struct $import
(define $IMPORT 18)
(define ($import import-specs)
  (let1 v (make-vector 2)
    (vector-set! v 0 $IMPORT)
    (vector-set! v 1 import-specs)
    v))

;  `#(,$IMPORT ,import-specs ))

(define-macro ($import.import-specs iform) `(vector-ref ,iform 1))
(define-macro ($import.set-import-specs! iform import-specs) `(vector-set! ,iform 1 ,import-specs))


;; struct $import-spec
(define $IMPORT-SPEC 19)
(define ($import-spec libname level)
  `#(,$IMPORT-SPEC ,libname ,level ))

(define-macro ($import-spec.libname iform) `(vector-ref ,iform 1))
(define-macro ($import-spec.level iform) `(vector-ref ,iform 2))
(define-macro ($import-spec.set-libname! iform libname) `(vector-set! ,iform 1 ,libname))
(define-macro ($import-spec.set-level! iform level) `(vector-set! ,iform 2 ,level))

(define $IT 20)
(define ($it)
  (make-vector 1 $IT))

;; struct $receive
(define $RECEIVE 21)
(define ($receive lvars reqargs optarg vals body tail?)
  `#(,$RECEIVE ,lvars ,reqargs ,optarg ,vals ,body ,tail?))

(define-macro ($receive.lvars iform) `(vector-ref ,iform 1))
(define-macro ($receive.reqargs iform) `(vector-ref ,iform 2))
(define-macro ($receive.optarg iform) `(vector-ref ,iform 3))
(define-macro ($receive.vals iform) `(vector-ref ,iform 4))
(define-macro ($receive.body iform) `(vector-ref ,iform 5))
(define-macro ($receive.tail? iform) `(vector-ref ,iform 6))
(define-macro ($receive.set-lvars! iform lvars) `(vector-set! ,iform 1 ,lvars))
(define-macro ($receive.set-vals! iform body) `(vector-set! ,iform 4 ,body))
(define-macro ($receive.set-body! iform body) `(vector-set! ,iform 5 ,body))
(define-macro ($receive.set-tail?! iform tail?) `(vector-set! ,iform 6 ,tail?))

(define $INSN-NUM 22)

(define-macro (tag iform)
  `(vector-ref ,iform 0))

(define-macro (tag? iform t)
  `(= ,t (tag ,iform)))

(define-macro (set-tag! iform t)
  `(vector-set! ,iform 0 ,t))

(define-macro (make-lvar sym)
  `($lvar ,sym '() 0 0))

(define-macro ($lvar.ref-count++! lvar)
  `($lvar.set-ref-count! ,lvar (+ ($lvar.ref-count ,lvar) 1)))

(define-macro ($lvar.ref-count--! lvar)
  `($lvar.set-ref-count! ,lvar (- ($lvar.ref-count ,lvar) 1)))

(define-macro ($lvar.set-count++! lvar)
  `($lvar.set-set-count! ,lvar (+ 1 ($lvar.set-count ,lvar))))

(define-macro ($local-ref.copy dst src)
  `($local-ref.set-lvar! ,dst ($local-ref.lvar ,src)))

(define-macro ($library.add-import-syms! library import-syms)
  `($library.set-import-syms! ,library (append2 ($library.import-syms ,library) ,import-syms)))

(define-macro ($library.add-import! library import)
  `($library.set-import! ,library (append2 ($library.import ,library) (list ,import))))

;;--------------------------------------------------------------------
;;
;; Pass1
;;

(define (make-empty-library name)
  ($library name '() '() '() '() '() #f))

(define top-level-library (make-empty-library '(top level)))

;; Parse lambda vars and return (optional-arg? . vars).
;;   a       => (#t (a))
;;   (a b)   => (#f (a b))
;;   (a . b) => (#t (a b))
(define (parse-lambda-vars vars)
  (cond ((pair? vars)
         (let loop ((p vars) (ret '()))
           (cond ((null? p) (list #f vars))
                 ((pair? p) (loop (cdr p) (cons (car p) ret)))
                 (else
                  (list #t (reverse (cons p ret)))))))
        ((null? vars)
         (list #f '()))
        (else
         (list #t (list vars)))))

;; 後でこれに統一
(define (parse-lambda-args formals)
  (let loop ((formals formals) (args '()))
    (cond ((null? formals) (values (reverse args) (length args) 0))
          ((pair? formals) (loop (cdr formals) (cons (car formals) args)))
          (else (values (reverse (cons formals args)) (length args) 1)))))

;;--------------------------------------------------------------------
;;
;; Pass1: Code expansion
;;
(define-macro ($src x sexp)
  `(set-source-info! (make-list-with-src-slot ,x) (source-info ,sexp)))

(define (pass1/expand sexp)
  (define (lambda-has-define? sexp)
    (and (not (null? (cddr sexp)))
         (pair? (third sexp))
         (eq? (car (third sexp)) 'define)))
  (define (let1->let sexp)
    `(let ((,(second sexp) ,(third sexp)))
       ,@(cdddr sexp)))
  (define (expand-let vars body)
    (let1 expanded-vars (fold-right (lambda (x y) (cons (list (first x) (pass1/expand (second x))) y)) '() vars)
      `(let ,expanded-vars ,@(pass1/expand body))))
  (cond
   ((pair? sexp)
    (case (first sexp)
      [(quote) sexp]
      [(define-macro) sexp]
      [(define)
       (if (define-is-lambda? sexp)
           (pass1/expand (define->lambda sexp))
           ($src ($map1 (lambda (s) (pass1/expand s)) sexp) sexp))]
      [(let1)
       ($src (pass1/expand (let1->let sexp)) sexp)]
      [(let)
       (if (let-is-named? sexp)
           ($src (pass1/expand (named-let->letrec sexp)) sexp)
           ($src (expand-let (second sexp) (cddr sexp)) sexp))]
      [(let*)
       ($src (pass1/expand (let*->let sexp)) sexp)]
      [(cond)
       ($src (pass1/expand (cond->if sexp)) sexp)]
      [(lambda)
       (cond [(lambda-has-define? sexp)
              ($src (pass1/expand ($src (internal-define->letrec sexp) sexp)) sexp)]
             [else
              ($src (append! (list 'lambda (cadr sexp)) (pass1/expand (cddr sexp))) sexp)])]
      [(when)
       (match sexp
         [('when pred body . more)
          ($src (pass1/expand `(cond (,pred ,body ,@more))) sexp)]
         [else
          (syntax-error "malformed when")])]
      [(unless)
       (match sexp
         [('unless pred body . more)
          ($src (pass1/expand `(cond ((not ,pred) ,body ,@more))) sexp)]
         [else
          (syntax-error "malformed unless")])]
      [(aif)
       ($src (pass1/expand (aif->let sexp)) sexp)]
      [(case)
       ($src (pass1/expand (case->cond sexp)) sexp)]
      [(quasiquote)
       (expand-quasiquote (cadr sexp) 0)]
      [else sexp])) ;; macro and call are expande later.
   (else sexp)))

(define (define-is-lambda? sexp)
  (pair? (cadr sexp)))

(define (conditions->if conditions)
  (if (null? (cdr conditions))
      (car conditions)
      (list 'if (car conditions) (conditions->if (cdr conditions)) #f)))

;;  (find-serial-from-head even? '(2 4 6 8 3 5))
;;   ==> ((2 4 6 8) (3 5))
;;
;;  (find-serial-from-head even? '(1 4 6 8 3 5))
;;   ==> (() (1 4 6 8 3 5))
;;
(define (find-serial-from-head pred lst)
  (let loop ([found '()]
             [lst   lst])
    (cond [(null? lst)
           (if (null? found) (list '() lst) (values found '()))]
          [(pred (car lst))
           (loop (append2 found (list (car lst))) (cdr lst))]
          [else
           (if (null? found) (list '() lst) (list found lst))])))

(define (internal-define->letrec sexp)
  (let* ([body (cddr sexp)]
         [args (second sexp)]
         [ret  (find-serial-from-head (lambda (s) (and (pair? s) (eq? 'define (car s)))) body)]
         [defines (first ret)]
         [rest (second ret)]
         [letrec-body ($src `(letrec ,(map (lambda (d) (list (second d) (third d))) (map pass1/expand defines))
                         ,@rest) sexp)])
    ($src `(lambda ,args
             ,letrec-body) sexp)))

(define (define->lambda sexp)
  (let ((args (cadr sexp))
        (body (cddr sexp)))
      `(define ,(car args) ,($src (append! (list 'lambda (cdr args)) body) sexp))))

(define (unless->cond sexp)
  `(cond ((not ,(cadr sexp)) ,@(cddr sexp))))

(define (let*->let sexp)
  (let ([args (cadr sexp)]
        [body (cddr sexp)])
    (car (let loop ([args args])
           (if (null? args)
               body
               ($src `((let (,(car args)) ,@(loop (cdr args)))) sexp))))))

(define (cond->if sexp)
  (define (make-if test then else)
    ;; don't use unquote splicing, it uses append
    (let ([then (if (> (length then) 1) (cons 'begin then) (car then))])
      `(if ,test ,then ,else)))
  (let loop ((clauses (cdr sexp)))
    (if (null? clauses)
        '#f
        (cond ((and (null? (cdr clauses)) (eq? 'else (caar clauses)))
               (if (> (length (cdar clauses)) 1)
                   (cons 'begin (cdar clauses))
                   (cadar clauses)))
              ((and (= 3 (length (car clauses))) (eq? '=> (cadar clauses)))
               (let ((tmp (gensym)))
                 `(let ((,tmp ,(caar clauses)))
                    (if ,tmp
                        (,(caddar clauses) ,tmp)
                        ,(loop (cdr clauses))))))
              ((= 1 (length (car clauses)))
               (let ((tmp (gensym)))
                 `(let ((,tmp ,(caar clauses)))
                    (if ,tmp
                        ,tmp
                        ,(loop (cdr clauses))))))
              (else
               (make-if (caar clauses) (cdar clauses) (loop (cdr clauses))))))))

;(cond->if '(cond ((null? n)) (else (loop (cdr n)))))

(define (case->cond sexp)
  (define (expand-clauses clauses tmpname)
    (let loop ([clauses clauses])
      (if (null? clauses)
          '()
          (if (eq? 'else (caar clauses))
              clauses
              (if (= 1 (length (caar clauses)))
                  (cons `((eqv? ',(caaar clauses) ,tmpname) ,@(cdar clauses)) (loop (cdr clauses)))
                  (cons `((memv ,tmpname ',(caar clauses)) ,@(cdar clauses)) (loop (cdr clauses))))))))
  (let* ([pred (cadr sexp)]
         [clauses (cddr sexp)]
         [tmpname (gensym)]
         [expanded-clauses (expand-clauses clauses tmpname)])
    `(let ((,tmpname ,pred))
       (cond
        ,@expanded-clauses))))

(define (named-let->letrec sexp)
  (let* ((name (cadr sexp))
         (args (caddr sexp))
         (vars ($map1 car args))
         (vals ($map1 cadr args))
         (body (cdddr sexp))
         (lambda-body ($src `(lambda ,vars ,@body) sexp)))
    ($src `(letrec ((,name ,lambda-body)) (,name ,@vals)) sexp)))

(define (aif->let sexp)
  `(let ((it ,(cadr sexp)))
     (if it ,(caddr sexp) ,(cadddr sexp))))

(define (let-is-named? sexp)
  (symbol? (cadr sexp)))

;;--------------------------------------------------------------------
;;
;;  Pass1: Quasiquote
;;
;;  based on bdc-scheme start
;;  Copyright (c) 1996-2002 Brian D. Carlstrom
;;
(define (expand-quasiquote x level)
  (define (finalize-quasiquote mode arg)
    (cond ((eq? mode 'quote) (list 'quote arg))
          ((eq? mode 'unquote) arg)
          ((eq? mode 'unquote-splicing)
           (error ",@ in invalid context" arg))
          (else (cons mode arg))))
  (define (descend-quasiquote x level return)
    (cond ((vector? x)
           (descend-quasiquote-vector x level return))
          ((not (pair? x))
           (return 'quote x))
          ((interesting-to-quasiquote? x 'quasiquote)
           (descend-quasiquote-pair x (+ level 1) return))
          ((interesting-to-quasiquote? x 'unquote)
           (cond ((= level 0)
                  (return 'unquote (cadr x)))
                 (else
                  (descend-quasiquote-pair x (- level 1) return))))
          ((interesting-to-quasiquote? x 'unquote-splicing)
           (cond ((= level 0)
                  (return 'unquote-splicing (cadr x)))
                 (else
                  (descend-quasiquote-pair x (- level 1) return))))
          (else
           (descend-quasiquote-pair x level return))))
  (define (descend-quasiquote-pair x level return)
    (descend-quasiquote (car x) level
                        (lambda (car-mode car-arg)
                          (descend-quasiquote (cdr x) level
                                              (lambda (cdr-mode cdr-arg)
                                                (cond ((and (eq? car-mode 'quote) (eq? cdr-mode 'quote))
                                                       (return 'quote x))
                                                      ((eq? car-mode 'unquote-splicing)
                                                       ;; (,@mumble ...)
                                                       (cond ((and (eq? cdr-mode 'quote) (null? cdr-arg))
                                                              (return 'unquote
                                                                      car-arg))
                                                             (else
                                                              (return 'append2
                                                                      (list car-arg (finalize-quasiquote
                                                                                     cdr-mode cdr-arg))))))
                                                      (else
                                                       (return 'cons
                                                               (list (finalize-quasiquote car-mode car-arg)
                                                                     (finalize-quasiquote cdr-mode cdr-arg))))))))))

  (define (descend-quasiquote-vector x level return)
    (descend-quasiquote (vector->list x) level
                        (lambda (mode arg)
                          (if (equal? mode 'quote)
                              (return 'quote x)
                              (return 'list->vector
                                      (list (finalize-quasiquote mode arg)))))))

  (define (interesting-to-quasiquote? x marker)
    (and (pair? x) (eq? (car x) marker)))

  (descend-quasiquote x level finalize-quasiquote))
;; based on bdc-scheme end

;;--------------------------------------------------------------------
;;
;;  Pass1: Sexp into IForm
;;
(define (pass1/lib-refer->iform symbol library)
  (let1 import-syms ($library.import-syms library)
    (aif (find10 (lambda (import) (eq? symbol (first import))) import-syms)
         ($global-ref (second it) (third it)) ;; bind found on import-syms.
         ($global-ref ($library.name library) symbol))))

(define (pass1/lib-assign->iform symbol library val)
  (let1 import-syms ($library.import-syms library)
    (aif (find10 (lambda (import) (eq? symbol (first import))) import-syms)
         ($global-assign (second it) (third it) val) ;; bind found on import-syms.
         ($global-assign ($library.name library) symbol val))))

(define (pass1/refer->iform symbol library lvars)
  (acond
   [(find10 (lambda (lvar) (eq? ($lvar.sym lvar) symbol)) lvars)
    ($lvar.ref-count++! it)
    ($local-ref it)]
   [(pass1/lib-refer->iform symbol library)
    it]
   [#t ($global-ref '(top level) symbol)]))

(define (pass1/assign->iform sexp library lvars tail?)
  (let* ([symbol (second sexp)]
         [val    (third sexp)]
         [iform  (pass1/sexp->iform val library lvars tail?)])
    (acond
     [(find10 (lambda (lvar) (eq? ($lvar.sym lvar) symbol)) lvars)
      ($lvar.set-count++! it)
      ($local-assign it iform)]
     [(pass1/lib-assign->iform symbol library iform)
      it]
     [#t ($global-assign '(top level) symbol iform)])))

(define (pass1/body->iform body library lvars tail?)
  (let1 iforms ($map1-with-tail
                (lambda (b t?) (pass1/sexp->iform (pass1/expand b) library lvars (and t? tail?))) body)
    (if (= 1 (length iforms))
        (car iforms)
        ($seq iforms tail?))))

;; Closure source info format
;; ((file . lineno) (proc args))
(define (pass1/lambda->iform name sexp library lvars)
  (define (dotpair->list p)
    (let loop ([p p])
      (cond
       [(and (not (pair? p)) (not (null? p)))
        (cons p '())]
       [(null? p)
        '()]
       [else
        (cons (car p) (loop (cdr p)))])))
  (let* ([vars          (second sexp)]
         [body          (cddr sexp)]
         [parsed-vars   (parse-lambda-vars vars)]
         [optional-arg? (first parsed-vars)]
         [vars          (second parsed-vars)]
         [this-lvars    ($map1 (lambda (sym) ($lvar sym #f 0 0)) vars)])
    ($lambda (cons (source-info sexp) (cons name (dotpair->list (second sexp))))
             name
             (if optional-arg? (- (length vars) 1) (length vars))
             (if optional-arg? 1 0)
             this-lvars
             ;; the inner lvar comes first.
             (pass1/body->iform body library (append2 this-lvars lvars) #t)
             '()
             '())))

;; Store <libname . $library> for compiler.
;; You can't use this for VM.
(define libraries (make-eq-hashtable))

(define-macro (make-identifier alias libname name)
  `(list ,alias ,libname ,name))

(define (copy-identifier i)
  (list (first i) (second i) (third i)))

;; Now, we ignore version.
(define (library-name form)
  (remove-tail (second form) pair?))

(define (pass1/and->iform sexp library lvars tail?)
  (define (rec s)
    (match s
      [() ($const #t)]
      [(s)
       (pass1/sexp->iform (pass1/expand s) library lvars tail?)]
      [(e . more)
       ($if (pass1/sexp->iform (pass1/expand e) library lvars tail?)
            (rec more)
            ($it))]
      [else
       (error "syntax-error: malformed and:" sexp)]))
  (rec (cdr sexp)))

(define (pass1/or->iform sexp library lvars tail?)
  (define (rec s)
    (match s
      [() ($const #f)]
      [(s)
       (pass1/sexp->iform (pass1/expand s) library lvars tail?)]
      [(e . more)
       ($if (pass1/sexp->iform (pass1/expand e) library lvars tail?)
            ($it)
            (rec more))]
      [else
       (error "syntax-error: malformed or:" sexp)]))
  (rec (cdr sexp)))

(define (pass1/library->iform sexp library lvars)
  (define (get-identifier symbol libname imports)
    (aif (find10 (lambda (import) (eq? symbol (first import))) imports)
         (copy-identifier it)
         (make-identifier symbol libname symbol)))
  (define (get-rename-identifier rename-set libname imports)
    (aif (find10 (lambda (import) (eq? (first rename-set) (first import))) imports)
         (let1 identifier (copy-identifier it)
           (set-car! identifier (second rename-set))
           identifier)
         (make-identifier (second rename-set) libname (first rename-set))))
  (define (extract-exports imports libname form)
    (let loop ([export (cdr form)]
               [ret    '()])
      (cond
       [(null? export) ret]
       [(and (pair? (car export)) (eq? (caar export) 'rename))
        (loop (cdr export)
              (append2 ret ($map1 (lambda (p) (get-rename-identifier p libname imports)) (cdar export))))]
       [else
        (loop (cdr export) (cons (get-identifier (car export) libname imports) ret))])))
  (let1 lib ($library (library-name sexp) '() '() '() '() '() #f)
    ;; We parse (library ...) with following order.
    ;; 1. parse (import ...), then we know all impoted symbols and their name.
    ;; 2. parse (export ...).
    ;;    When the library exports symbol which is imported from another library, we link these by using imported symbols information got above.
    ;; 3. set body.
    ($library.set-import! lib (pass1/import->iform (fourth sexp) lib))
    ($library.set-export-syms! lib (extract-exports ($library.import-syms lib) ($library.name lib) (third sexp)))
    ;; We compile body at runtime.
    ($library.set-body! lib (cddddr sexp))
    (hashtable-set! libraries ($library.name lib) lib)
    lib))

(define (pass1/import->iform sexp library)
  ;; ignore <version>.
  (define (library-name form)
    (libname->symbol (remove-tail form pair?)))
  (define (parse-level form)
    (cond
     [(symbol? form)
      (case form
        [(expand) 1]
        [(run)    0]
        [else (error "unknown for")])]
     [(and (pair? form) (= (length form) 2) (eq? (first form) 'meta))
      (second form)]
     [else
      (error "unknown level on meta")]))
  ;; todo cleanup this code. (uhaaaa)
  (define (import-iter form level)
    (case (first form)
      [(for)
       (import-iter (second form) (parse-level (third form)))]
      [(only)
       (let1 only-binds (cddr form)
         (acond
          [(hashtable-ref libraries (library-name (second form)) #f)
           ($library.add-import-syms! library ($filter-map1 (lambda (x)
                                                              (if (memq (car x) only-binds)
                                                                  (copy-identifier x)
                                                                  #f)) ;; not imported
                                                            ($library.export-syms it)))
           ($import-spec ($library.name it) level)]
          [#t
           (error "library " (library-name (second form)) " not found")]))]
      [(except)
       (let1 except-binds (cddr form)
         (acond
          [(hashtable-ref libraries (library-name (second form)) #f)
           ($library.add-import-syms! library ($filter-map1 (lambda (x)
                                                              (if (memq (car x) except-binds)
                                                                  #f ;; not imported
                                                                  (copy-identifier x)))
                                                            ($library.export-syms it)))
           ($import-spec ($library.name it) level)]
          [#t
           (error "library " (library-name (second form)) " not found")]))]
      [(rename)
       (let1 renames (cddr form)
         (acond
          [(hashtable-ref libraries (library-name (second form)) #f)
           ($library.add-import-syms! library ($filter-map1 (lambda (x)
                                                              (aif (find10 (lambda (rename) (eq? (first x) (first rename))) renames)
                                                                   (make-identifier (second it) (second x) (third x))
                                                                   (copy-identifier x)))
                                                            ($library.export-syms it)))
           ($import-spec ($library.name it) level)]
          [#t
           (error "library " (library-name (second form)) " not found")]))]
      [(prefix)
       (let1 prefix (symbol->string (third form))
         (acond
          [(hashtable-ref libraries (library-name (second form)) #f)
           ($library.add-import-syms! library ($filter-map1 (lambda (x) (make-identifier
                                                                         (string->symbol (string-append prefix (symbol->string (first x))))
                                                                         (second x)
                                                                         (third x)))
                                                            ($library.export-syms it)))
           ($import-spec ($library.name it) level)]
          [#t
           (error "library " (library-name (second form)) " not found")]))]
      [else
       (acond
        [(hashtable-ref libraries (library-name form) #f)
         ($library.add-import-syms! library ($map1 copy-identifier ($library.export-syms it)))
         ($import-spec ($library.name it) level)]
        [#t
         (error "library " (library-name form) " not found")])]))
  ;; default import level is zero.
  ($import ($map1 (lambda (i) (import-iter i 0)) (cdr sexp))))

;; N.B.
;; this procedure is called from freeproc.cpp
(define (pass1/macroexpand sexp)
  (let1 proc (first sexp)
    (acond
     [(and (symbol? proc) (assoc proc ($library.macro top-level-library)))
      (pass1/expand (vm/apply (cdr it) (cdr sexp)))]
     [#t sexp])))

(define (pass1/sexp->iform sexp library lvars tail?)
  (define (sexp->iform sexp)
    (pass1/sexp->iform (pass1/expand sexp) library lvars tail?))
  (define (operator-nargs->iform op tag)
    (let* ([args (cdr sexp)]
           [len (length args)])
      (cond [(= 0 len)
             (case op
               [(+)
                (sexp->iform 0)]
               [(*)
                (sexp->iform 1)]
               [else
                (error op " got too few argment")])]
            [(= 1 len)
             (case op
               [(-)
                (sexp->iform (* -1 (car args)))]
               [(/)
                (sexp->iform `(/ 1 ,(car args)))]
               [else
                (sexp->iform (car args))])]
            [(= 2 len)
             ($asm tag (list (sexp->iform (first args)) (sexp->iform (second args))))]
            [else
             (let1 args-iform ($map1 sexp->iform args)
               (fold (lambda (x y) ($asm tag (list y x))) (car args-iform) (cdr args-iform)))])))
  (define (call-1arg->iform tag)
    ($asm tag (list (pass1/sexp->iform (pass1/expand (second sexp)) library lvars tail?))))
  (define (call-1arg-optional->iform tag)
    ($asm tag (list (pass1/sexp->iform (if (null? (cdr sexp)) '() (pass1/expand (second sexp))) library lvars tail?))))
  (define (call-2args->iform tag)
    ($asm tag (list (pass1/sexp->iform (pass1/expand (second sexp)) library lvars tail?)
                    (pass1/sexp->iform (pass1/expand (third  sexp)) library lvars tail?))))
  (define (call-3args->iform tag)
    ($asm tag (list (pass1/sexp->iform (pass1/expand (second sexp)) library lvars tail?)
                    (pass1/sexp->iform (pass1/expand (third  sexp)) library lvars tail?)
                    (pass1/sexp->iform (pass1/expand (fourth sexp)) library lvars tail?))))
  (define (numcmp->iform operator args tag)
    (let1 len (length args)
      (cond [(> 2 len) (error operator " got too few argument")]
            [(= 2 len)
             ($asm tag
                   (list (sexp->iform (first args))
                         (sexp->iform (second args))))]
            [else
             (sexp->iform (conditions->if (apply-each-pair operator args)))])))
  (cond
   [(pair? sexp)
    (case (car sexp)
      ;;---------------------------- cons --------------------------------------
      [(cons)
       ($asm 'CONS ($map1 sexp->iform (cdr sexp)))]
      ;;---------------------------- and ---------------------------------------
      [(and)
       (pass1/and->iform sexp library lvars tail?)]
      ;;---------------------------- and ---------------------------------------
      [(or)
       (pass1/or->iform sexp library lvars tail?)]
      ;;---------------------------- begin -------------------------------------
      [(begin)
       (pass1/body->iform (pass1/expand (cdr sexp)) library lvars tail?)]
      ;;---------------------------- values ------------------------------------
      [(values)
       ($asm 'VALUES ($map1 sexp->iform (cdr sexp)))]
      ;;---------------------------- define ------------------------------------
      [(define)
       (match sexp
         [('define name ('lambda . more))
          (let1 closure  (make-list-with-src-slot (cons 'lambda more))
            (set-source-info! closure (source-info (third sexp)))
            ($define ($library.name library) name (pass1/lambda->iform name closure library lvars)))]
         [else
           ($define ($library.name library) (second sexp) (sexp->iform (third sexp)))])]
      ;;---------------------------- define-macro ------------------------------
      [(define-macro)
       (if (pair? (second sexp))
           ; we can't use hash-table here, because hash-table can't be written with (write).
           ; So we use acons instead.
           ($library.set-macro! library (acons (caadr sexp)  (compile-partial `(lambda ,(cdadr sexp) ,(third sexp)) library) ($library.macro library)))
           ($library.set-macro! library (acons (second sexp) (compile-partial (third sexp)) ($library.macro library))))
       ($undef)]
      ;;---------------------------- receive -----------------------------------
      [(receive)
       (match sexp
         [('receive vars vals . body)
          (receive (vars reqargs optarg) (parse-lambda-args vars)
            (let1 this-lvars ($map1 (lambda (sym) ($lvar sym #f 0 0)) vars)
              ($receive
               this-lvars
               reqargs
               optarg
               (sexp->iform vals)
               ;; the inner lvar comes first.
               (pass1/body->iform (pass1/expand body) library (append2 this-lvars lvars) tail?)
               tail?)))]
         [else
          (syntax-error "malformed receive")])]
      ;;---------------------------- let ---------------------------------------
      [(let)
       (let* ([vars       ($map1 car (second sexp))]
              [vals       ($map1 cadr (second sexp))]
              [body       (cddr sexp)]
              [inits      ($map1 sexp->iform vals)]
              [this-lvars (map (lambda (sym init) ($lvar sym init 0 0)) vars inits)])
         ($let 'let
               this-lvars
               inits
               ;; the inner lvar comes first.
               (pass1/body->iform (pass1/expand body) library (append2 this-lvars lvars) tail?)
               tail?
               (source-info sexp)
               ))]
      ;;---------------------------- letrec ------------------------------------
      [(letrec)
       (let* ([vars       ($map1 car (second sexp))]
              [vals       ($map1 cadr (second sexp))]
              [body       (cddr sexp)]
              [this-lvars ($map1 (lambda (sym) ($lvar sym ($undef) 0 0)) vars)]
              [inits      ($map1 (lambda (x) (pass1/sexp->iform x library (append2 this-lvars lvars) tail?)) vals)])
         (for-each (lambda (lvar init) ($lvar.set-init-val! lvar init)) this-lvars inits)
         ($let 'rec
               this-lvars
               inits
               ;; the inner lvar comes first.
               (pass1/body->iform (pass1/expand body) library (append2 this-lvars lvars) tail?)
               tail?
               (source-info sexp)
               ))]
      ;;---------------------------- lambda ------------------------------------
      [(lambda)
       (pass1/lambda->iform 'lambda sexp library lvars)]
      ;;---------------------------- library -----------------------------------
      [(library)
       (pass1/library->iform sexp library lvars)]
      ;;---------------------------- import ------------------------------------
      [(import)
       (pass1/import->iform sexp library)]
      ;;---------------------------- set! --------------------------------------
      [(set!)
       (pass1/assign->iform `(set! ,(second sexp), (pass1/expand (third sexp))) library lvars tail?)]
      ;;---------------------------- if ----------------------------------------
      [(if)
       (let ([test (second sexp)]
             [then (third sexp)])
         ($if
          (pass1/sexp->iform (pass1/expand test) library lvars #f)
          (pass1/sexp->iform (pass1/expand then) library lvars tail?)
          (if (null? (cdddr sexp))
              ($undef)
              (pass1/sexp->iform (pass1/expand (fourth sexp)) library lvars tail?))))]
      ;;---------------------------- call/cc -----------------------------------
      [(call/cc)
       ($call-cc (sexp->iform (second sexp)) tail?)]
      [(call-with-current-continuation)
       ($call-cc (sexp->iform (second sexp)) tail?)]
      ;;---------------------------- quote -------------------------------------
      [(quote)
       ($const (second sexp))]
      [(make-vector)
       (if (null? (cddr sexp))
           ($asm 'MAKE_VECTOR (list (pass1/sexp->iform (pass1/expand (second sexp)) library lvars tail?)
                                    (pass1/sexp->iform (pass1/expand '()) library lvars tail?)))
           (call-2args->iform 'MAKE_VECTOR))]
      [(+)                (operator-nargs->iform '+ 'NUMBER_ADD)]
      [(-)                (operator-nargs->iform '- 'NUMBER_SUB)]
      [(*)                (operator-nargs->iform '* 'NUMBER_MUL)]
      [(/)                (operator-nargs->iform '/ 'NUMBER_DIV)]
;      [(append)           (operator-nargs->iform 'append 'APPEND)]
      [(=)                (numcmp->iform '= (cdr sexp) 'NUMBER_EQUAL)]
      [(>=)               (numcmp->iform '>= (cdr sexp) 'NUMBER_GE)]
      [(>)                (numcmp->iform '> (cdr sexp) 'NUMBER_GT)]
      [(<)                (numcmp->iform '< (cdr sexp) 'NUMBER_LT)]
      [(<=)               (numcmp->iform '<= (cdr sexp) 'NUMBER_LE)]
      [(vector?)          (call-1arg->iform 'VECTOR_P)]
      [(vector-length)    (call-1arg->iform 'VECTOR_LENGTH)]
      [(vector-set!)      (call-3args->iform 'VECTOR_SET)]
      [(vector-ref)       (call-2args->iform 'VECTOR_REF)]
      [(car)              (call-1arg->iform 'CAR)]
      [(cdr)              (call-1arg->iform 'CDR)]
      [(caar)             (call-1arg->iform 'CAAR)]
      [(cadr)             (call-1arg->iform 'CADR)]
      [(cdar)             (call-1arg->iform 'CDAR)]
      [(cddr)             (call-1arg->iform 'CDDR)]
      [(set-car!)         (call-2args->iform 'SET_CAR)]
      [(set-cdr!)         (call-2args->iform 'SET_CDR)]
      [(eq?)              (call-2args->iform 'EQ)]
      [(eqv?)              (call-2args->iform 'EQV)]
      [(equal?)           (call-2args->iform 'EQUAL)]
      [(not)              (call-1arg->iform 'NOT)]
      [(null?)            (call-1arg->iform 'NULL_P)]
      [(pair?)            (call-1arg->iform 'PAIR_P)]
      [(symbol?)          (call-1arg->iform 'SYMBOL_P)]
      [(read)             (call-1arg-optional->iform 'READ)]
      [(read-char)        (call-1arg-optional->iform 'READ_CHAR)]
      ;;---------------------------- call or macro------------------------------
      [else
       (let1 proc (first sexp)
         (acond
          [(and (symbol? proc) (assoc proc ($library.macro library)))
           (sexp->iform (vm/apply (cdr it) (cdr sexp)))]
          [(and (symbol? proc) (find10 (lambda (sym) (eq? (first sym) proc)) ($library.import-syms library)))
           (let* ([lib (hashtable-ref libraries (second it) #f)]
                  [mac (assoc (third it) ($library.macro lib))])
             (if mac
                 (sexp->iform (pass1/expand (vm/apply (cdr mac) (cdr sexp))))
                 ($call (sexp->iform proc)
                        ($map1 sexp->iform (cdr sexp))
                        tail?
                        #f ;; type will be set on pass2
                        )))]
          ;; call
          [#t
           ($call (sexp->iform proc)
                  ($map1 sexp->iform (cdr sexp))
                  tail?
                  #f ;; type will be set on pass2
                  )]))])]
   [(symbol? sexp)
    (pass1/refer->iform sexp library lvars)]
   [else ($const sexp)]))

;;--------------------------------------------------------------------
;;
;;  Pass1: Pretty print for Iform
;;
;;    based on Gauche/src/compiler.scm by Shiro Kawai start.
;;
(define (pretty-iform iform)
  (define labels '()) ;; alist of label node and count

  (define (indent count)
    (dotimes (i count) (write-char #\space)))
  (define (nl ind)
    (newline) (indent ind))
  (define (lvar->string lvar)
    (format "~a[~a ~a]"
            ($lvar.sym lvar)
            ($lvar.ref-count lvar)
            ($lvar.set-count lvar)))
  (define (rec ind iform)
    (cond
     [(tag? iform $CONST)
      (format #t "($CONST ~s)" ($const.val iform))]
     [(tag? iform $UNDEF)
      (display "($UNDEF)")]
     [(tag? iform $LAMBDA)
      (format #t "($LAMBDA[~a ~a ~a]" ($lambda.name iform)
              (map lvar->string ($lambda.lvars iform)) ($lambda.flag iform))
      (nl (+ ind 2))
      (rec (+ ind 2) ($lambda.body iform)) (display ")")]
     [(tag? iform $SEQ)
      (format #t "($SEQ")
      (for-each (lambda (node) (nl (+ ind 2)) (rec (+ ind 2) node))
                ($seq.body iform))
      (display ")")]
     [(tag? iform $LIBRARY)
      (format #t "($LIBRARY ~a export [~a] import [~a]"
              ($library.name iform)
              ($library.export-syms iform)
              ($library.import-syms iform))
      (nl (+ ind 2))
                                        ;      (rec ind ($library.body iform))
      (display ")")]
     [(tag? iform $LOCAL-REF)
      (format #t "($LOCAL-REF ~a)" (lvar->string ($local-ref.lvar iform)))]
     [(tag? iform $GLOBAL-REF)
      (format #t "($GLOBAL-REF ~a ~a)" ($global-ref.libname iform) ($global-ref.sym iform))]
     [(tag? iform $LOCAL-ASSIGN)
      (format #t "($LOCAL-ASSIGN ~a"  (lvar->string ($local-assign.lvar iform)))
      (nl (+ ind 2))
      (rec (+ ind 2) ($local-assign.val iform)) (display ")")]
     [(tag? iform $GLOBAL-ASSIGN)
      (format #t "($GLOBAL-ASSIGN ~a ~a)" ($global-assign.sym iform) ($global-assign.val iform))]
     [(tag? iform $LET)
      (let* ((hdr  (format "($LET ("))
             (xind (+ ind (string-length hdr))))
        (display hdr)
        (for-each (lambda (var init)
                    (let1 z (format "(~a " (lvar->string var))
                      (display z)
                      (rec (+ xind  (string-length z)) init)
                      (display ")")
                      (nl xind)))
                  ($let.lvars iform) ($let.inits iform))
        (display ")") (nl (+ ind 2))
        (rec (+ ind 2) ($let.body iform)) (display ")"))]
     [(tag? iform $IF)
      (display "($IF ")
      (rec (+ ind 5) ($if.test iform)) (nl (+ ind 2))
      (rec (+ ind 2) ($if.then iform)) (nl (+ ind 2))
      (rec (+ ind 2) ($if.else iform)) (display ")")]
     [(tag? iform $LABEL)
      (cond ((assq iform labels)
             => (lambda (p) (format #t "label#~a" (cdr p))))
            (else
             (let1 num (length labels)
               (push! labels (cons iform num))
               (format #t "($label #~a" num)
               (nl (+ ind 2))
               (rec (+ ind 2) ($label.body iform)) (display ")"))))]
     [(tag? iform $ASM)
      (let1 insn ($asm.insn iform)
        (format #t "($asm ~a" insn))
      (for-each (lambda (node) (nl (+ ind 2)) (rec (+ ind 2) node))
                ($asm.args iform))
      (display ")")]
     [(tag? iform $DEFINE)
      (format #t "($DEFINE ~a:~a" ($define.libname iform) ($define.sym iform))
      (nl (+ ind 2))
      (rec (+ ind 2) ($define.val iform)) (display ")")]
     [(tag? iform $CALL-CC)
      (display "($CALL-CC ")
      (rec 0 ($call-cc.proc iform))
      (display ")")]
     [(tag? iform $LABEL)
      (display "($LABEL ")
      (rec 0 ($label.body iform))
      (display ")")]
     [(tag? iform $CALL)
      (let1 pre
          (cond (($call.tail? iform) => (lambda (x) "($call[tail] "))
                (else "($call "))
        (format #t pre)
        (format #t "[~a]" ($call.type iform))
        (rec (+ ind (string-length pre)) ($call.proc iform))
        (for-each (lambda (node) (nl (+ ind 2)) (rec (+ ind 2) node))
                  ($call.args iform))
        (display ")"))]
     (else
      (error "pretty-iform: unknown tag:" (tag iform)))
     ))
  (rec 0 iform)
  (newline))

;;  based on Gauche/src/compiler.scm by Shiro Kawai end.


;;--------------------------------------------------------------------
;;
;;  Pass2
;;
;;
;; =============================================================================
;;
;;     Known Bug
;;     jump with embedded call, miss over the let boundary.
;;
;;      (define *plugins* '())
;;      (define (register-plugin plugin)
;;        (set! *plugins* plugin))
;;      (register-plugin
;;                    (lambda ()
;;                      (let loop ([parent "parent"]
;;                                 [paths '(1)])
;;                        (if (null? paths)
;;                            '()
;;                            (let1 page (car paths)
;;                              (loop page (cdr paths)))))))
;;
;;      (*plugins*)
;;

(define SMALL_LAMBDA_SIZE 12)

(define pass2/dispatch-table (make-vector $INSN-NUM))

(define (pass2/$let iform closures)
  ($let.set-body! iform (pass2/optimize ($let.body iform) closures))
  ($let.set-inits! iform ($map1 (lambda (i) (pass2/optimize i closures)) ($let.inits iform)))
  (let1 o (pass2/eliminate-let iform)
    (if (eq? o iform)
        o
        (pass2/optimize o closures))))

(define (pass2/$receive iform closures)
  ($receive.set-body! iform (pass2/optimize ($receive.body iform) closures))
  ($receive.set-vals! iform (pass2/optimize ($receive.vals iform) closures))
  iform)

(define (pass2/$local-ref iform closures)
  (pass2/optimize-local-ref iform)
  iform)

(define (pass2/$seq iform closures)
  ($seq.set-body! iform ($map1 (lambda (x) (pass2/optimize x closures)) ($seq.body iform)))
  iform)

(define (pass2/const-inliner iform)
  (let ([insn ($asm.insn iform)]
        [args ($asm.args iform)])
    (case insn
      [(NUMBER_ADD)
       (when (and (tag? (first args) $CONST) (tag? (second args) $CONST))
         (vector-set! iform 0 $CONST)
         ($const.set-val! iform (+ ($const.val (first args)) ($const.val (second args)))))]
      [(NUMBER_MUL)
       (when (and (tag? (first args) $CONST) (tag? (second args) $CONST))
         (vector-set! iform 0 $CONST)
         ($const.set-val! iform (* ($const.val (first args)) ($const.val (second args)))))]
      [(NUMBER_MINUS)
       (when (and (tag? (first args) $CONST) (tag? (second args) $CONST))
         (vector-set! iform 0 $CONST)
         ($const.set-val! iform (- ($const.val (first args)) ($const.val (second args)))))]
      [else #f])))

(define (pass2/$asm iform closures)
  ($asm.set-args! iform ($map1 (lambda (x) (pass2/optimize x closures)) ($asm.args iform)))
  (pass2/const-inliner iform)
  iform)

(define (pass2/$lambda iform closures)
  ($lambda.set-body! iform
                     (pass2/optimize ($lambda.body iform) (cons iform closures)))
  iform)

(define (pass2/$if iform closures)
  (let ([test-c (pass2/optimize ($if.test iform) closures)]
        [then-c (pass2/optimize ($if.then iform) closures)]
        [else-c (pass2/optimize ($if.else iform) closures)])
  ($if test-c then-c else-c)))

(define (pass2/$call iform closures)
  (pass2/collect-call iform closures))

(define (pass2/empty iform closures)
  iform)

(define (pass2/register insn proc)
  (vector-set! pass2/dispatch-table insn proc))

(pass2/register $CONST         pass2/empty)
(pass2/register $LAMBDA        pass2/$lambda)
(pass2/register $LOCAL-REF     pass2/$local-ref)
(pass2/register $LOCAL-ASSIGN  pass2/empty)
(pass2/register $GLOBAL-ASSIGN pass2/empty)
(pass2/register $GLOBAL-REF    pass2/empty)
(pass2/register $SEQ           pass2/$seq)
(pass2/register $UNDEF         pass2/empty)
(pass2/register $IF            pass2/$if)
(pass2/register $ASM           pass2/$asm)
(pass2/register $DEFINE        pass2/empty)
(pass2/register $CALL          pass2/$call)
(pass2/register $CALL-CC       pass2/empty)
(pass2/register $LET           pass2/$let)
(pass2/register $LIST          pass2/empty)
(pass2/register $LIBRARY       pass2/empty)
(pass2/register $IMPORT        pass2/empty)
(pass2/register $IT            pass2/empty)
(pass2/register $RECEIVE       pass2/$receive)

(define (pass2/optimize iform closures)
  ((vector-ref pass2/dispatch-table (vector-ref iform 0)) iform closures))

(define (pass2/optimize-local-ref iform)
  (let* ([lvar     ($local-ref.lvar iform)]
         [init-val ($lvar.init-val lvar)]) ;; init-val = #f if lvar belongs to $LAMBDA.
    ;; if lvar is never set! and initial value is constant.
    (cond [(and init-val (zero? ($lvar.set-count lvar)) (tag? init-val $CONST))
           ;; We re-use the vector.
           (set-tag! iform $CONST)
           ($lvar.ref-count--! lvar)
           ($const.set-val! iform ($const.val init-val))]
          [(and init-val (tag? init-val $LOCAL-REF)
                (zero? ($lvar.set-count ($local-ref.lvar init-val))))
           ($lvar.ref-count--! lvar)
           ($lvar.ref-count++! ($local-ref.lvar init-val))
           ($local-ref.copy iform init-val)
           (pass2/optimize-local-ref iform)]
          [else iform])))

(define (pass2/eliminate-let iform)
  (let ([vars ($let.lvars  iform)]
        [inits ($let.inits iform)]
        [body ($let.body iform)])
    (for-each pass2/optimize-closure vars inits)
    (let* ([v (pass2/remove-vars vars inits)]
           [new-vars      (vector-ref v 0)]
           [new-inits     (vector-ref v 1)]
           [removed-inits (vector-ref v 2)])

      (cond ((null? new-vars)
             (if (null? removed-inits)
                 body
                 ($seq (append2 removed-inits (list body)) ($let.tail? iform))))
            (else
             ($let.set-lvars! iform new-vars)
             ($let.set-inits! iform new-inits)
             ($let.set-body! iform body)
             (unless (null? removed-inits)
               (if (tag? body $SEQ)
                   ($seq.set-body! body
                                   (append2 removed-inits
                                            ($seq.body body)))
                   ($let.set-body! iform
                                   ($seq (append2 removed-inits
                                                 (list body))
                                         ($let.tail? iform)))))
             iform)))))

(define (iform-copy-zip-lvs orig-lvars lv-alist)
  (let1 new-lvars ($map1 (lambda (lv) (make-lvar ($lvar.sym lv))) orig-lvars)
    (cons new-lvars
          (foldr2 acons lv-alist orig-lvars new-lvars)))) ;; todo foldr2

(define (iform-copy-lvar lvar lv-alist)
  ;; NB: using extra lambda after => is a kludge for the current optimizer
  ;; to work better.  Should be gone later.
  (cond ((assq lvar lv-alist) => (lambda (p) (cdr p)))
        (else lvar)))

(define (iform-copy iform lv-alist)
  (let1 t (tag iform)
    (cond
     [(= $DEFINE t)
      ($define  ($define.libname iform)
                ($define.sym iform)
                (iform-copy ($define.val iform) lv-alist))]
     [(= $LOCAL-REF t)
      ($local-ref (iform-copy-lvar ($local-ref.lvar iform) lv-alist))]
     [(= $LOCAL-ASSIGN t)
      ($local-assign (iform-copy-lvar ($local-assign.lvar iform) lv-alist)
                     (iform-copy ($local-assign.val iform) lv-alist))]
     [(= $GLOBAL-REF t)
      ($global-ref ($global-ref.libname iform) ($global-ref.sym iform))]
     [(= $GLOBAL-ASSIGN t)
      ($global-assign ($global-assign.libname iform) ($global-assign.sym iform) (iform-copy ($global-assign.val iform) lv-alist))]
     [(= $CONST t)
      ($const ($const.val iform))]
     [(= $IF t)
      ($if (iform-copy ($if.test iform) lv-alist)
           (iform-copy ($if.then iform) lv-alist)
           (iform-copy ($if.else iform) lv-alist))]
     [(= $LET t)
      (let* ([ret (iform-copy-zip-lvs ($let.lvars iform) lv-alist)]
             [newlvs (car ret)]
             [newalist (cdr ret)])
        ($let ($let.type iform)
              newlvs
              (let1 al (case ($let.type iform)
                         ((let) lv-alist)
                         ((rec) newalist))
                ($map1 (lambda (x) (iform-copy x al)) ($let.inits iform)))
              (iform-copy ($let.body iform) newalist)
              ($let.tail? iform)
              ($let.src iform)
              ))]
     [(= $LAMBDA t)
      (let* ([ret (iform-copy-zip-lvs ($lambda.lvars iform) lv-alist)]
             [newlvs (car ret)]
             [newalist (cdr ret)])
        ($lambda ($lambda.src iform)
                 ($lambda.name iform)
                 ($lambda.reqargs iform)
                 ($lambda.optarg iform)
                 newlvs
                 (iform-copy ($lambda.body iform) newalist)
                 ($lambda.flag iform)
                 ($lambda.calls iform)))]
     [(= $SEQ t)
      ($seq ($map1 (lambda (x) (iform-copy x lv-alist)) ($seq.body iform)) ($seq.tail? iform))]
     [(= $CALL t)
      ($call (iform-copy ($call.proc iform) lv-alist)
             ($map1 (lambda (x) (iform-copy x lv-alist)) ($call.args iform))
             #f
             ($call.type iform))]
     [(= $ASM t)
      ($asm ($asm.insn iform)
            ($map1 (lambda (x) (iform-copy x lv-alist)) ($asm.args iform)))]
     [else iform])))

;; based on Gauche/src/compiler.scm by Shiro Kawai start.
(define (pass2/optimize-closure lvar lambda-node)
  (when (and (zero? ($lvar.set-count lvar))
             (> ($lvar.ref-count lvar) 0)
             (tag? lambda-node $LAMBDA))
    (or (and (= ($lvar.ref-count lvar) (length ($lambda.calls lambda-node)))
             (let* ([ret (pass2/classify-calls ($lambda.calls lambda-node) lambda-node)]
                    [locals (first ret)]
                    [recs (second ret)]
                    [tail-recs (third ret)])
               (and (null? recs)
                    (pair? locals)
                    (or (and (null? (cdr locals))
                             (pass2/local-call-embedder lvar lambda-node
                                                        (car locals)
                                                        tail-recs))
                        (and (null? tail-recs)
                             (< (iform-count-size-upto lambda-node
                                                       SMALL_LAMBDA_SIZE)
                                SMALL_LAMBDA_SIZE)
                             (pass2/local-call-inliner lvar lambda-node
                                                       locals))))))
        (pass2/local-call-optimizer lvar lambda-node))
    ))

(define-macro (sum-items cnt . items)
  (if (null? items)
      cnt
      (let1 target-list? (and (pair? (car items)) (eq? (caar items) '*))
        `(let1 s1 (,(if target-list? 'rec-list 'rec) ,(if target-list? (cadar items) (car items)) ,cnt)
           (if (>= s1 limit) limit
               (sum-items s1 ,@(cdr items)))))))

;; Counts the size (approx # of nodes) of the iform.
(define (iform-count-size-upto iform limit)
  (define (rec iform cnt)
    (let1 t (tag iform)
      (cond
       [(= $DEFINE t)        (sum-items (+ cnt 1) ($define.val iform))]
       [(= $LOCAL-REF t)     (+ cnt 1)]
       [(= $GLOBAL-REF t)    (+ cnt 1)]
       [(= $CONST t)         (+ cnt 1)]
       [(= $LOCAL-ASSIGN t)  (sum-items (+ cnt 1) ($local-assign.val iform))]
       [(= $GLOBAL-ASSIGN t) (sum-items (+ cnt 1) ($global-assign.val iform))]
       [(= $IF t)            (sum-items (+ cnt 1) ($if.test iform)
                                        ($if.then iform) ($if.else iform))]
       [(= $LET t)           (sum-items (+ cnt 1) (* ($let.inits iform)) ($let.body iform))]
       [(= $LAMBDA t)        (sum-items (+ cnt 1) ($lambda.body iform))]
       [(= $LABEL t)         (sum-items cnt ($label.body iform))]
       [(= $SEQ t)           (sum-items cnt (* ($seq.body iform)))]
       [(= $CALL t)          (sum-items (+ cnt 1) ($call.proc iform) (* ($call.args iform)))]
       [(= $ASM t)           (sum-items (+ cnt 1) (* ($asm.args iform)))]
       [else
        (error "[internal error] iform-count-size-upto: unknown iform tag:"
               (tag iform))]
       )))
  (define (rec-list iform-list cnt)
    (cond ((null? iform-list) cnt)
          ((>= cnt limit) limit)
          (else
           (rec-list (cdr iform-list)
                     (rec (car iform-list) cnt)))))
  (rec iform 0))

;; Adjust argument list according to reqargs and optarg count.
;; Used in procedure inlining and local call optimization.
(define (adjust-arglist reqargs optarg iargs name)
  (unless (argcount-ok? iargs reqargs (> optarg 0))
    (errorf "wrong number of arguments: ~a requires ~a, but got ~a at ~a"
            name reqargs (length iargs) (source-info iargs)))
  (if (zero? optarg)
      iargs
      (receive (reqs opts) (split-at iargs reqargs)
        (append2 reqs (list ($list opts))))))

;; Does the given argument list satisfy procedure's reqargs/optarg?
(define (argcount-ok? args reqargs optarg?)
  (let1 nargs (length args)
    (or (and (not optarg?) (= nargs reqargs))
        (and optarg? (>= nargs reqargs)))))


;; Called when the local function (lambda-node) doesn't have recursive
;; calls, can be inlined, and called from multiple places.
;; NB: This inlining would introduce quite a few redundant $LETs and
;; we want to run LREF beta-conversion again.  It means one more path.
;; Maybe we'd do that in the future version.
;; NB: Here we destructively modify $call node to change it to $seq,
;; in order to hold the $LET node.  It breaks the invariance that $seq
;; contains zero or two or more nodes---this may prevent Pass 3 from
;; doing some optimization.
(define (pass2/local-call-inliner lvar lambda-node calls)
  (define (inline-it call-node lambda-node)
    (let1 inlined (pass2/expand-inlined-procedure lambda-node
                                                  ($call.args call-node))
      (vector-set! call-node 0 $SEQ)
      (if (tag? inlined $SEQ)
          ($seq.set-body! call-node ($seq.body inlined))
          ($seq.set-body! call-node (list inlined)))))
;  (log "*** optimized *** local-call-inliner")
  ($lvar.set-ref-count! lvar 0)
  ($lambda.set-flag! lambda-node 'dissolved)
  (let loop ((calls calls))
    (cond ((null? (cdr calls))
           (inline-it (car calls) lambda-node))
          (else
           (inline-it (car calls) (iform-copy lambda-node '()))
           (loop (cdr calls))))))



;; Called when the local function (lambda-node) isn't needed to be a closure
;; and can be embedded.
;; NB: this operation introduces a shared/circular structure in the IForm.
;; hoge
(define (pass2/local-call-embedder lvar lambda-node call rec-calls)
;  (log "*** optimized *** local-call-embedder")
  (let ((reqargs ($lambda.reqargs lambda-node))
        (optarg  ($lambda.optarg lambda-node))
        (name    ($lambda.name lambda-node))
        )
    ($call.set-args! call (adjust-arglist reqargs optarg ($call.args call)
                                          name))
    ($lvar.ref-count--! lvar)
    ($call.set-type! call 'embed)
    ($call.set-proc! call lambda-node)
    ($lambda.set-flag! lambda-node 'dissolved)
    (unless (null? rec-calls)
      (let1 body
          ($label #f ($lambda.body lambda-node))
        ($lambda.set-body! lambda-node body)
        (dolist (jcall rec-calls)
          ($lvar.ref-count--! lvar)
          ($call.set-args! jcall (adjust-arglist reqargs optarg
                                                 ($call.args jcall)
                                                 name))
          ($call.set-proc! jcall call)
          ($call.set-type! jcall 'jump))))))

;; Set up local calls to LAMBDA-NODE.  Marking $call node as 'local
;; lets pass3 to generate LOCAL-ENV-CALL instruction.
(define (pass2/local-call-optimizer lvar lambda-node)
;  (log "*** optimized *** local-call-optimizerr")
  (let ((reqargs ($lambda.reqargs lambda-node))
        (optarg  ($lambda.optarg lambda-node))
        (name    ($lambda.name lambda-node))
        (calls   ($lambda.calls lambda-node)))
    (dolist (call calls)
      ($call.set-args! (car call)
                       (adjust-arglist reqargs optarg
                                       ($call.args (car call))
                                       name))
      ($call.set-type! (car call) 'local))
    ;; We clear the calls list, just in case if the lambda-node is
    ;; traversed more than once.
    ($lambda.set-calls! lambda-node '())))


;; Classify the calls into categories.  TAIL-REC call is classified as
;; REC if the call is across the closure boundary.
(define (pass2/classify-calls call&envs lambda-node)
  (define (direct-call? env)
    (let loop ((env env))
      (cond ((null? env) #t)
            ((eq? (car env) lambda-node) #t)
            ((eq? ($lambda.flag (car env)) 'dissolved)
             (loop (cdr env))) ;; skip dissolved (inlined) lambdas
            (else #f))))
  (let loop ((call&envs call&envs)
             (local '())
             (rec '())
             (trec '()))
    (match call&envs
      (()
       (list local rec trec))
      (((call . env) . more)
       (case ($call.type call)
         ((tail-rec)
          (if (direct-call? env)
              (loop more local rec (cons call trec))
              (loop more local (cons call rec) trec)))
         ((rec) (loop more local (cons call rec) trec))
         (else  (loop more (cons call local) rec trec)))))
    ))



(define (pass2/remove-vars vars init-iforms)
  (let loop ([vars vars]
             [init-iforms init-iforms]
             [rl '()]
             [ri '()]
             [rr '()])
    (cond [(null? vars)
           `#(,(reverse rl) ,(reverse ri) ,(reverse rr))]
          [(and (= 0 ($lvar.ref-count (car vars)))
                (zero? ($lvar.set-count (car vars))))
           (cond [(tag? (car init-iforms) $LOCAL-REF) ;; if removed inits is $LOCAL-REF, decrement ref-count.
                  ($lvar.ref-count--! ($local-ref.lvar (car init-iforms)))])
           (loop (cdr vars) (cdr init-iforms) rl ri
                 (if (memq (tag (car init-iforms))
                           `(,$CONST ,$LOCAL-REF ,$LAMBDA))
                     rr
                     (cons (car init-iforms) rr)))]
          (else
           (loop (cdr vars) (cdr init-iforms)
                 (cons (car vars) rl) (cons (car init-iforms) ri) rr)))))

(define (pass2/self-recursing? closure closures)
  (find10 (lambda (c) (eq? closure c)) closures))

(define (pass2/classify-local-ref-call iform closures tail?)
  (let1 lvar ($local-ref.lvar iform)
    (if (> ($lvar.set-count lvar) 0)
        'local)
    (let1 init-val ($lvar.init-val lvar)
      (cond [(and init-val (tag? init-val $LAMBDA))
             (cond [(pass2/self-recursing? init-val closures)
                    (if tail? 'tail-rec 'rec)]
                   [(= ($lvar.ref-count lvar) 1)
                    ($lvar.ref-count--! lvar)
                    ($lvar.set-init-val! lvar '())
                    init-val]
                   [else
                    'local])]
            [else
             #f]))))

(define (pass2/expand-inlined-procedure iform iargs)
  (let ((lvars ($lambda.lvars iform))
        (args  (pass2/adjust-arglist ($lambda.reqargs iform) ($lambda.optarg iform)
                                     iargs ($lambda.name iform))))
    (for-each (lambda (lv a) ($lvar.set-init-val! lv a)) lvars args)
    ($let 'let lvars args ($lambda.body iform) #f #f)))

(define (pass2/argcount-ok? args reqargs optarg?)
  (let1 nargs (length args)
    (or (and (not optarg?) (= nargs reqargs))
        (and optarg? (>= nargs reqargs)))))

(define (pass2/adjust-arglist reqargs optarg iargs name)
  (unless (pass2/argcount-ok? iargs reqargs (> optarg 0))
    (errorf "wrong number of arguments: ~a requires ~a, but got ~a at ~a"
            name reqargs (length iargs) (source-info iargs)))
  (if (zero? optarg)
      iargs
      (let* ([ret-args (pass2/split-args iargs reqargs)]
             [reqs     (car ret-args)]
             [opts     (cdr ret-args)])
        (append2 reqs (list ($list opts))))))

(define (pass2/split-args args reqargs)
  (let loop ((i reqargs) (rest args) (r '()))
    (cond ((= i 0) (cons (reverse r) rest))
          ((null? rest) (error "given list is too short:" args))
          (else (loop (- i 1) (cdr rest) (cons (car rest) r))))))

(define (pass2/collect-call iform closures)
  (cond
   [($call.type iform) iform]
   [else
    (let ([proc ($call.proc iform)]
          [args ($call.args iform)])
      (cond [(tag? proc $LAMBDA)
             (pass2/optimize (pass2/expand-inlined-procedure proc args)
                             closures)]
            [(and (tag? proc $LOCAL-REF)
                  (pass2/classify-local-ref-call proc closures ($call.tail? iform)))
             => (lambda (type)
                  (cond
                   ;; Directly inline
                   [(vector? type)
                    ($call.set-proc! iform type)
                    ;; Directly inlinable case.  NB: this only happens if the $LREF
                    ;; node is the lvar's single reference, so we know the inlined
                    ;; procedure is never called recursively.  Thus we can safely
                    ;; traverse the inlined body without going into infinite loop.
                    ;;                ($call-proc-set! iform result)
                    (let1 o (pass2/expand-inlined-procedure type args)
                      (pass2/optimize o closures)
                      o)
                    ;;                           penv tail?))
                    ]
                   [(not type)
                    iform]
                   [else
                    (let1 lambda-iform ($lvar.init-val ($local-ref.lvar proc))
                      ($call.set-type! iform type)
                      ($lambda.set-calls! lambda-iform
                                          (cons (cons iform closures)
                                                ($lambda.calls lambda-iform)))
                      ;; todo
                      ;; args の最適化 see Gauche
                      ($call.set-args! iform ($map1 (lambda (x) (pass2/optimize x closures)) args))
                      iform)]))]
            [else
             ($call.set-args! iform ($map1 (lambda (x) (pass2/optimize x closures)) args))
             iform]))]))

;;--------------------------------------------------------------------
;;
;;  Pass3
;;
;;

;;
;; Find free variables in IForm.
;;   free variables is neither global variable nor local variable.
;;
;;   Arguments
;;     iform:     IForm
;;     locals:    local variables as $lvar structure.
;;     can-frees: candidates of free variables as $lvar structure.
;;
;; moved to freeproc.cpp
;; N.B. these procedures are still required by vm.scm
(define (pass3/find-free iform locals can-frees)
  (define (rec i l labels-seen)
    (let1 t (tag i)
      (cond
       [(= $CONST t) '()]
       [(= $LET t)
        (append2 ($append-map1 (lambda (fm) (rec fm l labels-seen)) ($let.inits i))
; removed unnecesary append
;                 (rec ($let.body i) (append2 l ($let.lvars i)) labels-seen))]
                 (rec ($let.body i) ($let.lvars i) labels-seen))]
       [(= $RECEIVE t)
        (append2 (rec ($receive.vals i) l labels-seen)
; removed unnecesary append
;                (rec ($receive.body i) (append2 l ($receive.lvars i)) labels-seen))]
                (rec ($receive.body i) ($receive.lvars i) labels-seen))]
       [(= $SEQ t)
        ($append-map1 (lambda (fm) (rec fm l labels-seen)) ($seq.body i))]
       [(= $LAMBDA t)
; removed unnecesary append
;        (rec ($lambda.body i) (append2 l ($lambda.lvars i)) labels-seen)]
        (rec ($lambda.body i) ($lambda.lvars i) labels-seen)]
       [(= $LOCAL-ASSIGN t)
        (let1 lvar ($local-assign.lvar i)
          (if (memq lvar can-frees)
              (cons lvar (rec ($local-assign.val i) l labels-seen))
              (rec ($local-assign.val i) l labels-seen)))]
       [(= $LOCAL-REF t)
        (let1 lvar ($local-ref.lvar i)
          (cond [(memq lvar l) '()]
                [(memq lvar can-frees) (list lvar)]
                [else '()]))]
       [(= $GLOBAL-REF t)
        (let* ([sym ($global-ref.sym i)]
               [found (find10 (lambda (x) (eq? ($lvar.sym x) sym)) can-frees)])
          (if found (list found) '()))]
       [(= $UNDEF t)      '()]
       [(= $IF t)
        (append2 (rec ($if.test i) l labels-seen)
                 (append2 (rec ($if.then i) l labels-seen)
                         (rec ($if.else i) l labels-seen)))]
       [(= $ASM t)
        ($append-map1 (lambda (fm) (rec fm l labels-seen)) ($asm.args i))]
       [(= $DEFINE t)
        (rec ($define.val i) l labels-seen)]
       [(= $CALL t)
        ;; N.B.
        ;; (proc args)
        ;;   args are evaluate before proc, so you should find free variables of args at first.
       (append2
         ($append-map1 (lambda (fm) (rec fm l labels-seen)) ($call.args i))
         (rec ($call.proc i) l labels-seen)
                )]
       [(= $CALL-CC t)
        (rec ($call-cc.proc i) l labels-seen)]
       [(= $GLOBAL-ASSIGN t)
        (rec ($global-assign.val i) l labels-seen)]
       [(= $LIST t)
        ($append-map1 (lambda (fm) (rec fm l labels-seen)) ($list.args i))]
       [(= $LABEL t)
        (if (memq i labels-seen)
            '()
            (rec ($label.body i) l (cons i labels-seen)))]
       [(= $IMPORT t)
        '() ;; todo 本当?
        ]
       [(= $LIBRARY t)
        '() ;; todo 本当?
        ]
       [(= $IT t) '()]
       [else
        (error "pass3/find-free unknown iform:" (tag i))])))
  (uniq (rec iform locals '())))

;; moved to freeproc.cpp
;; N.B. these procedures are still required by vm.scm
(define (pass3/find-sets iform lvars)
  (define (rec i)
    (let1 t (tag i)
      (cond
       [(= $CONST t) '()]
       [(= $LET t)
        (append ($append-map1 rec ($let.inits i))
                (rec ($let.body i)))]
       [(= $RECEIVE t)
        (append (rec ($receive.vals i))
                (rec ($receive.body i)))]
       [(= $SEQ t)
        ($append-map1 rec ($seq.body i))]
       [(= $LAMBDA t)
        (rec ($lambda.body i))]
       [(= $LOCAL-ASSIGN t)
        (let1 lvar ($local-assign.lvar i)
          (append (if (memq lvar lvars) (list lvar) '())
                  (rec ($local-assign.val i))))]
       [(= $LOCAL-REF t)  '()]
       [(= $GLOBAL-REF t) '()]
       [(= $UNDEF t)      '()]
       [(= $IF t)
        (append (rec ($if.test i))
                (rec ($if.then i))
                (rec ($if.else i)))]
       [(= $ASM t)
        ($append-map1 rec ($asm.args i))]
       [(= $DEFINE t)
        (rec ($define.val i))]
       [(= $CALL t)
        (append
         ($append-map1 rec ($call.args i))
         (rec ($call.proc i))
                )]
       [(= $CALL-CC t)
        (rec ($call-cc.proc i))]
       [(= $GLOBAL-ASSIGN t)
        (rec ($global-assign.val i))]
       [(= $LIST t)
        ($append-map1 rec ($list.args i))]
       [(= $LABEL t)
        '() ;; todo 本当
        ]
       [(= $IMPORT t)
        '() ;; todo 本当?
        ]
       [(= $LIBRARY t)
        '() ;; todo 本当?
        ]
       [(= $IT t) '()]
       [else
        (error "pass3/find-sets unknown iform:" i)])))
  (uniq (rec iform)))

;; moved to freeproc.cpp start
;; N.B. these procedures are still required by vm.scm
(define (make-code-builder)
  (list 'builder))

(define (code-builder-put1! cb x)
  (append! cb (list x)))

(define (code-builder-put2! cb a b)
  (append! cb (list a b)))

(define (code-builder-put3! cb a b c)
  (append! cb (list a b c)))

(define (code-builder-put4! cb a b c d)
  (append! cb (list a b c d)))

(define (code-builder-put5! cb a b c d e)
  (append! cb (list a b c d e)))

(define (code-builder-append! cb1 cb2)
  (let loop ([e (cdr cb2)])
    (cond
     [(null? e)
      '()]
     [else
      (code-builder-put1! cb1 (car e))
      (loop (cdr e))])))

(define (code-builder-emit cb)
  (cdr cb))
;; moved to freeproc.cpp end

;; code-builder synonym
(define-macro (cput! cb . more)
  (match more
    [() '()]
    [(a b c d e . f)
     `(begin (code-builder-put5! ,cb ,a ,b ,c ,d ,e)
             (cput! ,cb ,@f))]
    [(a b c d . e)
     `(begin (code-builder-put4! ,cb ,a ,b ,c ,d)
             (cput! ,cb ,@e))]
    [(a b c . d)
     `(begin (code-builder-put3! ,cb ,a ,b ,c)
             (cput! ,cb ,@d))]
    [(a b . c)
     `(begin (code-builder-put2! ,cb ,a ,b)
             (cput! ,cb ,@c))]
    [(a . b)
     `(begin (code-builder-put1! ,cb ,a)
             (cput! ,cb ,@b))]))

(define (pass3/collect-free cb frees-here locals frees)
  (fold (lambda (i accum)
          (let1 size (pass3/compile-refer cb i locals frees)
            (cput! cb 'PUSH)
            (+ size accum))) 0 (reverse frees-here)))

(define (pass3/symbol-lookup lvar locals frees return-local return-free)
  (let next-local ([locals locals] [n 0])
    (if (null? locals)
        (let next-free ([free frees] [n 0])
          (cond [(null? free)
                 (error "pass3/symbol-lookup bug? Unknown lvar:" lvar)]
                [(eq? (car free) lvar)
                 (return-free n)]
                [else
                 (next-free (cdr free) (+ n 1))]))
        (if (eq? (car locals) lvar)
            (return-local n)
            (next-local (cdr locals) (+ n 1))))))

(define (pass3/compile-refer cb lvar locals frees)
  (pass3/symbol-lookup lvar locals frees
                       (lambda (n)
                         (cput! cb 'REFER_LOCAL n)
                         0)
                       (lambda (n)
                         (cput! cb 'REFER_FREE n)
                         0)))

(define (pass3/compile-assign cb lvar locals frees)
  (pass3/symbol-lookup lvar locals frees
                       (lambda (n)
                         (cput! cb 'ASSIGN_LOCAL n)
                         0)
                       (lambda (n)
                         (cput! cb 'ASSIGN_FREE n)
                         0)))

(define (pass3/make-boxes cb sets vars)
  ($for-each1-with-rindex (lambda (index var)
                            (if (memq var sets)
                                (cput! cb 'BOX index)))
                          vars))

(define pass3/dispatch-table (make-vector $INSN-NUM))

(define (pass3/register insn proc)
  (vector-set! pass3/dispatch-table insn proc))

(define (pass3/$const cb iform locals frees can-frees sets tail)
  (cput! cb 'CONSTANT ($const.val iform))
  0)

(define (pass3/$it cb iform locals frees can-frees sets tail) 0)

(define (pass3/$list cb iform locals frees can-frees sets tail)
  (let1 args ($list.args iform)
    (begin0
      (fold (lambda (i accum)
              (let1 size (pass3/rec cb i locals frees can-frees sets tail)
                (cput! cb 'PUSH)
                (+ size accum))) 0 args)
      (cput! cb 'LIST (length args)))))

;; $local-lef is classified into REFER_LOCAL and REFER_FREE
(define (pass3/$local-ref cb iform locals frees can-frees sets tail)
  (pass3/compile-refer cb ($local-ref.lvar iform) locals frees)
  (when (memq ($local-ref.lvar iform) sets)
    (cput! cb 'INDIRECT))
  0)

;; $local-assign is classified into ASSIGN_LOCAL and ASSIGN_FREE
(define (pass3/$local-assign cb iform locals frees can-frees sets tail)
  (let ([val-stack-size (pass3/rec cb ($local-assign.val iform) locals frees can-frees sets #f)]
        [var-stack-size (pass3/compile-assign cb ($local-assign.lvar iform) locals frees)])
    (+ val-stack-size var-stack-size)))

(define top-level-sym (string->symbol "top level "))

(define (merge-libname-sym libname sym)
  (string->symbol
   (string-append (symbol->string libname)
                  ":$:"
                  (symbol->string sym))))

;; $global-lef is classified into REFER_GLOBAL and REFER_FREE
(define (pass3/$global-ref cb iform locals frees can-frees sets tail)
  (let1 sym ($global-ref.sym iform)
    (let next-free ([free frees] [n 0])
      (cond [(null? free)
             (cput! cb 'REFER_GLOBAL (merge-libname-sym ($global-ref.libname iform) sym))
             0]
            [(eq? ($lvar.sym (car free)) sym)
             (cput! cb 'REFER_FREE n)
             0]
            [else
             (next-free (cdr free) (+ n 1))]))))

(define (pass3/$global-assign cb iform locals frees can-frees sets tail)
  (let1 sym ($global-assign.sym iform)
    (let next-free ([free frees] [n 0])
      (cond
       [(null? free)
        (begin0
          (pass3/rec cb ($global-assign.val iform) locals frees can-frees sets #f)
          (cput! cb
                 'ASSIGN_GLOBAL
                 (merge-libname-sym ($global-assign.libname iform) sym)))]
       [(eq? ($lvar.sym (car free)) sym)
        (begin0
         (pass3/rec cb ($global-assign.val iform) locals frees can-frees sets #f)
         (cput! cb 'ASSIGN_FREE n))]
       [else
        (next-free (cdr free) (+ n 1))]))))

(define (pass3/$seq cb iform locals frees can-frees sets tail)
  (let loop ([form ($seq.body iform)]
             [size 0])
    (cond
     [(null? form) size]
     [else
      (let1 tail? (if (null? (cdr form)) tail #f)
        (loop (cdr form)
              (+ size (pass3/rec cb (car form) locals frees can-frees sets tail?))))])))

(define (pass3/$undef cb iform locals frees can-frees sets tail)
  (cput! cb 'UNDEF)
  0)

(define (pass3/$asm cb iform locals frees can-frees sets tail)
  (define (compile-1arg insn args)
    (begin0
      (pass3/rec cb (first args) locals frees can-frees sets #f)
      (cput! cb insn)))
  (define (compile-2arg insn args)
    (let ([x (pass3/compile-arg cb (first args) locals frees can-frees sets #f)]
          [y (pass3/rec cb (second args) locals frees can-frees sets #f)])
      (cput! cb insn)
      (+ x y)))
  (define (compile-3arg insn args)
    (let ([x (pass3/compile-arg cb (first args) locals frees can-frees sets #f)]
          [y (pass3/compile-arg cb (second args) locals frees can-frees sets #f)]
          [z (pass3/rec cb (third args) locals frees can-frees sets #f)])
      (cput! cb insn)
      (+ x y z)))
  (define (compile-n-args args)
    (let loop ([args args]
               [stack-size 0])
      (cond
       [(null? args) stack-size]
       [(null? (cdr args)) ;; last argument is not pushed.
        (+ stack-size (pass3/rec cb (car args) locals frees can-frees sets #f))]
       [else
        (loop (cdr args)
              (+ stack-size (pass3/compile-arg cb (car args) locals frees can-frees sets #f)))])))
  (let1 args ($asm.args iform)
    (case ($asm.insn iform)
      [(NUMBER_ADD)        (compile-2arg   'NUMBER_ADD      args)]
      [(NUMBER_SUB)        (compile-2arg   'NUMBER_SUB      args)]
      [(NUMBER_MUL)        (compile-2arg   'NUMBER_MUL      args)]
      [(NUMBER_DIV)        (compile-2arg   'NUMBER_DIV      args)]
      [(NUMBER_EQUAL)      (compile-2arg   'NUMBER_EQUAL    args)]
      [(NUMBER_GE)         (compile-2arg   'NUMBER_GE       args)]
      [(NUMBER_GT)         (compile-2arg   'NUMBER_GT       args)]
      [(NUMBER_LT)         (compile-2arg   'NUMBER_LT       args)]
      [(NUMBER_LE)         (compile-2arg   'NUMBER_LE       args)]
      [(CONS)              (compile-2arg   'CONS            args)]
      [(CAR)               (compile-1arg   'CAR             args)]
      [(CDR)               (compile-1arg   'CDR             args)]
      [(CAAR)              (compile-1arg   'CAAR            args)]
      [(CADR)              (compile-1arg   'CADR            args)]
      [(CDAR)              (compile-1arg   'CDAR            args)]
      [(CDDR)              (compile-1arg   'CDDR            args)]
      [(SET_CDR)           (compile-2arg   'SET_CDR         args)]
      [(SET_CAR)           (compile-2arg   'SET_CAR         args)]
      [(MAKE_VECTOR)       (compile-2arg   'MAKE_VECTOR     args)]
      [(VECTOR_LENGTH)     (compile-1arg   'VECTOR_LENGTH   args)]
      [(VECTOR_SET)        (compile-3arg   'VECTOR_SET      args)]
      [(VECTOR_REF)        (compile-2arg   'VECTOR_REF      args)]
      [(EQ)                (compile-2arg   'EQ              args)]
      [(EQV)               (compile-2arg   'EQV             args)]
      [(EQUAL)             (compile-2arg   'EQUAL           args)]
      [(PAIR_P)            (compile-1arg   'PAIR_P          args)]
      [(NULL_P)            (compile-1arg   'NULL_P          args)]
      [(SYMBOL_P)          (compile-1arg   'SYMBOL_P        args)]
      [(VECTOR_P)          (compile-1arg   'VECTOR_P        args)]
      [(NOT)               (compile-1arg   'NOT             args)]
      [(OPEN_INPUT_FILE)   (compile-1arg   'OPEN_INPUT_FILE args)]
      [(READ)              (compile-1arg   'READ            args)]
      [(READ_CHAR)         (compile-1arg   'READ_CHAR       args)]
      [(VALUES)
       (begin0
         (compile-n-args args)
         (cput! cb 'VALUES (length args)))]
      [(APPLY)
       (let1 end-of-frame (make-label)
         (cput! cb 'FRAME (ref-label end-of-frame))
         (let1 arg2-size (pass3/rec cb (second args) locals frees can-frees sets #f)
           (cput! cb 'PUSH)
           (let1 arg1-size (pass3/rec cb (first args) locals frees can-frees sets #f)
             (cput! cb 'APPLY end-of-frame)
             (+ arg1-size arg2-size))))]
      [else
       (print "unknown insn on pass3/$asm")])))

(define (pass3/$if cb iform locals frees can-frees sets tail)
  (let ([end-of-else   (make-label)]
        [begin-of-else (make-label)])
    (let1 test-size (pass3/rec cb ($if.test iform) locals frees can-frees sets #f)
      (cput! cb 'TEST (ref-label begin-of-else))
      (let1 then-size (pass3/rec cb ($if.then iform) locals frees can-frees sets tail)
        (cput! cb
               'UNFIXED_JUMP
               (ref-label end-of-else)
               begin-of-else)
        (let1 else-size (pass3/rec cb ($if.else iform) locals frees can-frees sets tail)
          (cput! cb end-of-else)
          (+ test-size then-size else-size))))))

(define (pass3/$define cb iform locals frees can-frees sets tail)
  (begin0
    (pass3/rec cb ($define.val iform) locals frees can-frees sets #f)
    (cput! cb 'DEFINE_GLOBAL (merge-libname-sym ($define.libname iform)
                                                ($define.sym iform)))))

(define (pass3/compile-arg cb arg locals frees can-frees sets tail)
  (let1 size (pass3/rec cb arg locals frees can-frees sets #f)
    (cput! cb 'PUSH)
    (+ size 1)))

(define (pass3/compile-args cb args locals frees can-frees sets tail)
  (fold (lambda (i accum)
          (let1 size (pass3/compile-arg cb i locals frees can-frees sets tail)
            (+ size accum)))
        0 args))

(define (pass3/$call cb iform locals frees can-frees sets tail)
  (case ($call.type iform)
    [(jump)
     (let1 label ($lambda.body ($call.proc ($call.proc iform)))
       (cput! cb 'REDUCE (length ($call.args iform)))
       (begin0
         (pass3/compile-args cb ($call.args iform) locals frees can-frees sets #f)
         (cput! cb
                'SHIFT
                (length ($call.args iform))
                (length ($call.args iform))
                'UNFIXED_JUMP
                label)))]
    [(embed)
     (let* ([label ($lambda.body ($call.proc iform))]
            [body ($label.body label)]
            [vars ($lambda.lvars ($call.proc iform))]
            [frees-here (pass3/find-free body
                                         vars
                                         (append2 locals (append2 frees can-frees)))]
            [sets-for-this-lvars (pass3/find-sets body vars)])
       (cput! cb 'LET_FRAME)
       (let1 free-size (if (> (length frees-here) 0)
                           (pass3/collect-free cb frees-here locals frees)
                           0)
         (when (> (length frees-here) 0)
           (cput! cb 'DISPLAY (length frees-here)))
         (let1 args-size (pass3/compile-args cb ($call.args iform) locals frees-here can-frees sets #f)
           (pass3/make-boxes cb sets-for-this-lvars vars)
           (cput! cb
                  'ENTER
                  (length ($call.args iform))
                  label)
           (let1 body-size (pass3/rec cb
                                      body
                                      vars
                                      frees-here
;                                      (%set-union can-frees vars)
                                      (append2 can-frees vars)
                                      (append2 sets-for-this-lvars sets)
;;                                       (%set-union (append2 sets-for-this-lvars sets)
;;                                                   (%set-intersect sets frees-here))
                                      (if tail (+ tail (length vars) 2) #f)) ;; 2 is size of LET_FRAME
             (cput! cb 'LEAVE (length ($call.args iform)))
             (+ args-size body-size free-size)))))]
    [else
     (let1 end-of-frame (make-label)
       ;;
       ;; How tail context call be optimized.
       ;;
       ;;   On ((lambda () (a 3))), (a 3) is tail call.
       ;;   Normally, after this call, VM jmp to saved continuation (code, ip, sp) with (RETURN n) instruction.
       ;;   This continuation is saved by FRAME instruction before applying (a 3).
       ;;   Because (a 3) is tail call, continuation of (a 3) is exactly equal to continuation of ((lambda () ...)).
       ;;   So we don't have to execute FRAME, instead we can use FRAME informtion which is saved before applying ((lambda () ...)).
       ;;   To access the FRAME informtion, we remove arguments for a, so we do this SHIFT.
       ;;
       (unless tail
         (cput! cb 'FRAME (ref-label end-of-frame)))
       (let* ([args-size (pass3/compile-args cb ($call.args iform) locals frees can-frees sets #f)]
              [proc-size (pass3/rec cb ($call.proc iform) locals frees can-frees sets #f)])
         (when tail
           (cput! cb 'SHIFT (length ($call.args iform)) tail))
         (cput! cb 'CALL (length ($call.args iform)))
         (unless tail
           (cput! cb end-of-frame))
         (+ args-size proc-size)))]))

(define (pass3/$call-cc cb iform locals frees can-frees sets tail)
  (let1 end-of-frame (make-label)
    (unless tail
      (cput! cb 'FRAME (ref-label end-of-frame)))
    (cput! cb 'MAKE_CONTINUATION (if tail 1 0) 'PUSH)
    (begin0
      (pass3/rec cb ($call-cc.proc iform) locals frees can-frees sets #f)
      (when tail
        (cput! cb 'SHIFT 1 tail))
      (cput! cb 'CALL 1)
      (unless tail
        (cput! cb end-of-frame)))))

(define (pass3/$lambda cb iform locals frees can-frees sets tail)
  (let* ([vars ($lambda.lvars iform)]
         [body ($lambda.body iform)]
         [frees-here (pass3/find-free body
                                      vars
                                      (append2 locals (append2 frees can-frees)))]
         [sets-for-this-lvars  (pass3/find-sets body vars)]
         [end-of-closure (make-label)]
         [lambda-cb (make-code-builder)])
    (let1 free-size (if (> (length frees-here) 0)
                        (pass3/collect-free cb frees-here locals frees)
                        0)
      (cput! cb
             'CLOSURE
             (ref-label end-of-closure)
             (length vars)                                            ;; length of arguments
             (> ($lambda.optarg iform) 0)                             ;; optional-arg?
             (length frees-here))                                     ;; number of free variables
      ;; we want to know stack size of lambda body, before emit.
      (let1 body-size (pass3/rec lambda-cb
                                 body
                                 vars
                                 frees-here
;                                 (%set-union can-frees vars)
                                 (append2 can-frees vars) ;; can-frees and vars don't have common lvars.
                                 (append2 sets-for-this-lvars sets)
;;                                  (%set-union (append2 sets-for-this-lvars sets)
;;                                              (%set-intersect sets frees-here))
                                 (length vars))
        (cput! cb
               (+ body-size free-size (length vars) 4) ;; max-stack 4 is sizeof frame
               ($lambda.src iform))                    ;; source code information
        (pass3/make-boxes cb sets-for-this-lvars vars)
        (code-builder-append! cb lambda-cb)
        (cput! cb 'RETURN (length vars) end-of-closure)
        0))))

(define (pass3/$receive cb iform locals frees can-frees sets tail)
  (let* ([vars ($receive.lvars iform)]
         [body ($receive.body iform)]
         [frees-here (append2
                      (pass3/find-free ($receive.vals iform) locals (append2 locals (append2 frees can-frees)))
                      (pass3/find-free body
                                       vars
                                       (append2 locals (append2 frees can-frees))))]
         [sets-for-this-lvars (pass3/find-sets body vars)])
    (cput! cb 'LET_FRAME)
    (let1 free-size (if (> (length frees-here) 0)
                        (pass3/collect-free cb frees-here locals frees)
                        0)
      (when (> (length frees-here) 0)
        (cput! cb 'DISPLAY (length frees-here)))
      (let1 vals-size (pass3/rec cb ($receive.vals iform) locals frees-here can-frees sets #f)
        (cput! cb 'RECEIVE ($receive.reqargs iform) ($receive.optarg  iform))
        (pass3/make-boxes cb sets-for-this-lvars vars)
        (cput! cb 'ENTER (length vars))
        (let1 body-size (pass3/rec cb
                                   body
                                   vars
                                   frees-here
;                                   (%set-union can-frees vars)
                                   (append2 can-frees vars)
;;                                    (%set-union (append2 sets-for-this-lvars sets)
;;                                                (%set-intersect sets frees-here))
                                   (append2 sets-for-this-lvars sets)
                                   (if tail (+ tail (length vars) 2) #f)) ;; 2 is size of LET_FRAME
          (cput! cb 'LEAVE (length vars))
          (+ body-size vals-size free-size))))))

(define (pass3/$let cb iform locals frees can-frees sets tail)
  (if (eq? ($let.type iform) 'rec)
      (pass3/letrec cb iform locals frees can-frees sets tail)
      (let* ([vars ($let.lvars iform)]
             [body ($let.body iform)]
             [frees-here (append2
                          ($append-map1 (lambda (i) (pass3/find-free i locals (append2 locals (append2 frees can-frees)))) ($let.inits iform))
                          (pass3/find-free body
                                           vars
                                           (append2 locals (append2 frees can-frees))))]
             [sets-for-this-lvars (pass3/find-sets body vars)])
        ;; tail-call doesn't work yet
        ;;       ,@(if tail '() '(LET_FRAME))
        ;;       ,@(if (> (length frees-here) 0) (pass3/collect-free frees-here locals frees) '())
        ;;       ,@(if (> (length frees-here) 0) (list 'DISPLAY (length frees-here)) '())
        ;;       ,@args-code
        ;;       ,@boxes-code
        ;;       ENTER
        ;;       ,@body-code
        ;;       ,@(if tail (list 'SHIFT (length vars) tail) (list 'LEAVE (length vars))))))
        ;; non-tail call works fine.
        (cput! cb 'LET_FRAME)
        (let1 free-size (if (> (length frees-here) 0) (pass3/collect-free cb frees-here locals frees) 0)
          (when (> (length frees-here) 0)
            (cput! cb 'DISPLAY (length frees-here)))
          (let1 args-size (pass3/compile-args cb ($let.inits iform) locals frees-here can-frees sets tail)
            (pass3/make-boxes cb sets-for-this-lvars vars)
            (cput! cb 'ENTER (length vars))
            (let1 body-size (pass3/rec cb
                                       body
                                       vars
                                       frees-here
;                                       (%set-union can-frees vars)
                                       (append2 can-frees vars)
                                       (append2 sets-for-this-lvars sets)
;;                                        (%set-union (append2 sets-for-this-lvars sets)
;;                                                    (%set-intersect sets frees-here))
                                       (if tail (+ tail (length vars) 2) #f)) ;; 2 is size of LET_FRAME
              (cput! cb 'LEAVE (length vars))
              (+ body-size args-size free-size)))))))

(define (pass3/letrec cb iform locals frees can-frees sets tail)
  (let* ([vars ($let.lvars iform)]
         [body ($let.body iform)]
         [frees-here (append2
                      ($append-map1 (lambda (i) (pass3/find-free i vars (append2 locals (append2 frees can-frees)))) ($let.inits iform))
                      (pass3/find-free body
                                       vars
                                       (append2 locals (append2 frees can-frees))))]
         ;; each vars can be set!
         [sets-for-this-lvars  (append vars (pass3/find-sets body vars) ($append-map1 (lambda (i) (pass3/find-sets i vars)) ($let.inits iform)))]
         [args ($let.inits iform)])
    (cput! cb 'LET_FRAME)
    (let1 free-size (if (> (length frees-here) 0) (pass3/collect-free cb frees-here locals frees) 0)
      (when (> (length frees-here) 0)
        (cput! cb 'DISPLAY (length frees-here)))
      (let loop ([args args]) ;; init code
        (cond
         [(null? args) '()]
         [else
          (cput! cb 'UNDEF 'PUSH)
          (loop (cdr args))]))
      (pass3/make-boxes cb sets-for-this-lvars vars)
      (cput! cb 'ENTER (length vars))
      (let* ([new-can-frees (append2 can-frees vars)]
             [assign-size
              (let loop ([args  args]
                         [size  0]
                         [index 0])
                (cond
                 [(null? args) size]
                 [else
                  (let1 stack-size (pass3/rec cb (car args) vars frees-here
;                                             (%set-union can-frees vars)
                                              new-can-frees
                                              (append2 sets-for-this-lvars sets)
;;                                               (%set-union (append2 sets-for-this-lvars sets)
;;                                                           (%set-intersect sets frees-here))
                                              #f)
                    (cput! cb 'ASSIGN_LOCAL index)
                    (loop (cdr args)
                          (+ stack-size size)
                          (+ index 1)))]))])
             (let1 body-size (pass3/rec cb
                                        body
                                        vars
                                        frees-here
    ;                                   (%set-union can-frees vars)
                                        new-can-frees
                                        (append2 sets-for-this-lvars sets)
;;                                    (%set-union (append2 sets-for-this-lvars sets)
;;                                                (%set-intersect sets frees-here))
                                   (if tail (+ tail (length vars) 2) #f)) ;; 2 is size of LET_FRAME
          (cput! cb 'LEAVE (length vars))
          (+ free-size assign-size body-size))))))

(define (pass3/$import cb iform locals frees can-frees sets tail)
  (define (rec form)
    (for-each
     (lambda (s)
       (let* ([libname      ($import-spec.libname s)]
              [lib          (hashtable-ref libraries libname)]
              [end-of-frame (make-label)])
         (rec ($library.import lib))
         (cput! cb
                'FRAME  ;; We execute (RETURN 0) in library body
                (ref-label end-of-frame)
                'IMPORT
                libname
                end-of-frame)))
     ($import.import-specs form))
    0)
  (rec iform))

(define (pass3/$library cb iform locals frees can-frees sets tail)
  (cput! cb 'LIBRARY ($library.name iform) iform)
  0)

(pass3/register $CONST         pass3/$const)
(pass3/register $LAMBDA        pass3/$lambda)
(pass3/register $LOCAL-REF     pass3/$local-ref)
(pass3/register $LOCAL-ASSIGN  pass3/$local-assign)
(pass3/register $GLOBAL-ASSIGN pass3/$global-assign)
(pass3/register $GLOBAL-REF    pass3/$global-ref)
(pass3/register $SEQ           pass3/$seq)
(pass3/register $UNDEF         pass3/$undef)
(pass3/register $IF            pass3/$if)
(pass3/register $ASM           pass3/$asm)
(pass3/register $DEFINE        pass3/$define)
(pass3/register $CALL          pass3/$call)
(pass3/register $CALL-CC       pass3/$call-cc)
(pass3/register $LET           pass3/$let)
(pass3/register $LIST          pass3/$list)
(pass3/register $LIBRARY       pass3/$library)
(pass3/register $IMPORT        pass3/$import)
(pass3/register $IT            pass3/$it)
(pass3/register $RECEIVE       pass3/$receive)

(define (pass3/rec cb iform locals frees can-frees sets tail)
  ((vector-ref pass3/dispatch-table (vector-ref iform 0)) cb iform locals frees can-frees sets tail))

(define (pass3 iform)
  (let1 cb (make-code-builder)
    (pass3/rec cb iform '() *free-lvars* '() '() #f)
    (code-builder-emit cb)))
(define (pass4 lst)
  (pass4/fixup-labels (list->vector (append2 lst '(HALT)))))

(define (compile-library-body! lib)
  (let1 body ($append-map1 (lambda (sexp) (pass3 (pass2/optimize (pass1/sexp->iform (pass1/expand sexp) lib '() #f) '()) )) ($library.body lib))
    ($library.set-compiled-body! lib (pass4 `(,@body RETURN 0)))))

(define (compile-partial sexp . lib)
  (let1 ss (pass1/expand sexp)
    (vector->list
     (pass4/fixup-labels
      (list->vector
       (merge-insn
         (pass3
          (pass2/optimize
           (pass1/sexp->iform ss (if (null? lib) top-level-library (car lib)) '() #f) '()))))))))

;; todo local macro
(define-macro (pass4/fixup-labels-clollect insn)
  `(begin
     (vector-set! ret j ,insn)
     (vector-set! ret (+ j 1) (vector-ref v (+ i 1)))
     (loop (+ i 2) (+ j 2))))

(define-macro (pass4/fixup-labels-insn insn)
  `(let1 label (hashtable-ref labels(vector-ref code (+ i 1)) #f)
     (cond
      [label
       (vector-set! code i ,insn)
       (vector-set! code (+ i 1) (- label i 1)) ;; jump point
       (loop (+ i 2))]
      [else
       (loop (+ i 1))])))

(define (pass4/fixup-labels v)
  (define (collect-labels)
    (let* ([len (vector-length v)]
           [ret (make-vector len 'NOP)]
           [labels (make-eq-hashtable)])
      (let loop ([i 0]
                 [j 0])
        (cond
         [(= i len) (values ret labels)]
         [else
          (let1 insn (vector-ref v i)
            (cond
             [(eq? insn 'UNFIXED_JUMP)          (pass4/fixup-labels-clollect 'UNFIXED_JUMP)]
             [(eq? insn 'TEST)                  (pass4/fixup-labels-clollect 'TEST)]
             [(eq? insn 'NUMBER_LE_TEST)        (pass4/fixup-labels-clollect 'NUMBER_LE_TEST)]
             [(eq? insn 'NOT_TEST)              (pass4/fixup-labels-clollect 'NOT_TEST)]
             [(eq? insn 'REFER_LOCAL0_EQV_TEST) (pass4/fixup-labels-clollect 'REFER_LOCAL0_EQV_TEST)]
             [(eq? insn 'FRAME)                 (pass4/fixup-labels-clollect 'FRAME)]
             [(eq? insn 'PUSH_FRAME)            (pass4/fixup-labels-clollect 'PUSH_FRAME)]
             [(eq? insn 'CLOSURE)               (pass4/fixup-labels-clollect 'CLOSURE)]
             [(and (vector? insn) (> (vector-length insn) 0) (tag? insn $LABEL))
              (hashtable-set! labels insn j)
              (loop (+ i 1) j)]  ;; save the location of label)
             [else
              (vector-set! ret j insn)
              (loop (+ i 1) (+ j 1))]))]))))
  (receive (code labels) (collect-labels)
    (let1 len (vector-length code)
    (let loop ([i 0])
      (cond
       [(= i len) code]
       [else
        (let1 insn (vector-ref code i)
          (cond
           [(eq? insn 'UNFIXED_JUMP)          (pass4/fixup-labels-insn 'LOCAL_JMP)]
           [(eq? insn 'CLOSURE)               (pass4/fixup-labels-insn 'CLOSURE)]
           [(eq? insn 'TEST)                  (pass4/fixup-labels-insn 'TEST)]
           [(eq? insn 'NUMBER_LE_TEST)        (pass4/fixup-labels-insn 'NUMBER_LE_TEST)]
           [(eq? insn 'NOT_TEST)              (pass4/fixup-labels-insn 'NOT_TEST)]
           [(eq? insn 'REFER_LOCAL0_EQV_TEST) (pass4/fixup-labels-insn 'REFER_LOCAL0_EQV_TEST)]
           [(eq? insn 'FRAME)                 (pass4/fixup-labels-insn 'FRAME)]
           [(eq? insn 'PUSH_FRAME)            (pass4/fixup-labels-insn 'PUSH_FRAME)]
           [else (loop (+ i 1))]))])))))


(define *free-lvars* ($map1 (lambda (p) ($lvar p '() 0 0)) *free-vars-decl*))

(define (merge-insn sexp)
  (define (iter s)
    (cond
     [(null? s) '()]
     [else
        (match s
          [('REFER_LOCAL0_PUSH 'CONSTANT . rest)
           (iter `(REFER_LOCAL0_PUSH_CONSTANT ,@rest))]
          [('REFER_LOCAL1_PUSH 'CONSTANT . rest)
           (iter `(REFER_LOCAL1_PUSH_CONSTANT ,@rest))]
          [('REFER_LOCAL 1 'PUSH . rest)
           (iter `(REFER_LOCAL1_PUSH ,@rest))]
          [('REFER_LOCAL 0 'PUSH . rest)
           (iter `(REFER_LOCAL0_PUSH ,@rest))]
          [('REFER_LOCAL 0 . rest)
           (iter `(REFER_LOCAL0 ,@rest))]
          ;; N.B.
          ;; compiled pass3/$asm code has list '(CONSTANT NUMBER_SUB PUSH), ignore it.
          [((and x (not 'CONSTANT)) 'NUMBER_SUB 'PUSH . rest)
           (iter `(, x NUMBER_SUB_PUSH ,@rest))]
          [('PUSH 'ENTER . rest)
           (iter (cons 'PUSH_ENTER rest))]
          [('CONSTANT v 'PUSH . rest)
           (iter `(CONSTANT_PUSH ,v ,@rest))]
          [('REFER_FREE 0 'PUSH . rest)
           (iter `(REFER_FREE0_PUSH ,@rest))]
          [('REFER_FREE 1 'PUSH . rest)
           (iter `(REFER_FREE1_PUSH ,@rest))]
          [('REFER_FREE 2 'PUSH . rest)
           (iter `(REFER_FREE2_PUSH ,@rest))]
          [('REFER_FREE n 'PUSH . rest)
           (iter `(REFER_FREE_PUSH ,n ,@rest))]
          [('REFER_FREE 0 . rest)
           (iter `(REFER_FREE0 ,@rest))]
          [('REFER_FREE 1 . rest)
           (iter `(REFER_FREE1 ,@rest))]
          [('REFER_FREE 2 . rest)
           (iter `(REFER_FREE2 ,@rest))]
          [('REFER_LOCAL 1 . rest)
           (iter `(REFER_LOCAL1 ,@rest))]
          [('REFER_LOCAL 2 . rest)
           (iter `(REFER_LOCAL2 ,@rest))]
          [('LEAVE 1 . rest)
           (iter `(LEAVE1 ,@rest))]
          [('NUMBER_LE 'TEST . rest)
           (iter `(NUMBER_LE_TEST ,@rest))]
          [('NUMBER_ADD 'PUSH . rest)
           (iter `(NUMBER_ADD_PUSH ,@rest))]
          [('RETURN 1 . rest)
           (iter `(RETURN1 ,@rest))]
          [('RETURN 2 . rest)
           (iter `(RETURN2 ,@rest))]
          [('RETURN 3 . rest)
           (iter `(RETURN3 ,@rest))]
          [('CALL 2 . rest)
           (iter `(CALL2 ,@rest))]
          [('REFER_LOCAL0 'EQV 'TEST . rest)
           (iter `(REFER_LOCAL0_EQV_TEST ,@rest))]
          [('PUSH 'CONSTANT . rest)
           (iter `(PUSH_CONSTANT ,@rest))]
          [('PUSH 'FRAME . rest)
           (iter `(PUSH_FRAME ,@rest))]
          ;; N.B.
          ;; compiled pass3/$asm code has list '(CONSTANT_PUSH PUSH FRAME), ignore it.
          [((and x (not 'CONSTANT_PUSH)) 'PUSH 'FRAME . rest)
           (iter `(, x PUSH_FRAME ,@rest))]
;;           [('PUSH 'FRAME . rest)
;;            (iter `(PUSH_FRAME ,@rest))]
          [('REFER_FREE 3 . rest)
           (iter `(REFER_FREE3 ,@rest))]
          [('REFER_LOCAL 3 . rest)
           (iter `(REFER_LOCAL3 ,@rest))]
          [('CAR 'PUSH . rest)
           (iter `(CAR_PUSH ,@rest))]
          [('CDR 'PUSH . rest)
           (iter `(CDR_PUSH ,@rest))]
          [('REFER_FREE0 'INDIRECT . rest)
           (iter `(REFER_FREE0_INDIRECT ,@rest))]
          [('REFER_LOCAL2 'PUSH . rest)
           (iter `(REFER_LOCAL2_PUSH ,@rest))]
          [('SHIFT m n 'CALL o . rest)
           (iter `(SHIFT_CALL ,m ,n ,o ,@rest))]
          [('CALL 3 . rest)
           (iter `(CALL3 ,@rest))]
          [('REFER_FREE1 'INDIRECT . rest)
           (iter `(REFER_FREE1_INDIRECT ,@rest))]
          [('NOT 'TEST . rest)
           (iter `(NOT_TEST ,@rest))]
          [('REFER_GLOBAL lib-id 'CALL n . rest)
           (iter `(REFER_GLOBAL_CALL ,lib-id ,n ,@rest))]
          [('REFER_LOCAL0 'NUMBER_ADD_PUSH . rest)
           (iter (cons 'REFER_LOCAL0_NUMBER_ADD_PUSH rest))]
          [('REFER_LOCAL0 'VECTOR_SET . rest)
           (iter (cons 'REFER_LOCAL0_VECTOR_SET rest))]
          [('REFER_LOCAL0 'VECTOR_REF . rest)
           (iter (cons 'REFER_LOCAL0_VECTOR_REF rest))]
          [('REFER_LOCAL n 'PUSH . rest)
           (iter `(REFER_LOCAL_PUSH ,n ,@rest))]
          [else
           (cons (car s) (iter (cdr s)))])]))
  sexp)

(define (compile sexp)
  (pass4 (merge-insn
          (pass3 (let1 x (pass2/optimize (pass1/sexp->iform (pass1/expand sexp) top-level-library '() #f) '())
                   x)
                 ))))

(define (compile-no-optimize sexp)
  (pass4 (pass3 (pass1/sexp->iform (pass1/expand sexp) top-level-library '() #f))))

(cond-expand
 [vm-outer?
  (define (main args)
    (if (= (length args) 2)
        (let1 port (open-string-input-port (second args))
          (write (compile (read port))))))
  (main (command-line))]
 [mosh #f]
 [else
  (define (main args)
    (if (= (length args) 2)
        (let1 port (open-string-input-port (second args))
          (write (compile (read port))))))])
