;; Optimization memo
;; 1. Do NOT use internal define. Because cost of closure creation is very high.
;;    Use global define or inline macro instead.
;; 2. Do NOT use fold or find, they alse create closure.
;; 3. (length ...) may be slow, so call only when necessary.

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
  (define for-all every)
  (define (set-source-info! a b) a)]
 [vm?
  (define (ungensym x) x)
  (define-macro (import-only module . syms)
    `(begin
       ,@(map (lambda (sym) `(define ,sym (with-module ,module ,sym))) syms)))
  (import-only gauche.internal extended-pair? extended-cons extended-list pair-attribute-get pair-attribute-set! pair-attributes)
  (define *command-line-args* '())
  (define (command-line) *command-line-args*)
  (define make-eq-hashtable make-hash-table)
  (define hashtable-set! hash-table-put!)
  (define hashtable-ref hash-table-get)
  (define hashtable-keys hash-table-keys)
  (define hashtable-for-each (lambda (proc ht) (hash-table-for-each ht proc)))
  (define dd (lambda a '()))
  (define pp (lambda a '()))
  (define for-all every)
  (define syntax-error error)
  (define find10 find)
  (define append2 append)
  (define appendA append)
  (define memq2 memq)
  (define df (lambda a '#f))
  (define print-stack (lambda a '#f))
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
 (define df format)
 (define dd display)
 (define pp print)
 (include "./free-vars-decl.scm")
 (define-macro (make-list-with-src-slot lst) lst)
 (define (command-line) *command-line-args*)
 (define (get-command-line) *command-line-args*) ;; required for psyntax
 (define (errorf form . args) (error 'compiler (apply format form args)))
  ])

;; inline map
(define-macro (imap proc lis)
  (let ([p (gensym)]
        [r (gensym)]
        [loop (gensym)])
    `(let ,loop ([,r '()]
                 [,p ,lis])
          (if (null? ,p)
              (reverse ,r)
              (,loop (cons (,proc (car ,p)) ,r) (cdr ,p))))))

(define (eq-hashtable-copy ht)
  (let1 ret (make-eq-hashtable)
    (hashtable-for-each
     (lambda (key value)
       (hashtable-set! ret key value))
     ht)
    ret))

(define (hashtable-set-true! ht keys)
  (let loop ([keys keys])
    (cond
     [(null? keys) ht]
     [else
      (hashtable-set! ht (car keys) #t)
      (loop (cdr keys))])))

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

(define (alist-cons obj1 obj2 obj3) (cons (cons obj1 obj2) obj3))

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

;; not used?
;; (define ($filter-map1 f l)
;;   (if (null? l)
;;       l
;;       (aif (f (car l))
;;            (cons it ($filter-map1 f (cdr l)))
;;            ($filter-map1 f (cdr l)))))

(define ($map1-with-tail f l)
  (if (null? l)
      l
      (cons (f (car l) (null? (cdr l))) ($map1-with-tail f (cdr l)))))

(define-macro ($append-map1 f l)
  `(apply append (imap ,f ,l)))

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
  (vector $CONST val))

(define-macro ($const.val iform) `(vector-ref ,iform 1))
(define-macro ($const.set-val! iform val) `(vector-set! ,iform 1 ,val))

;; struct $lvar
(define $LVAR 1)
(define ($lvar sym init-val ref-count set-count)
  (vector $LVAR sym init-val ref-count set-count))

(define ($lvar.sym-proc iform) (vector-ref iform 1))
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
(define ($let type lvars inits body tail? src . error)
  (vector $LET type lvars inits body tail? src (if (null? error) #f (car error))))

(define-macro ($let.type iform) `(vector-ref ,iform 1))
(define-macro ($let.lvars iform) `(vector-ref ,iform 2))
(define-macro ($let.inits iform) `(vector-ref ,iform 3))
(define-macro ($let.body iform) `(vector-ref ,iform 4))
(define-macro ($let.tail? iform) `(vector-ref ,iform 5))
(define-macro ($let.src iform) `(vector-ref ,iform 6))
(define-macro ($let.error iform) `(vector-ref ,iform 7))
(define-macro ($let.set-type! iform type) `(vector-set! ,iform 1 ,type))
(define-macro ($let.set-lvars! iform lvars) `(vector-set! ,iform 2 ,lvars))
(define-macro ($let.set-inits! iform inits) `(vector-set! ,iform 3 ,inits))
(define-macro ($let.set-body! iform body) `(vector-set! ,iform 4 ,body))
(define-macro ($let.set-tail?! iform tail?) `(vector-set! ,iform 5 ,tail?))
(define-macro ($let.set-src! iform src) `(vector-set! ,iform 6 ,src))
(define-macro ($let.set-error! iform error) `(vector-set! ,iform 7 ,error))

;; struct $seq
(define $SEQ 3)
(define ($seq body tail?)
  (vector $SEQ body tail?))

(define-macro ($seq.body iform) `(vector-ref ,iform 1))
(define-macro ($seq.tail? iform) `(vector-ref ,iform 2))
(define-macro ($seq.set-body! iform body) `(vector-set! ,iform 1 ,body))
(define-macro ($seq.set-tail?! iform tail?) `(vector-set! ,iform 2 ,tail?))

;; struct $lambda
(define $LAMBDA 4)
(define ($lambda src name reqargs optarg lvars body flag calls)
  (vector $LAMBDA src name reqargs optarg lvars body flag calls))

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

(define-macro ($lvar.ref-count++! lvar)
  `($lvar.set-ref-count! ,lvar (+ ($lvar.ref-count ,lvar) 1)))

(define-macro ($lvar.ref-count--! lvar)
  `($lvar.set-ref-count! ,lvar (- ($lvar.ref-count ,lvar) 1)))

(define-macro ($lvar.set-count++! lvar)
  `($lvar.set-set-count! ,lvar (+ 1 ($lvar.set-count ,lvar))))

(define-macro ($local-ref.copy dst src)
  `($local-ref.set-lvar! ,dst ($local-ref.lvar ,src)))

;; struct $local-ref
(define $LOCAL-REF 5)

;; moved to C++
(define ($local-ref lvar)
  ($lvar.ref-count++! lvar)
  (vector $LOCAL-REF lvar))

(define-macro ($local-ref.lvar iform) `(vector-ref ,iform 1))
(define-macro ($local-ref.set-lvar! iform lvar) `(vector-set! ,iform 1 ,lvar))


;; struct $local-assign
(define $LOCAL-ASSIGN 6)
(define ($local-assign lvar val)
  (vector $LOCAL-ASSIGN lvar val))

(define-macro ($local-assign.lvar iform) `(vector-ref ,iform 1))
(define-macro ($local-assign.val iform) `(vector-ref ,iform 2))
(define-macro ($local-assign.set-lvar! iform lvar) `(vector-set! ,iform 1 ,lvar))
(define-macro ($local-assign.set-val! iform val) `(vector-set! ,iform 2 ,val))

;; struct $global-ref
(define $GLOBAL-REF 7)
(define ($global-ref sym)
  (vector $GLOBAL-REF sym))

(define-macro ($global-ref.sym iform) `(vector-ref ,iform 1))
(define-macro ($global-ref.set-sym! iform sym) `(vector-set! ,iform 1 ,sym))

;; struct $global-assign
(define $GLOBAL-ASSIGN 8)
(define ($global-assign sym val)
  (vector $GLOBAL-ASSIGN sym val))

(define-macro ($global-assign.sym iform) `(vector-ref ,iform 1))
(define-macro ($global-assign.val iform) `(vector-ref ,iform 2))
(define-macro ($global-assign.set-sym! iform sym) `(vector-set! ,iform 1 ,sym))
(define-macro ($global-assign.set-val! iform val) `(vector-set! ,iform 2 ,val))

;; struct $undef
(define $UNDEF 9)
(define ($undef) (make-vector 1 $UNDEF))

;; struct $if
(define $IF 10)
(define ($if test then else)
  (vector $IF test then else))

(define-macro ($if.test iform) `(vector-ref ,iform 1))
(define-macro ($if.then iform) `(vector-ref ,iform 2))
(define-macro ($if.else iform) `(vector-ref ,iform 3))
(define-macro ($if.set-test! iform test) `(vector-set! ,iform 1 ,test))
(define-macro ($if.set-then! iform then) `(vector-set! ,iform 2 ,then))
(define-macro ($if.set-else! iform else) `(vector-set! ,iform 3 ,else))

;; struct $asm
(define $ASM 11)
(define ($asm insn args)
  (vector $ASM insn args))

(define-macro ($asm.insn iform) `(vector-ref ,iform 1))
(define-macro ($asm.args iform) `(vector-ref ,iform 2))
(define-macro ($asm.set-insn! iform insn) `(vector-set! ,iform 1 ,insn))
(define-macro ($asm.set-args! iform args) `(vector-set! ,iform 2 ,args))

;; struct $define
(define $DEFINE 12)
(define ($define sym val)
  (vector $DEFINE sym val))

(define-macro ($define.sym iform) `(vector-ref ,iform 1))
(define-macro ($define.val iform) `(vector-ref ,iform 2))
(define-macro ($define.set-sym! iform sym) `(vector-set! ,iform 1 ,sym))
(define-macro ($define.set-val! iform val) `(vector-set! ,iform 2 ,val))

;; struct $call-cc
(define $CALL-CC 13)
(define ($call-cc proc tail?)
  (vector $CALL-CC proc tail?))

(define-macro ($call-cc.proc iform) `(vector-ref ,iform 1))
(define-macro ($call-cc.tail? iform) `(vector-ref ,iform 2))
(define-macro ($call-cc.set-proc! iform proc) `(vector-set! ,iform 1 ,proc))
(define-macro ($call-cc.set-tail?! iform tail?) `(vector-set! ,iform 2 ,tail?))

;; struct $call
(define $CALL 14)
(define ($call proc args tail? type)
  (vector $CALL proc args tail? type 0 0))

(define-macro ($call.proc iform) `(vector-ref ,iform 1))
(define-macro ($call.args iform) `(vector-ref ,iform 2))
(define-macro ($call.tail? iform) `(vector-ref ,iform 3))
(define-macro ($call.type iform) `(vector-ref ,iform 4))
(define-macro ($call.depth iform) `(vector-ref ,iform 5))
(define-macro ($call.display-count iform) `(vector-ref ,iform 6))
(define-macro ($call.set-proc! iform proc) `(vector-set! ,iform 1 ,proc))
(define-macro ($call.set-args! iform args) `(vector-set! ,iform 2 ,args))
(define-macro ($call.set-tail?! iform tail?) `(vector-set! ,iform 3 ,tail?))
(define-macro ($call.set-type! iform type) `(vector-set! ,iform 4 ,type))
(define-macro ($call.set-depth! iform type) `(vector-set! ,iform 5 ,type))
(define-macro ($call.set-display-count! iform type) `(vector-set! ,iform 6 ,type))

(define $LABEL 15)

;; moved to C++
(define ($label body)
  (vector $LABEL body #f))

(define-macro ($label.body iform) `(vector-ref ,iform 1))
(define-macro ($label.set-body! iform body) `(vector-set! ,iform 1 ,body))
(define-macro ($label.visited? iform) `(vector-ref ,iform 2))
(define-macro ($label.set-visited?! iform body) `(vector-set! ,iform 2 ,body))

(define-macro (make-label) `($label #f))
(define-macro (ref-label l) l)

;; struct $list
(define $LIST 16)
(define ($list args)
  (vector $LIST args))

(define-macro ($list.args iform) `(vector-ref ,iform 1))
(define-macro ($list.set-args! iform args) `(vector-set! ,iform 1 ,args))

(define $IT 17)
(define ($it)
  (make-vector 1 $IT))

;; struct $receive
(define $RECEIVE 18)
(define ($receive lvars reqargs optarg vals body tail?)
  (vector $RECEIVE lvars reqargs optarg vals body tail?))

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

(define $INSN-NUM 19)

(define-macro (tag iform)
  `(vector-ref ,iform 0))

(define-macro (tag? iform t)
  `(eqv? ,t (tag ,iform)))

(define-macro (set-tag! iform t)
  `(vector-set! ,iform 0 ,t))

(define-macro (make-lvar sym)
  `($lvar ,sym '() 0 0))


;;--------------------------------------------------------------------
;;
;; Pass1
;;

;; Parse lambda vars and return (optional-arg? . vars).
;;   a       => (#t (a))
;;   (a b)   => (#f (a b))
;;   (a . b) => (#t (a b))
(define (parse-lambda-vars vars)
  (cond ((pair? vars)
         (let loop ((p vars) (ret '()))
           (cond ((null? p) (values #f vars))
                 ((pair? p) (loop (cdr p) (cons (car p) ret)))
                 (else
                  (values #t (reverse (cons p ret)))))))
        ((null? vars)
         (values #f '()))
        (else
         (values #t (list vars)))))

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

(define (lambda-has-define? sexp)
  (match sexp
    [(_ _ ('define . _) . _) #t]
    [else #f]))

(define (let1->let sexp)
  `(let ((,(second sexp) ,(third sexp)))
     ,@(cdddr sexp)))

(define (expand-let vars body)
  (let1 expanded-vars (fold-right (lambda (x y) (cons (list (first x) (pass1/expand (second x))) y)) '() vars)
    `(let ,expanded-vars ,@(imap pass1/expand body))))

(define (let-internal-define->letrec sexp)
  (let* ([body (cddr sexp)]
         [args (second sexp)]
         [ret  (find-serial-from-head (lambda (s) (and (pair? s) (eq? 'define (car s)))) body)]
         [defines (first ret)]
         [rest (second ret)]
         [letrec-body ($src `(letrec ,(map (lambda (d) (list (second d) (third d))) (map pass1/expand defines))
                         ,@rest) sexp)])
    ($src `(let ,args
             ,letrec-body) sexp)))

;; don't use internal define, if proc is supposed to be called so many times.
(define (pass1/expand sexp)
  (cond
   ((pair? sexp)
    (case (first sexp)
      [(quote) sexp]
      [(define-macro) sexp]
      [(define)
       (if (define-is-lambda? sexp)
           (pass1/expand (define->lambda sexp))
           ($src (imap (lambda (s) (pass1/expand s)) sexp) sexp))]
      [(let1)
       ($src (pass1/expand (let1->let sexp)) sexp)]
      [(let)
       (if (let-is-named? sexp)
           ($src (pass1/expand (named-let->letrec sexp)) sexp)
           (match sexp
             [('let vars ('define a . b) . more)
              ($src (pass1/expand ($src (let-internal-define->letrec sexp) sexp)) sexp)]
             [else
              ($src (expand-let (second sexp) (cddr sexp)) sexp)]))]
      [(let*)
       ($src (pass1/expand (let*->let sexp)) sexp)]
      ;; use receive instead of call-with-values.
      [(call-with-values)
       ($src (pass1/expand ($src `(receive vals (,(second sexp)) (apply ,(third sexp) vals)) sexp)) sexp)]
;;       [(cond)
;;        ($src (pass1/expand (cond->if sexp)) sexp)]
      [(lambda)
       (cond [(lambda-has-define? sexp)
              ($src (pass1/expand ($src (internal-define->letrec sexp) sexp)) sexp)]
             [else
              ($src (append! (list 'lambda (cadr sexp)) (imap pass1/expand (cddr sexp))) sexp)])]
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

;;  (find-d-serial-from-head even? '(2 4 6 8 3 5))
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
           (loop (append found (list (car lst))) (cdr lst))]
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

(define (let*->let sexp)
  (let ([args (cadr sexp)]
        [body (cddr sexp)])
    (car (let loop ([args args])
           (if (null? args)
               body
               ($src `((let (,(car args)) ,@(loop (cdr args)))) sexp))))))

;; (define (cond->if sexp)
;;   (define (make-if test then else)
;;     ;; don't use unquote splicing, it uses append
;;     (let ([then (if (> (length then) 1) (cons 'begin then) (car then))])
;;       `(if ,test ,then ,else)))
;;   (let loop ((clauses (cdr sexp)))
;;     (if (null? clauses)
;;         '(undef)
;;         (cond ((and (null? (cdr clauses)) (eq? 'else (caar clauses)))
;;                (if (> (length (cdar clauses)) 1)
;;                    (cons 'begin (cdar clauses))
;;                    (cadar clauses)))
;;               ((and (= 3 (length (car clauses))) (eq? '=> (cadar clauses)))
;;                (let ((tmp (gensym)))
;;                  `(let ((,tmp ,(caar clauses)))
;;                     (if ,tmp
;;                         (,(caddar clauses) ,tmp)
;;                         ,(loop (cdr clauses))))))
;;               ((= 1 (length (car clauses)))
;;                (let ((tmp (gensym)))
;;                  `(let ((,tmp ,(caar clauses)))
;;                     (if ,tmp
;;                         ,tmp
;;                         ,(loop (cdr clauses))))))
;;               (else
;;                (make-if (caar clauses) (cdar clauses) (loop (cdr clauses))))))))

(define (expand-clauses clauses tmpname)
  (let loop ([clauses clauses])
    (if (null? clauses)
        '()
        (if (eq? 'else (caar clauses))
            clauses
            (if (= 1 (length (caar clauses)))
                (cons `((eqv? ',(caaar clauses) ,tmpname) ,@(cdar clauses)) (loop (cdr clauses)))
                (cons `((memv ,tmpname ',(caar clauses)) ,@(cdar clauses)) (loop (cdr clauses))))))))

(define (case->cond sexp)
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
         (vars (imap car args))
         (vals (imap cadr args))
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
  (define (finalize-quasiquote mode arg)
    (cond ((eq? mode 'quote) (list 'quote arg))
          ((eq? mode 'unquote) arg)
          ((eq? mode 'unquote-splicing)
           (error 'quasiquote ",@ in invalid context" arg))
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
                                                              (return 'append
                                                                      (list car-arg (finalize-quasiquote
                                                                                     cdr-mode cdr-arg))))))
                                                      (else
                                                       (return 'cons
                                                               (list (finalize-quasiquote car-mode car-arg)
                                                                     (finalize-quasiquote cdr-mode cdr-arg))))))))))

  (define (descend-quasiquote-vector x level return)
    (descend-quasiquote (vector->list x) level
                        (lambda (mode arg)
                          (if (eq? mode 'quote)
                              (return 'quote x)
                              (return 'list->vector
                                      (list (finalize-quasiquote mode arg)))))))

  (define (interesting-to-quasiquote? x marker)
    (and (pair? x) (eq? (car x) marker)))


(define (expand-quasiquote x level)
  (descend-quasiquote x level finalize-quasiquote))
;; based on bdc-scheme end

;;--------------------------------------------------------------------
;;
;;  Pass1: Sexp into IForm
;;

;; same as (find (lambbda (l) (eq? object (car l))) lst), but it doesn't need to create closure
;; (define (find-with-car object lst)
;;   (if (null? lst)
;;       #f
;;       (if (eq? object (caar lst))
;;           (car lst)
;;           (find-with-car object (cdr lst)))))

(cond-expand
 [vm?
  ;; moved to CompilerProcedures.cpp
  ;; N.B. this procedure is still required by vm.scm
  (define (pass1/find-symbol-in-lvars symbol lvars)
    (cond
     [(null? lvars) #f]
     [(eq? symbol ($lvar.sym (car lvars))) (car lvars)]
     [else
      (pass1/find-symbol-in-lvars symbol (cdr lvars))]))]
 [else #f])

(define (pass1/refer->iform symbol lvars)
  (acond
   [(pass1/find-symbol-in-lvars symbol lvars) ;; don't use find, it requires closure creation.
;    ($lvar.ref-count++! it)
    ($local-ref it)]
   [#t ($global-ref symbol)]))

(define (pass1/assign symbol val lvars tail?)
 (match val
            [('lambda . more)
             (let1 iform (pass1/lambda->iform symbol val lvars)
               (acond
                [(pass1/find-symbol-in-lvars symbol lvars) ;; don't use find, it requires closure creation.
                 ($lvar.set-count++! it)
                 ($local-assign it iform)]
                [#t ($global-assign symbol iform)]))]
            [else
    (let1 iform (pass1/sexp->iform val lvars tail?)
       (acond
        [(pass1/find-symbol-in-lvars symbol lvars) ;; don't use find, it requires closure creation.
         ($lvar.set-count++! it)
         ($local-assign it iform)]
        [#t ($global-assign symbol iform)]))]))

(define (pass1/body->iform body lvars tail?)
  (let1 iforms ($map1-with-tail
                (lambda (b t?) (pass1/sexp->iform (pass1/expand b) lvars (and t? tail?))) body)
    (if (= 1 (length iforms))
        (car iforms)
        ($seq iforms tail?))))

  (define (dotpair->list p)
    (let loop ([p p])
      (cond
       [(and (not (pair? p)) (not (null? p)))
        (cons p '())]
       [(null? p)
        '()]
       [else
        (cons (car p) (loop (cdr p)))])))

;; Closure source info format
;; ((file . lineno) (proc args))
(define (pass1/lambda->iform name sexp lvars)
  (match sexp
    [('lambda vars . body)
     (receive (optional-arg? vars) (parse-lambda-vars vars)
       (let ([this-lvars    (imap (lambda (sym) ($lvar sym #f 0 0)) vars)]
             [vars-length  (length vars)])
       ($lambda (cons (source-info sexp) (cons name (dotpair->list (second sexp))))
                name
                (if optional-arg? (- vars-length 1) vars-length)
                (if optional-arg? 1 0)
                this-lvars
                ;; the inner lvar comes first.
                (pass1/body->iform body (append this-lvars lvars) #t)
                '()
                '())))]
    [else
     (error 'compiler "malformed lambda" sexp)]))

;; (define (pass1/lambda->iform name sexp lvars)
;;   (let* ([vars          (second sexp)]
;;          [body          (cddr sexp)])
;;          [parsed-vars   (parse-lambda-vars vars)]
;;          [optional-arg? (first parsed-vars)]
;;          [vars          (second parsed-vars)]
;;          [this-lvars    (imap (lambda (sym) ($lvar sym #f 0 0)) vars)]
;;          [vars-length  (length vars)])
;;     ($lambda (cons (source-info sexp) (cons name (dotpair->list (second sexp))))
;;              name
;;              (if optional-arg? (- vars-length 1) vars-length)
;;              (if optional-arg? 1 0)
;;              this-lvars
;;              ;; the inner lvar comes first.
;;              (pass1/body->iform body (append this-lvars lvars) #t)
;;              '()
;;              '())))

(define (pass1/and->iform sexp lvars tail?)
  (define (rec s)
    (match s
      [() ($const #t)]
      [(s)
       (pass1/sexp->iform (pass1/expand s) lvars tail?)]
      [(e . more)
       ($if (pass1/sexp->iform (pass1/expand e) lvars tail?)
            (rec more)
            ($it))]
      [else
       (error 'compiler "syntax-error: malformed and:" sexp)]))
  (rec (cdr sexp)))

(define (pass1/or->iform sexp lvars tail?)
  (define (rec s)
    (match s
      [() ($const #f)]
      [(s)
       (pass1/sexp->iform (pass1/expand s) lvars tail?)]
      [(e . more)
       ($if (pass1/sexp->iform (pass1/expand e) lvars tail?)
            ($it)
            (rec more))]
      [else
       (error 'compiler "syntax-error: malformed or:" sexp)]))
  (rec (cdr sexp)))

(define (pass1/cond->iform sexp lvars tail?)
  (define (process-clauses cls)
    (match cls
      [() ($undef)]
      ;; (else . exprs)
      [(((? (lambda (x) (eq? x 'else))) exprs ...) . rest)
       (unless (null? rest)
         (error 'compiler "syntax-error: 'else' clause followed by more clauses:" sexp))
       (pass1/body->iform exprs lvars tail?)]
      ;; (test => proc)
      [((test (? (lambda (x) (eq? x '=>))) proc) . rest)
       (let ([test (pass1/sexp->iform (pass1/expand test) lvars #f)]
             [tmp (make-lvar 'tmp)])
         ($lvar.set-init-val! tmp test)
         ($let 'let
               (list tmp)
               (list test)
               ($if ($local-ref tmp)
                    ($call (pass1/sexp->iform (pass1/expand proc) lvars tail?)
                           (list ($local-ref tmp))
                           tail?
                           #f)
                    (process-clauses rest)) tail? sexp))]
      [((test) . rest)                  ; (test)
       ($if (pass1/sexp->iform (pass1/expand test) lvars tail?)
            ($it)
            (process-clauses rest))]
      [((test exprs ...) . rest)          ; (test . exprs)
       ($if (pass1/sexp->iform test lvars #f)
            (pass1/body->iform exprs lvars tail?)
            (process-clauses rest))]
      (_ (error 'compiler "syntax-error: bad clause in cond:" sexp))))
  (match sexp
    ((_)
     (error "syntax-error: at least one clause is required for cond:" sexp))
    ((_ clause ...)
     (process-clauses clause))
    (else
     (error "syntax-error: malformed cond:" sexp))))

(define top-level-macros '())

;; N.B.
;; this procedure is called from freeproc.cpp
(define (pass1/macroexpand sexp)
  (let1 proc (first sexp)
    (acond
     [(and (symbol? proc) (assq proc top-level-macros))
;      (display top-level-macros (current-error-port))
      (pass1/expand (vm/apply (cdr it) (cdr sexp)))]
     [#t sexp])))

;; for checking performance with logging.
(define-macro (case-with-time val . clauses)
  `(case ,val
     ,@(map (lambda (clause)
             (match clause
               [(p . more)
                (let1 temp (gensym)
                `(,p (let1 ,temp (get-timeofday) (let1 v (begin ,@more) (dd "(log ") (dd (quote ,p)) (dd ,temp) (dd (get-timeofday)) (dd ")\n") v))))]))
           clauses)))

(define-macro (case-with-lambda val . clauses)
  `(case ,val
     ,@(map (lambda (clause)
             (match clause
               [('else . more)
                `(else ((lambda (else) (set! else 3) ,@more) 4))]
               [(p . more)
                (if (and (pair? p) (symbol? (car p)))
                    (let1 sym (string->symbol (string-append "profile-" (symbol->string (car p))))
                      `(,p ((lambda (,sym) (set! ,sym 3) ,@more) 4)))
                    `(,p ((lambda (anonymous) (set! anonymous 3) ,@more) 4)))]))
           clauses)))

;; short cut macros
(define-macro (pass1/s->i sexp)
  `(pass1/sexp->iform (pass1/expand ,sexp) lvars tail?))

(define-macro (pass1/s->i-non-tail sexp)
  `(pass1/sexp->iform (pass1/expand ,sexp) lvars #f))

(define-macro (pass1/map-s->i sexp)
  `(imap (lambda (s) (pass1/s->i s)) ,sexp))

(define-macro (pass1/map-s->i-non-tail sexp)
  `(imap (lambda (s) (pass1/s->i-non-tail s)) ,sexp))

(define (pass1/call proc args lvars tail?)
  (acond
   [(and (symbol? proc)
         (assq proc top-level-macros))
    (pass1/s->i (vm/apply (cdr it) args))]
   [#t
    ($call (pass1/s->i proc)
           (pass1/map-s->i-non-tail args)
           tail?
           #f)]))

(define (pass1/define sexp lvars tail?)
  (match sexp
    [('define name ('lambda . more))
     (let1 closure  (make-list-with-src-slot (cons 'lambda more))
       (set-source-info! closure (source-info (third sexp)))
       ($define name (pass1/lambda->iform name closure lvars)))]
    [else
     ($define (second sexp) (pass1/s->i (third sexp)))]))

(define (pass1/receive sexp lvars tail?)
  (match sexp
    [('receive vars vals . body)
     (receive (vars reqargs optarg) (parse-lambda-args vars)
       (let1 this-lvars (imap (lambda (sym) ($lvar sym #f 0 0)) vars)
         ($receive
          this-lvars
          reqargs
          optarg
          (pass1/s->i-non-tail vals)
          ;; the inner lvar comes first.
          (pass1/body->iform (pass1/expand body) (append this-lvars lvars) tail?)
          tail?)))]
    [else
     (syntax-error "malformed receive")]))


(define (pass1/let vars vals body source-info lvars tail?)
  (let* ([inits      (pass1/map-s->i vals)]
         [this-lvars (map (lambda (sym init) ($lvar sym init 0 0)) vars inits)])
    ($let 'let
          this-lvars
          inits
          ;; the inner lvar comes first.
          (pass1/body->iform (pass1/expand body)  (append this-lvars lvars) tail?)
          tail?
          source-info
          )))

(define (make-compile-error who message . irritants)
  (list who message irritants))

(define (pass1/letrec vars vals body source-info lvars tail?)
  (let* ([this-lvars (imap (lambda (sym) ($lvar sym ($undef) 0 0)) vars)]
         [inits      (imap (lambda (x) (pass1/sexp->iform x (append this-lvars lvars) tail?)) vals)])
    (for-each (lambda (lvar init)
                ;; this name, used for error message.(etc. wrong number arguments)
                ;; named let
                (when (tag? init $LAMBDA)
                  ;; set name to src-info
                  (when (and ($lambda.src init) (pair? ($lambda.src init)) (pair? (cdr ($lambda.src init))))
                    (set-car! (cdr ($lambda.src init)) ($lvar.sym lvar)))
                  ($lambda.set-name! init ($lvar.sym lvar)))
                ($lvar.set-init-val! lvar init))
              this-lvars inits)
    (let1 found-error (find (lambda (init)
                              (and (tag? init $LOCAL-REF) (memq ($local-ref.lvar init) this-lvars))) inits)
    ($let 'rec
          this-lvars
          inits
          ;; the inner lvar comes first.
          (pass1/body->iform (pass1/expand body) (append this-lvars lvars) tail?)
          tail?
          source-info
          (if found-error (make-compile-error
                           'letrec
                           "reference to uninitialized variable on letrec"
                           (ungensym ($lvar.sym ($local-ref.lvar found-error))))
              #f)
          ))))

(define (pass1/if test then more lvars tail?)
  ($if
   (pass1/sexp->iform (pass1/expand test) lvars #f) ;; N.B. test clause is NOT in tail-context.
   (pass1/s->i then)
   (if (null? more)
       ($undef)
       (pass1/s->i (car more)))))

(define (pass1/define-macro sexp lvars tail?)
  (if (pair? (second sexp))
      ; we can't use hash-table here, because hash-table can't be written with (write).
      ; So we use acons instead.
      (set! top-level-macros (alist-cons (caadr sexp)  (compile-partial `(lambda ,(cdadr sexp) ,(third sexp))) top-level-macros))
      (set! top-level-macros (alist-cons (second sexp) (compile-partial (third sexp)) top-level-macros)))
  ($undef))

(define (pass1/asm-numcmp tag operator args lvars)
  (let1 len (length args)
    (cond [(> 2 len) (error operator " got too few argument")]
          [(= 2 len)
           ($asm tag
                 (list (pass1/s->i-non-tail (first args))
                       (pass1/s->i-non-tail (second args))))]
          [else
           (pass1/s->i-non-tail (conditions->if (apply-each-pair operator args)))])))

(define (pass1/asm-1-arg tag arg1 lvars)
  ($asm tag (list (pass1/s->i-non-tail arg1))))

(define (pass1/asm-2-arg tag arg1 arg2 lvars)
  ($asm tag (list (pass1/s->i-non-tail arg1) (pass1/s->i-non-tail arg2))))

(define (pass1/asm-3-arg tag arg1 arg2 arg3 lvars)
  ($asm tag (list (pass1/s->i-non-tail arg1)
                  (pass1/s->i-non-tail arg2)
                  (pass1/s->i-non-tail arg3))))

(define (pass1/asm-1-arg-optional tag args lvars)
  (let1 arg1 (if (null? args) '() (car args))
    ($asm tag (list (pass1/s->i-non-tail arg1)))))

(define (pass1/asm-2-arg-optional tag arg1 rest lvars)
  (let1 arg2 (if (null? rest) '() (car rest))
    ($asm tag (list (pass1/s->i-non-tail arg1) (pass1/s->i-non-tail arg2)))))

(define (sub->add form)
  (let* ([args (cdr form)]
         [len (length (cdr form))])
    (cond
     ;; this is error so return
     [(zero? len) form]
     [(= 1 len)
      `(+ ,(* -1 (car args)))]
     [else
      `(+ ,(car args) ,@(imap - (cdr args)))])))

(define (pass1/asm-n-args tag operator args lvars)
  (let1 len (length args)
    (cond
     [(zero? len)
      (case operator
        [(+)
         (pass1/s->i-non-tail 0)]
        [(*)
         (pass1/s->i-non-tail 1)]
        [(append)
         (pass1/s->i-non-tail '())]
        [else
         (error operator " got too few argment")])]
     [(= 1 len)
      (case operator
        [(-)
         (pass1/s->i-non-tail `(* -1 ,(car args)))]
        [(/)
         (pass1/s->i-non-tail `(/ 1 ,(car args)))]
        [else
         (pass1/s->i-non-tail (car args))])]
     [(= 2 len)
      ($asm tag (list (pass1/s->i-non-tail (first args)) (pass1/s->i-non-tail (second args))))]
     [else
      (let1 args-iform (pass1/map-s->i-non-tail args)
        (fold (lambda (x y) ($asm tag (list y x))) (car args-iform) (cdr args-iform)))])))

(define (pass1/sexp->iform sexp lvars tail?)
  (cond
   [(pair? sexp)
    (case (car sexp)
      ;;---------------------------- lambda ------------------------------------
      [(lambda)
       (pass1/lambda->iform 'lambda sexp lvars)]
      ;;---------------------------- cons --------------------------------------
      [(cons)
       (pass1/asm-2-arg 'CONS (second sexp) (third sexp) lvars)]
      ;;---------------------------- and ---------------------------------------
      [(and)
       (pass1/and->iform sexp lvars tail?)]
      ;;---------------------------- and ---------------------------------------
      [(or)
       (pass1/or->iform sexp lvars tail?)]
      ;;---------------------------- begin -------------------------------------
      [(begin)
       (pass1/body->iform (pass1/expand (cdr sexp)) lvars tail?)]
      ;;---------------------------- cond --------------------------------------
      [(cond)
       (pass1/cond->iform (pass1/expand sexp) lvars tail?)]
      ;;---------------------------- values ------------------------------------
      [(values)
       ($asm 'VALUES (pass1/map-s->i-non-tail (cdr sexp)))]
      ;;---------------------------- vector ------------------------------------
      [(vector)
       ($asm 'VECTOR (pass1/map-s->i-non-tail (cdr sexp)))]
      ;;---------------------------- define ------------------------------------
      [(define)
       (pass1/define sexp lvars tail?)]
      ;;---------------------------- define-macro ------------------------------
      [(define-macro)
       (pass1/define-macro sexp lvars tail?)]
      ;;---------------------------- receive -----------------------------------
      [(receive)
       (pass1/receive sexp lvars tail?)]
      ;;---------------------------- let ---------------------------------------
      [(let)
       (pass1/let (imap car (second sexp))  ; vars
                  (imap cadr (second sexp)) ; vals
                  (cddr sexp)                ; body
                  (source-info sexp)         ; source-info
                  lvars tail?)]
      ;;---------------------------- letrec ------------------------------------
      [(letrec)
       (pass1/letrec (imap car (second sexp))  ; vars
                     (imap cadr (second sexp)) ; vals
                     (cddr sexp)                ; body
                     (source-info sexp)         ; source-info
                     lvars tail?)]
      ;;---------------------------- letrec ------------------------------------
      [(letrec*)
       (pass1/letrec (imap car (second sexp))  ; vars
                     (imap cadr (second sexp)) ; vals
                     (cddr sexp)                ; body
                     (source-info sexp)         ; source-info
                     lvars tail?)]
      ;;---------------------------- set! --------------------------------------
      [(set!)
       (pass1/assign (second sexp)                ;; symbol
                     (pass1/expand (third sexp))  ;; value
                     lvars tail?)]
      ;;---------------------------- if ----------------------------------------
      [(if)
       (pass1/if (second sexp) ;; test
                 (third sexp)  ;; then
                 (cdddr sexp)  ;; else if exists.
                 lvars tail?)]
      [(undef)
       ($undef)]
      ;;---------------------------- call/cc -----------------------------------
      [(%call/cc)
       ($call-cc (pass1/s->i (second sexp)) tail?)]
      [(%call-with-current-continuation)
       ($call-cc (pass1/s->i (second sexp)) tail?)]
      ;;---------------------------- quote -------------------------------------
      [(quote)
       ($const (second sexp))]
      [(append)           (pass1/asm-n-args         'APPEND2      'append (cdr sexp) lvars)]
      [(+)                (pass1/asm-n-args         'NUMBER_ADD   '+  (cdr sexp)    lvars)]
      [(-)
       (if (for-all number? (cdr sexp))
           (pass1/asm-n-args 'NUMBER_ADD   '+  (cdr (sub->add sexp))    lvars)
           (pass1/asm-n-args 'NUMBER_SUB   '-  (cdr sexp)    lvars))]
      [(*)                (pass1/asm-n-args         'NUMBER_MUL   '*  (cdr sexp)    lvars)]
      [(/)                (pass1/asm-n-args         'NUMBER_DIV   '/  (cdr sexp)    lvars)]
      [(=)                (pass1/asm-numcmp         'NUMBER_EQUAL '=  (cdr sexp)    lvars)]
      [(>=)               (pass1/asm-numcmp         'NUMBER_GE    '>= (cdr sexp)    lvars)]
      [(>)                (pass1/asm-numcmp         'NUMBER_GT    '>  (cdr sexp)    lvars)]
      [(<)                (pass1/asm-numcmp         'NUMBER_LT    '<  (cdr sexp)    lvars)]
      [(<=)               (pass1/asm-numcmp         'NUMBER_LE    '<= (cdr sexp)    lvars)]
      [(vector?)          (pass1/asm-1-arg          'VECTOR_P      (second sexp)    lvars)]
      [(vector-length)    (pass1/asm-1-arg          'VECTOR_LENGTH (second sexp)    lvars)]
      [(vector-set!)      (pass1/asm-3-arg          'VECTOR_SET    (second sexp)    (third sexp) (fourth sexp) lvars)]
      [(vector-ref)       (pass1/asm-2-arg          'VECTOR_REF    (second sexp)    (third sexp) lvars)]
      [(simple-struct-ref) (pass1/asm-2-arg          'SIMPLE_STRUCT_REF    (second sexp)    (third sexp) lvars)]
      [(make-vector)      (pass1/asm-2-arg-optional 'MAKE_VECTOR   (second sexp)    (cddr sexp) lvars)]
      [(car)              (pass1/asm-1-arg          'CAR           (second sexp)    lvars)]
      [(cdr)              (pass1/asm-1-arg          'CDR           (second sexp)    lvars)]
      [(caar)             (pass1/asm-1-arg          'CAAR          (second sexp)    lvars)]
      [(cadr)             (pass1/asm-1-arg          'CADR          (second sexp)    lvars)]
      [(cdar)             (pass1/asm-1-arg          'CDAR          (second sexp)    lvars)]
      [(cddr)             (pass1/asm-1-arg          'CDDR          (second sexp)    lvars)]
      [(set-car!)         (pass1/asm-2-arg          'SET_CAR       (second sexp)    (third sexp) lvars)]
      [(set-cdr!)         (pass1/asm-2-arg          'SET_CDR       (second sexp)    (third sexp) lvars)]
      [(eq?)              (pass1/asm-2-arg          'EQ            (second sexp)    (third sexp) lvars)]
      [(eqv?)             (pass1/asm-2-arg          'EQV           (second sexp)    (third sexp) lvars)]
      [(equal?)           (pass1/asm-2-arg          'EQUAL         (second sexp)    (third sexp) lvars)]
      [(not)              (pass1/asm-1-arg          'NOT           (second sexp)    lvars)]
      [(null?)            (pass1/asm-1-arg          'NULL_P        (second sexp)    lvars)]
      [(pair?)            (pass1/asm-1-arg          'PAIR_P        (second sexp)    lvars)]
      [(symbol?)          (pass1/asm-1-arg          'SYMBOL_P      (second sexp)    lvars)]
      [(read)             (pass1/asm-1-arg-optional 'READ          (cdr sexp)       lvars)]
      [(read-char)        (pass1/asm-1-arg-optional 'READ_CHAR     (cdr sexp)       lvars)]
      ;;---------------------------- call or macro------------------------------
      [else
       (pass1/call (car sexp) ; proc
                   (cdr sexp) ; args
                   lvars tail?)])]
   [(symbol? sexp)
    (pass1/refer->iform sexp lvars)]
   [else ($const sexp)]))



;;--------------------------------------------------------------------
;;
;;  Pass1: Pretty print for Iform
;;
;;    based on Gauche/src/compiler.scm by Shiro Kawai start.
;;
(define (mosh-insn->gosh-insn insn)
  (let1 val (assq insn '((NUMBER_SUB . NUMSUB2)
                         (VECTOR_LENGTH . VEC-LEN)
                         (VECTOR_REF . VEC-REF)
                         (VECTOR_SET . VEC-SET)
                         (NUMBER_ADD . NUMADD2)
                         (NUMBER_SUB . NUMSUB2)
                         (NUMBER_LT . NUMLT2)
                         (NUMBER_LE . NUMLE2)
                         (NUMBER_GT . NUMGT2)
                         (NUMBER_GE . NUMGE2)

                  ))
    (if val (list (cdr val)) insn)))

(define (pp-iform iform)
  (define labels '()) ;; alist of label node and count

  (define (indent count)
    (let loop ([i 0])
      (if (= i count)
          '()
          (begin
            (display #\space (current-error-port))
          (loop (+ i 1))))))
  (define (nl ind)
    (newline (current-error-port)) (indent ind))
  (define (lvar->string lvar)
    (format "~a[~a;~a]"
            ($lvar.sym lvar)
            ($lvar.ref-count lvar)
            ($lvar.set-count lvar)))
  (define (rec ind iform)
    (cond
     [(tag? iform $CONST)
      (format (current-error-port) "($const ~s)" ($const.val iform))]
     [(tag? iform $LIST)
      (format (current-error-port) "($list)")]
     [(tag? iform $UNDEF)
      (display "($const #<undef>)" (current-error-port))]
     [(tag? iform $LAMBDA)
      (format (current-error-port) "($lambda[~a;~a] ~a" ($lambda.name iform)
              (length ($lambda.calls iform))
              (map lvar->string ($lambda.lvars iform)))
      (nl (+ ind 2))
      (rec (+ ind 2) ($lambda.body iform)) (display ")" (current-error-port))]
     [(tag? iform $SEQ)
      (format (current-error-port) "($seq")
      (for-each (lambda (node) (nl (+ ind 2)) (rec (+ ind 2) node))
                ($seq.body iform))
      (display ")" (current-error-port))]
     [(tag? iform $LOCAL-REF)
      (format (current-error-port) "($lref ~a)" (lvar->string ($local-ref.lvar iform)))]
     [(tag? iform $GLOBAL-REF)
      (format (current-error-port) "($gref ~a)" ;; ($global-ref.libname iform)
              ($global-ref.sym iform))]
     [(tag? iform $LOCAL-ASSIGN)
      (format (current-error-port) "($lset ~a"  (lvar->string ($local-assign.lvar iform)))
      (nl (+ ind 2))
      (rec (+ ind 2) ($local-assign.val iform)) (display ")" (current-error-port))]
     [(tag? iform $GLOBAL-ASSIGN)
      (format (current-error-port) "($gset ~a)";;  ($global-assign.sym iform)
              ($global-assign.val iform))]
     [(tag? iform $LET)
      (let* ((hdr  (format "($let~a (" (case ($let.type iform)
                                         ((let) "") ((rec) "rec"))))
             (xind (+ ind (string-length hdr))))
        (display hdr (current-error-port))
        (for-each (lambda (var init)
                    (let1 z (format "(~a " (lvar->string var))
                      (display z (current-error-port))
                      (rec (+ xind  (string-length z)) init)
                      (display ")" (current-error-port))
                      (nl xind)))
                  ($let.lvars iform) ($let.inits iform))
        (display ")" (current-error-port)) (nl (+ ind 2))
        (rec (+ ind 2) ($let.body iform)) (display ")" (current-error-port)))]
     [(tag? iform $IF)
      (display "($if " (current-error-port))
      (rec (+ ind 5) ($if.test iform)) (nl (+ ind 2))
      (rec (+ ind 2) ($if.then iform)) (nl (+ ind 2))
      (rec (+ ind 2) ($if.else iform)) (display ")" (current-error-port))]
     ((tag? iform $RECEIVE)
      (format #t "($receive ~a" (map lvar->string ($receive.lvars iform)))
      (nl (+ ind 4))
      (rec (+ ind 4) ($receive.vals iform)) (nl (+ ind 2))
      (rec (+ ind 2) ($receive.body iform)) (display ")"))
     [(tag? iform $LABEL)
      (cond ((assq iform labels)
             => (lambda (p) (format (current-error-port) "label#~a" (cdr p))))
            (else
             (let1 num (length labels)
               (push! labels (cons iform num))
               (format (current-error-port) "($label #~a" num)
               (nl (+ ind 2))
               (rec (+ ind 2) ($label.body iform)) (display ")" (current-error-port)))))]
     [(tag? iform $ASM)
      (let1 insn (mosh-insn->gosh-insn ($asm.insn iform))
        (format (current-error-port) "($asm ~a" insn))
      (for-each (lambda (node) (nl (+ ind 2)) (rec (+ ind 2) node))
                ($asm.args iform))
      (display ")" (current-error-port))]
     [(tag? iform $DEFINE)
      (format (current-error-port) "($define () ~a" ;; ($define.libname iform)
              ($define.sym iform))
      (nl (+ ind 2))
      (rec (+ ind 2) ($define.val iform)) (display ")" (current-error-port))]
     [(tag? iform $CALL-CC)
      (display "($CALL-CC " (current-error-port))
      (rec 0 ($call-cc.proc iform))
      (display ")" (current-error-port))]
     [(tag? iform $LABEL)
      (display "($label " (current-error-port))
      (rec 0 ($label.body iform))
      (display ")" (current-error-port))]
     [(tag? iform $IT)
      (display "($it)" (current-error-port))]
     [(tag? iform $RECEIVE)
      (format (current-error-port) "($receive ~a" (map lvar->string ($receive.lvars iform)))
      (nl (+ ind 4))
      (rec (+ ind 4) ($receive.vals iform)) (nl (+ ind 2))
      (rec (+ ind 2) ($receive.body iform)) (display ")" (current-error-port))]
     [(tag? iform $CALL)
      (let1 pre
          (cond (($call.type iform) => (lambda (x) (format "($call[~a] " x)))
                (else "($call "))
        (format (current-error-port) pre)
;        (format (current-error-port) "[~a]" ($call.tail? iform))
        (rec (+ ind (string-length pre)) ($call.proc iform))
        (for-each (lambda (node) (nl (+ ind 2)) (rec (+ ind 2) node))
                  ($call.args iform))
        (display ")" (current-error-port)))]
     (else
      (error "pp-iform: unknown tag:" (tag iform)))
     ))
  (rec 0 iform)
  (newline (current-error-port)))

;;  based on Gauche/src/compiler.scm by Shiro Kawai end.


;;--------------------------------------------------------------------
;;
;;  Pass2
;;
;;
;; =============================================================================

(define SMALL_LAMBDA_SIZE 12)

(define pass2/dispatch-table (make-vector $INSN-NUM))

;; (define (pass2/$let iform closures)
;;   (cond
;;    [($let.error iform) iform]
;;    [else
;;     ($let.set-body! iform (pass2/optimize ($let.body iform) closures))
;;     ($let.set-inits! iform (imap (lambda (i) (pass2/optimize i closures)) ($let.inits iform)))
;;     (let1 o (pass2/eliminate-let iform)
;;       (if (eq? o iform)
;;           o
;;           (pass2/optimize o closures)))]))

;; Used with logging.
;; This macro will be work on Gauche compiler.
(define-macro (define-pass2/tracable init . body)
  (if #f
      (match init
        [(name . var)
         (let1 ret (gensym)
           `(define ,init
              (format (current-error-port) "[~a] IN  =>\n" ',name)
              (pp-iform ,(car var))
              (let1 ,ret (begin ,@body)
                (format (current-error-port) "[~a] OUT =>\n" ',name)
                (pp-iform ,(car var))
                (display "\n" (current-error-port))
                ,ret)))]
        [else
         (error 'define-pass2/tracable "invalid syntax")])
      `(define ,init ,@body)))

(define-pass2/tracable (pass2/$let iform closures)
  (cond
   [($let.error iform ) iform]
   [else
    (let ([lvars ($let.lvars iform)]
          [inits (imap (lambda (init) (pass2/optimize init closures)) ($let.inits iform))]
          [obody (pass2/optimize ($let.body iform) closures)])
      (for-each pass2/optimize-closure lvars inits)
      (receive (new-lvars new-inits removed-inits) (pass2/remove-vars lvars inits)
        (cond
         [(null? new-lvars)
          (if (null? removed-inits)
              obody
              ($seq (append! removed-inits (list obody)) ($let.tail? iform)))]
         [else
          ($let.set-lvars! iform new-lvars)
          ($let.set-inits! iform new-inits)
          ($let.set-body! iform obody)
          (unless (null? removed-inits)
            (if (tag? obody $SEQ)
                (begin
                  ($seq.set-body! obody
                                  (append! removed-inits
                                           ($seq.body obody))))
                (begin
                  ($let.set-body! iform
                                  ($seq (append removed-inits
                                                (list obody))
                                        ($let.tail? iform))))))
          iform])))]))

(define-pass2/tracable (pass2/$receive iform closures)
  ($receive.set-vals! iform (pass2/optimize ($receive.vals iform) closures))
  ($receive.set-body! iform (pass2/optimize ($receive.body iform) closures))
  iform)

(define-pass2/tracable (pass2/$local-ref iform closures)
  (let1 lvar ($local-ref.lvar iform)
    (if (zero? ($lvar.set-count lvar))
        (let1 init-val ($lvar.init-val lvar)
          (cond
           [(not (vector? init-val)) iform]
           [(tag? init-val $CONST)
            ($lvar.ref-count--! lvar)
            (set-tag! iform $CONST)
            ($const.set-val! iform ($const.val init-val))
            iform]
           [(and (tag? init-val $LOCAL-REF)
                 (zero? ($lvar.set-count ($local-ref.lvar init-val))))
               (when (eq? iform init-val)
                 (error "mosh" "circular reference appeared in letrec bindings:"
                        (lvar.sym lvar)))
               ($lvar.ref-count--! lvar)
               ($lvar.ref-count++! ($local-ref.lvar init-val))
               ($local-ref.copy iform init-val)
               (pass2/$local-ref iform closures)]
           [else iform]))
    iform)))

(define-pass2/tracable (pass2/$seq iform closures)
  ($seq.set-body! iform (imap (lambda (x) (pass2/optimize x closures)) ($seq.body iform)))
  iform)


(define (pass2/const-inliner iform)
  (let ([insn ($asm.insn iform)]
        [args ($asm.args iform)])
    (case insn
      [(NUMBER_ADD)
       (when (and (tag? (first args) $CONST) (number? ($const.val (first args)))
                  (tag? (second args) $CONST) (number? ($const.val (second args))))
         (vector-set! iform 0 $CONST)
         ($const.set-val! iform (+ ($const.val (first args)) ($const.val (second args)))))]
      [(NUMBER_MUL)
       (when (and (tag? (first args) $CONST) (number? ($const.val (first args)))
                  (tag? (second args) $CONST) (number? ($const.val (second args))))
         (vector-set! iform 0 $CONST)
         ($const.set-val! iform (* ($const.val (first args)) ($const.val (second args)))))]
      [(NUMBER_SUB)
       (when (and (tag? (first args) $CONST) (number? ($const.val (first args)))
                  (tag? (second args) $CONST) (number? ($const.val (second args))))
         (vector-set! iform 0 $CONST)
         ($const.set-val! iform (- ($const.val (first args)) ($const.val (second args)))))]
      [else #f])))

(define-pass2/tracable (pass2/$asm iform closures)
  ($asm.set-args! iform (imap (lambda (x) (pass2/optimize x closures)) ($asm.args iform)))
  (pass2/const-inliner iform)
  iform)

(define-pass2/tracable (pass2/$lambda iform closures)
  ($lambda.set-body! iform
                     (pass2/optimize ($lambda.body iform) (cons iform closures)))
  iform)

(define-pass2/tracable (pass2/$if iform closures)
  (let1 if-test ($if.test iform)
    (cond
     ;; (if const else then) => else or then.
     [(tag? if-test $CONST)
      (if ($const.val if-test)
          ($seq (list if-test (pass2/optimize ($if.then iform) closures)) #f)
          ($seq (list if-test (pass2/optimize ($if.else iform) closures)) #f))]
     [else
      (let ([test-c (pass2/optimize ($if.test iform) closures)]
            [then-c (pass2/optimize ($if.then iform) closures)]
            [else-c (pass2/optimize ($if.else iform) closures)])
        ($if test-c then-c else-c))])))

;; ** This version of pass2/$if works, but very slow. **
;;   This genrates many labels and so it makes pass3/find-free/sets slower.
;;   We need to distinguish source label and destination label.
;;   And pass3/find-free must visit only destination label .
;; (define-pass2/tracable (pass2/$if iform closures)
;;   (let1 test (pass2/optimize ($if.test iform) closures)
;;     (or (and
;;          (tag? test $IF)
;;          (let ([test-then ($if.then test)]
;;                [test-else ($if.else test)])
;;            (cond ((tag? test-then $IT)
;;                   (receive (l0 l1)
;;                       (pass2/label-or-dup
;;                        (pass2/optimize ($if.then iform) closures))
;;                     (pass2/update-if iform ($if.test test)
;;                                      l0
;;                                      (pass2/optimize ($if
;;                                                       test-else
;;                                                       l1
;;                                                       ($if.else iform))
;;                                                 closures))))
;;                  ((or (tag? test-else $IT)
;;                       (and (tag? test-else $CONST)
;;                            (not ($const.val test-else))))
;;                   (receive (l0 l1)
;;                       (pass2/label-or-dup
;;                        (pass2/optimize ($if.else iform) closures))
;;                     (pass2/update-if iform ($if.test test)
;;                                      (pass2/optimize ($if
;;                                                      test-then
;;                                                      ($if.then iform)
;;                                                      l0)
;;                                                 closures)
;;                                      l1)))
;;                  ((and (tag? test-then $CONST)
;;                        (not ($const.val test-then)))
;;                   (receive (l0 l1)
;;                       (pass2/label-or-dup
;;                        (pass2/optimize ($if.else iform) closures))
;;                     (pass2/update-if iform ($if.test test)
;;                                      (if (tag? l0 $IT)
;;                                        ($const #f)
;;                                        l0)
;;                                      (pass2/optimize ($if
;;                                                      test-else
;;                                                      ($if.then iform)
;;                                                      l1)
;;                                                 closures))))
;;                  (else #f))))
;;         ;; default case
;;         (pass2/update-if iform
;;                          test
;;                          (pass2/optimize ($if.then iform) closures)
;;                          (pass2/optimize ($if.else iform) closures)))))

(define (pass2/label-or-dup iform)
  (if (memv (tag iform) `(,$LOCAL-REF ,$CONST ,$IT))
    (values iform (iform-copy iform '()))
    (let1 lab ($label iform)
      (values lab lab))))

(define (pass2/update-if iform new-test new-then new-else)
  (if (eq? new-then new-else)
    ($seq (list new-test new-then))
    (begin ($if.set-test! iform new-test)
           ($if.set-then! iform new-then)
           ($if.set-else! iform new-else)
           iform)))


(define-pass2/tracable (pass2/$local-assign iform closures)
  ($local-assign.set-val! iform (pass2/optimize ($local-assign.val iform) closures))
  iform)

(define-pass2/tracable (pass2/$global-assign iform closures)
  ($global-assign.set-val! iform (pass2/optimize ($global-assign.val iform) closures))
  iform)

(define-pass2/tracable (pass2/$define iform closures)
  ($define.set-val! iform (pass2/optimize ($define.val iform) closures))
  iform)

(define-pass2/tracable (pass2/$call iform closures)
  (cond
   [($call.type iform) iform]
   [else
    (let ([proc ($call.proc iform)]
          [args ($call.args iform)])
      ;; scan OP first to give an opportunity of variable renaming
      ($call.set-proc! iform (pass2/optimize ($call.proc iform) closures))
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
                    )
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
                      ($call.set-args! iform (imap (lambda (x) (pass2/optimize x closures)) args))
                      iform)]))]
            [else
             ($call.set-args! iform (imap (lambda (x) (pass2/optimize x closures)) args))
             iform]))]))
;  (pass2/collect-call iform closures))

(define (pass2/empty iform closures)
  iform)

(define (pass2/register insn proc)
  (vector-set! pass2/dispatch-table insn proc))

(pass2/register $CONST         pass2/empty)
(pass2/register $LET           pass2/$let)
(pass2/register $SEQ           pass2/$seq)
(pass2/register $LAMBDA        pass2/$lambda)
(pass2/register $LOCAL-REF     pass2/$local-ref)
(pass2/register $LOCAL-ASSIGN  pass2/$local-assign)
(pass2/register $GLOBAL-REF    pass2/empty)
(pass2/register $GLOBAL-ASSIGN pass2/$global-assign)
(pass2/register $UNDEF         pass2/empty)
(pass2/register $IF            pass2/$if)
(pass2/register $ASM           pass2/$asm)
(pass2/register $DEFINE        pass2/$define)
(pass2/register $CALL          pass2/$call)
(pass2/register $CALL-CC       pass2/empty)
(pass2/register $LABEL         pass2/empty)
(pass2/register $LIST          pass2/empty)
(pass2/register $IT            pass2/empty)
(pass2/register $RECEIVE       pass2/$receive)


(define (pass2/optimize iform closures)
  ((vector-ref pass2/dispatch-table (vector-ref iform 0)) iform closures))

;; (define (pass2/optimize-local-ref iform)
;;   (let* ([lvar     ($local-ref.lvar iform)]
;;          [init-val ($lvar.init-val lvar)]) ;; init-val = #f if lvar belongs to $LAMBDA.
;;     ;; if lvar is never set! and initial value is constant.
;;     (cond [(and init-val (zero? ($lvar.set-count lvar)) (tag? init-val $CONST))
;;            ;; We re-use the vector.
;;            (set-tag! iform $CONST)
;;            ($lvar.ref-count--! lvar)
;;            ($const.set-val! iform ($const.val init-val))]
;;           [(and init-val (tag? init-val $LOCAL-REF)
;;                 (zero? ($lvar.set-count ($local-ref.lvar init-val))))
;;            ($lvar.ref-count--! lvar)
;;            ($lvar.ref-count++! ($local-ref.lvar init-val))
;;            ($local-ref.copy iform init-val)
;;            (pass2/optimize-local-ref iform)]
;;           [else iform])))


;; (define (pass2/eliminate-let iform)
;;   (let ([vars ($let.lvars  iform)]
;;         [inits ($let.inits iform)]
;;         [body ($let.body iform)])
;;     (for-each pass2/optimize-closure vars inits)
;;     (let* ([v (pass2/remove-vars vars inits)]
;;            [new-vars      (vector-ref v 0)]
;;            [new-inits     (vector-ref v 1)]
;;            [removed-inits (vector-ref v 2)])

;;       (cond ((null? new-vars)
;;              (if (null? removed-inits)
;;                  body
;;                  ($seq (append removed-inits (list body)) ($let.tail? iform))))
;;             (else
;;              ($let.set-lvars! iform new-vars)
;;              ($let.set-inits! iform new-inits)
;;              ($let.set-body! iform body)
;;              (unless (null? removed-inits)
;;                (if (tag? body $SEQ)
;;                    ($seq.set-body! body
;;                                    (append removed-inits
;;                                             ($seq.body body)))
;;                    ($let.set-body! iform
;;                                    ($seq (append removed-inits
;;                                                  (list body))
;;                                          ($let.tail? iform)))))
;;              iform)))))

(define (iform-copy-zip-lvs orig-lvars lv-alist)
  (let1 new-lvars (imap (lambda (lv)
                          (let1 new-lvar (make-lvar ($lvar.sym lv))
                            ($lvar.set-ref-count! new-lvar ($lvar.ref-count lv))
                            new-lvar))
                            orig-lvars)
    (cons new-lvars
          (foldr2 alist-cons lv-alist orig-lvars new-lvars))))

(define (iform-copy-lvar lvar lv-alist)
  ;; NB: using extra lambda after => is a kludge for the current optimizer
  ;; to work better.  Should be gone later.
  (cond ((assq lvar lv-alist) => (lambda (p) (cdr p)))
        (else lvar)))

(define (iform-copy iform lv-alist)
  (let1 t (tag iform)
    (cond
     [(= $DEFINE t)
      ($define
                ($define.sym iform)
                (iform-copy ($define.val iform) lv-alist))]
     [(= $LOCAL-REF t)
      ($local-ref (iform-copy-lvar ($local-ref.lvar iform) lv-alist))]
     [(= $LOCAL-ASSIGN t)
      ($local-assign (iform-copy-lvar ($local-assign.lvar iform) lv-alist)
                     (iform-copy ($local-assign.val iform) lv-alist))]
     [(= $GLOBAL-REF t)
      ($global-ref ($global-ref.sym iform))]
     [(= $GLOBAL-ASSIGN t)
      ($global-assign ($global-assign.sym iform) (iform-copy ($global-assign.val iform) lv-alist))]
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
                (imap (lambda (x) (iform-copy x al)) ($let.inits iform)))
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
      ($seq (imap (lambda (x) (iform-copy x lv-alist)) ($seq.body iform)) ($seq.tail? iform))]
     [(= $CALL t)
      ($call (iform-copy ($call.proc iform) lv-alist)
             (imap (lambda (x) (iform-copy x lv-alist)) ($call.args iform))
             #f
             ($call.type iform))]
     [(= $ASM t)
      ($asm ($asm.insn iform)
            (imap (lambda (x) (iform-copy x lv-alist)) ($asm.args iform)))]
     [else iform])))

;; based on Gauche/src/compiler.scm by Shiro Kawai start.
(define (pass2/optimize-closure lvar lambda-node)
  (when (and (zero? ($lvar.set-count lvar))
             (> ($lvar.ref-count lvar) 0)
             (tag? lambda-node $LAMBDA))
    (or (and (= ($lvar.ref-count lvar) (length ($lambda.calls lambda-node)))
             (receive (locals recs tail-recs) (pass2/classify-calls ($lambda.calls lambda-node) lambda-node)
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
       [(= $IT t)            cnt]
       [(= $RECEIVE t)       (sum-items (+ cnt 1) ($receive.vals iform) ($receive.body iform))]
       [else
        (error 'compiler (format "[internal error] iform-count-size-upto: unknown iform tag:~a" (tag iform)))]
       )))
  (define (rec-list iform-list cnt)
    (cond ((null? iform-list) cnt)
          ((>= cnt limit) limit)
          (else
           (rec-list (cdr iform-list)
                     (rec (car iform-list) cnt)))))
  (rec iform 0))

;; Adjust argument list according to reqargs and optarg count.  Used in procedure inlining and local call optimization.
(define (adjust-arglist reqargs optarg iargs name src)
  (unless (argcount-ok? iargs reqargs (> optarg 0))
    (if (and src (pair? src) (pair? (car src)))
        (errorf
         "wrong number of arguments: ~a requires ~a, but got ~a at ~a ~a:~a"
         (ungensym name)
         reqargs
         (length iargs)
         (cdr src)
         (caar src)
         (cadar src))
        (errorf
         "wrong number of arguments: ~a requires ~a, but got ~a"
         (ungensym name)
         reqargs
         (length iargs)
         )))
  #;(unless (argcount-ok? iargs reqargs (> optarg 0))
    (errorf "wrong number of arguments: letrec/named let requires ~a, but got ~a at ~a"
            name reqargs (length iargs) (source-info iargs)))
  (if (zero? optarg)
      iargs
      (receive (reqs opts) (split-at iargs reqargs)
        (append reqs (list ($list opts))))))

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

;; Input
;;   lvar is $lvar which points to the lambda to be called.
;;   lambda-node is $lambda-node which is to be called.
;;   calls if list of ($call $lambda-node ...) nodes.
;; Output
;;   local-ref to lambda-node in call-node will be replaced into $lambda-node.
;;   lambda-node will be marked as 'dissolved.
;;   ref-count of lvar become 0.
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
                                          name ($lambda.src lambda-node)))
    ($lvar.ref-count--! lvar)
    ($call.set-type! call 'embed)
    ($call.set-proc! call lambda-node)
    ($lambda.set-flag! lambda-node 'dissolved)
    (unless (null? rec-calls)
      (let1 body
          ($label ($lambda.body lambda-node))
        ($lambda.set-body! lambda-node body)
        (dolist (jcall rec-calls)
          ($lvar.ref-count--! lvar)
          ($call.set-args! jcall (adjust-arglist reqargs optarg
                                                 ($call.args jcall)
                                                 name ($lambda.src lambda-node)))
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
                                       name ($lambda.src lambda-node)))
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
       (values local rec trec))
      (((call . env) . more)
       (case ($call.type call)
         ((tail-rec)
          (if (direct-call? env)
              (loop more local rec (cons call trec))
              (loop more local (cons call rec) trec)))
         ((rec) (loop more local (cons call rec) trec))
         (else  (loop more (cons call local) rec trec)))))
    ))



;; (define (pass2/remove-vars vars init-iforms)
;;   (let loop ([vars vars]
;;              [init-iforms init-iforms]
;;              [rl '()]
;;              [ri '()]
;;              [rr '()])
;;     (cond [(null? vars)
;;            `#(,(reverse rl) ,(reverse ri) ,(reverse rr))]
;;           [(and (= 0 ($lvar.ref-count (car vars)))
;;                 (zero? ($lvar.set-count (car vars))))
;;            (cond [(tag? (car init-iforms) $LOCAL-REF) ;; if removed inits is $LOCAL-REF, decrement ref-count.
;;                   ($lvar.ref-count--! ($local-ref.lvar (car init-iforms)))])
;;            (loop (cdr vars) (cdr init-iforms) rl ri
;;                  (if (memq (tag (car init-iforms))
;;                            `(,$CONST ,$LOCAL-REF ,$LAMBDA))
;;                      rr
;;                      (cons (car init-iforms) rr)))]
;;           (else
;;            (loop (cdr vars) (cdr init-iforms)
;;                  (cons (car vars) rl) (cons (car init-iforms) ri) rr)))))

;; (define (pass2/remove-vars lvars inits)
;;   (let loop ((lvars lvars) (inits inits) (rl '()) (ri '()) (rr '()))
;;     (cond ((null? lvars)
;;            (vector (reverse rl) (reverse ri) (reverse rr)))
;;           ((and (zero? ($lvar.ref-count (car lvars)))
;;                 (zero? ($lvar.set-count (car lvars))))
;;            ;; TODO: if we remove $LREF from inits, we have to decrement
;;            ;; refcount?
;;            (loop (cdr lvars) (cdr inits) rl ri
;;                  (if (memv (tag (car inits))
;;                            `(,$CONST ,$LOCAL-REF ,$LAMBDA))
;;                    rr
;;                    (cons (car inits) rr))))
;;           (else
;;            (loop (cdr lvars) (cdr inits)
;;                  (cons (car lvars) rl) (cons (car inits) ri) rr)))))

(define (pass2/remove-vars lvars inits)
  (let loop ((lvars lvars) (inits inits) (rl '()) (ri '()) (rr '()))
    (cond ((null? lvars)
           (values (reverse rl) (reverse ri) (reverse rr)))
          ((and (zero? ($lvar.ref-count (car lvars)))
                (zero? ($lvar.set-count (car lvars))))
           ;; TODO: if we remove $LREF from inits, we have to decrement
           ;; refcount?
           (loop (cdr lvars) (cdr inits) rl ri
                 (if (memv (tag (car inits))
                           `(,$CONST ,$LOCAL-REF ,$LAMBDA))
                   rr
                   (cons (car inits) rr))))
          (else
           (loop (cdr lvars) (cdr inits)
                 (cons (car lvars) rl) (cons (car inits) ri) rr)))))




(define (pass2/self-recursing? closure closures)
;  (find10 (lambda (c) (eq? closure c)) closures))
  (memq closure closures))

;; (define (pass2/classify-local-ref-call iform closures tail?)
;;   (let1 lvar ($local-ref.lvar iform)
;;     (if (> ($lvar.set-count lvar) 0)
;;         'local)
;;     (let1 init-val ($lvar.init-val lvar)
;;       (cond [(and init-val (tag? init-val $LAMBDA))
;;              (cond [(pass2/self-recursing? init-val closures)
;;                     (if tail? 'tail-rec 'rec)]
;;                    [(= ($lvar.ref-count lvar) 1)
;;                     ($lvar.ref-count--! lvar)
;;                     ($lvar.set-init-val! lvar '())
;;                     init-val]
;;                    [else
;;                     'local])]
;;             [else
;;              #f]))))

(define-pass2/tracable (pass2/classify-local-ref-call iform closures tail?)
  (let* ([lvar ($local-ref.lvar iform)]
         [zerop (zero? ($lvar.set-count lvar))]
         [init-val ($lvar.init-val lvar)]
         [vectorp (vector? init-val)])
    (if vectorp
        (let1 lambdap (tag? init-val $LAMBDA)
          (if (and lvar zerop init-val lambdap)
              (cond [(pass2/self-recursing? init-val closures)
                     (if tail? 'tail-rec 'rec)]
                    [(= ($lvar.ref-count lvar) 1)
                     ($lvar.ref-count--! lvar)
                     ($lvar.set-init-val! lvar '())
                     init-val]
                    [else
                     'local])
              #f))
        #f)))

(define-pass2/tracable (pass2/expand-inlined-procedure iform iargs)
  (let ((lvars ($lambda.lvars iform))
        (args  (pass2/adjust-arglist ($lambda.reqargs iform) ($lambda.optarg iform)
                                     iargs ($lambda.name iform) ($lambda.src iform))))
    (for-each (lambda (lv a) ($lvar.set-init-val! lv a)) lvars args)
    ($let 'let lvars args ($lambda.body iform) #f #f)))

(define (pass2/argcount-ok? args reqargs optarg?)
  (let1 nargs (length args)
    (or (and (not optarg?) (= nargs reqargs))
        (and optarg? (>= nargs reqargs)))))

(define (pass2/adjust-arglist reqargs optarg iargs name src)
  (unless (pass2/argcount-ok? iargs reqargs (> optarg 0))
    (if src
        (errorf "wrong number of arguments: ~a requires ~a, but got ~a at ~a:~a"
                name reqargs (length iargs) (caar src) (cadar src))
        (errorf "wrong number of arguments: ~a requires ~a, but got ~a"
                name reqargs (length iargs))))
  (if (zero? optarg)
      iargs
      (let* ([ret-args (pass2/split-args iargs reqargs)]
             [reqs     (car ret-args)]
             [opts     (cdr ret-args)])
        (append reqs (list ($list opts))))))

(define (pass2/split-args args reqargs)
  (let loop ((i reqargs) (rest args) (r '()))
    (cond ((= i 0) (cons (reverse r) rest))
          ((null? rest) (error "given list is too short:" args))
          (else (loop (- i 1) (cdr rest) (cons (car rest) r))))))

;; (define (pass2/collect-call iform closures)
;;   (cond
;;    [($call.type iform) iform]
;;    [else
;;     (let ([proc ($call.proc iform)]
;;           [args ($call.args iform)])
;;       ;; scan OP first to give an opportunity of variable renaming
;;       ($call.set-proc! iform (pass2/optimize ($call.proc iform) closures))
;;       (cond [(tag? proc $LAMBDA)
;;              (pass2/optimize (pass2/expand-inlined-procedure proc args)
;;                              closures)]
;;             [(and (tag? proc $LOCAL-REF)
;;                   (pass2/classify-local-ref-call proc closures ($call.tail? iform)))
;;              => (lambda (type)
;;                   (cond
;;                    ;; Directly inline
;;                    [(vector? type)
;;                     ($call.set-proc! iform type)
;;                     ;; Directly inlinable case.  NB: this only happens if the $LREF
;;                     ;; node is the lvar's single reference, so we know the inlined
;;                     ;; procedure is never called recursively.  Thus we can safely
;;                     ;; traverse the inlined body without going into infinite loop.
;;                     ;;                ($call-proc-set! iform result)
;;                     (let1 o (pass2/expand-inlined-procedure type args)
;;                       (pass2/optimize o closures)
;;                     )
;;                     ;;                           penv tail?))
;;                     ]
;;                    [(not type)
;;                     iform]
;;                    [else
;;                     (let1 lambda-iform ($lvar.init-val ($local-ref.lvar proc))
;;                       ($call.set-type! iform type)
;;                       ($lambda.set-calls! lambda-iform
;;                                           (cons (cons iform closures)
;;                                                 ($lambda.calls lambda-iform)))
;;                       ;; todo
;;                       ;; args の最適化 see Gauche
;;                       ($call.set-args! iform (imap (lambda (x) (pass2/optimize x closures)) args))
;;                       iform)]))]
;;             [else
;;              ($call.set-args! iform (imap (lambda (x) (pass2/optimize x closures)) args))
;;              iform]))]))

;;--------------------------------------------------------------------
;;
;;  Pass3
;;
;;

(define (pass3/exists-in-can-frees? sym can-frees)
  (if (null? can-frees)
      #f
      (if (memq sym (car can-frees))
          #t
          (pass3/exists-in-can-frees? sym (cdr can-frees)))))

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
          (append ($append-map1 (lambda (fm) (rec fm l labels-seen)) ($let.inits i))
                  (rec ($let.body i) ($let.lvars i) labels-seen))]
         [(= $RECEIVE t)
          (append (rec ($receive.vals i) l labels-seen)
                  (rec ($receive.body i) ($receive.lvars i) labels-seen))]
         [(= $SEQ t)
          ($append-map1 (lambda (fm) (rec fm l labels-seen)) ($seq.body i))]
         [(= $LAMBDA t)
          (rec ($lambda.body i) ($lambda.lvars i) labels-seen)]
         [(= $LOCAL-ASSIGN t)
          (let1 sym ($lvar.sym ($local-assign.lvar i))
            (if (pass3/exists-in-can-frees? sym can-frees)
                (cons sym (rec ($local-assign.val i) l labels-seen))
                (rec ($local-assign.val i) l labels-seen)))]
         [(= $LOCAL-REF t)
          (let1 sym ($lvar.sym ($local-ref.lvar i))
            (cond [(memq sym l) '()]
                  [(pass3/exists-in-can-frees? sym can-frees) (list sym)]
                  [else '()]))]
         [(= $GLOBAL-REF t)
          (let* ([sym ($global-ref.sym i)]
                 [found (pass3/exists-in-can-frees? sym can-frees)])
            (if found (list sym) '()))]
         [(= $UNDEF t)      '()]
         [(= $IF t)
          (append (rec ($if.test i) l labels-seen)
                  (append (rec ($if.then i) l labels-seen)
                          (rec ($if.else i) l labels-seen)))]
         [(= $ASM t)
          ($append-map1 (lambda (fm) (rec fm l labels-seen)) ($asm.args i))]
         [(= $DEFINE t)
          (rec ($define.val i) l labels-seen)]
         [(= $CALL t)
          ;; N.B.
          ;; (proc args)
          ;;   args are evaluate before proc, so you should find free variables of args at first.
          (append
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
         [(= $IT t) '()]
         [else
          (error "pass3/find-free unknown iform:" (tag i))])))
    (uniq (rec iform locals '())))

;;
;; Find free variables in IForm.
;;   free variables is neither global variable nor local variable.
;;
;;   Arguments
;;     iform:     IForm
;;     locals:    local variables as $lvar structure.
;;     can-frees: candidates of free variables as $lvar structure.
;;
(cond-expand
 [vm?
  ;; moved to freeproc.cpp
  ;; N.B. these procedures are still required by vm.scm
  (define (pass3/find-free iform locals can-frees)
    (define (rec i l labels-seen)
      (let1 t (tag i)
        (cond
         [(= $CONST t) '()]
         [(= $LET t)
          (append ($append-map1 (lambda (fm) (rec fm l labels-seen)) ($let.inits i))
                  (rec ($let.body i) ($let.lvars i) labels-seen))]
         [(= $RECEIVE t)
          (append (rec ($receive.vals i) l labels-seen)
                  (rec ($receive.body i) ($receive.lvars i) labels-seen))]
         [(= $SEQ t)
          ($append-map1 (lambda (fm) (rec fm l labels-seen)) ($seq.body i))]
         [(= $LAMBDA t)
          (rec ($lambda.body i) ($lambda.lvars i) labels-seen)]
         [(= $LOCAL-ASSIGN t)
          (let1 sym ($lvar.sym ($local-assign.lvar i))
            (if (pass3/exists-in-can-frees? sym can-frees)
                (cons sym (rec ($local-assign.val i) l labels-seen))
                (rec ($local-assign.val i) l labels-seen)))]
         [(= $LOCAL-REF t)
          (let1 sym ($lvar.sym ($local-ref.lvar i))
            (cond [(memq sym l) '()]
                  [(pass3/exists-in-can-frees? sym can-frees) (list sym)]
                  [else '()]))]
         [(= $GLOBAL-REF t)
          (let* ([sym ($global-ref.sym i)]
                 [found (pass3/exists-in-can-frees? sym can-frees)])
            (if found (list sym) '()))]
         [(= $UNDEF t)      '()]
         [(= $IF t)
          (append (rec ($if.test i) l labels-seen)
                  (append (rec ($if.then i) l labels-seen)
                          (rec ($if.else i) l labels-seen)))]
         [(= $ASM t)
          ($append-map1 (lambda (fm) (rec fm l labels-seen)) ($asm.args i))]
         [(= $DEFINE t)
          (rec ($define.val i) l labels-seen)]
         [(= $CALL t)
          ;; N.B.
          ;; (proc args)
          ;;   args are evaluate before proc, so you should find free variables of args at first.
          (append
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
         [(= $IT t) '()]
         [else
          (error "pass3/find-free unknown iform:" (tag i))])))
    (uniq (rec iform locals '())))

  ;; moved to freeproc.cpp
  ;; N.B. these procedures are still required by vm.scm
  (define (pass3/find-sets iform lvars)
    (define (rec i labels-seen)
      (let1 t (tag i)
        (cond
         [(= $CONST t) '()]
         [(= $LET t)
          (append ($append-map1 (lambda (init) (rec init labels-seen)) ($let.inits i))
                  (rec ($let.body i) labels-seen))]
         [(= $RECEIVE t)
          (append (rec ($receive.vals i) labels-seen)
                  (rec ($receive.body i) labels-seen))]
         [(= $SEQ t)
          ($append-map1 (lambda (fm) (rec fm labels-seen)) ($seq.body i))]
         [(= $LAMBDA t)
          (rec ($lambda.body i) labels-seen)]
         [(= $LOCAL-ASSIGN t)
          (let1 lvar ($local-assign.lvar i)
            (append (if (memq lvar lvars) (list lvar) '())
                    (rec ($local-assign.val i) labels-seen)))]
         [(= $LOCAL-REF t)  '()]
         [(= $GLOBAL-REF t) '()]
         [(= $UNDEF t)      '()]
         [(= $IF t)
          (append (rec ($if.test i) labels-seen)
                  (rec ($if.then i) labels-seen)
                  (rec ($if.else i) labels-seen))]
         [(= $ASM t)
          ($append-map1 (lambda (arg) (rec arg labels-seen)) ($asm.args i))]
         [(= $DEFINE t)
          (rec ($define.val i) labels-seen)]
         [(= $CALL t)
          (append
           ($append-map1 (lambda (arg) (rec arg labels-seen)) ($call.args i))
           (rec ($call.proc i) labels-seen)
           )]
         [(= $CALL-CC t)
          (rec ($call-cc.proc i) labels-seen)]
         [(= $GLOBAL-ASSIGN t)
          (rec ($global-assign.val i) labels-seen)]
         [(= $LIST t)
          ($append-map1 (lambda (arg) (rec arg labels-seen)) ($list.args i))]
         [(= $LABEL t)
          (if (memq i labels-seen)
              '()
              (rec ($label.body i) (cons i labels-seen)))]
         [(= $IT t) '()]
         [else
          (error "pass3/find-sets unknown iform:" i)])))
    (uniq (rec iform '())))
  ]
 [else #f])

(cond-expand
 [mosh
  (define make-file-options
    (enum-set-constructor
     (make-enumeration
      '(no-create no-fail no-truncate))))
  ]
 [else
  (define (make-array)
    (list 'array (make-vector 2) 0))

  (define (array? obj)
    (eq? 'array (car obj)))

  (define (array-length array)
    (caddr array))

  (define (vector-copy dst src length)
    (do ((i 0 (+ i 1))) ((>= i length) #f)
      (vector-set! dst i (vector-ref src i))))

  (define (set-array-length! array length)
    (set! (caddr array) length)
    (when (>= length (vector-length (array-data array)))
      (let1 next-data (make-vector (* length 2))
        (vector-copy next-data (array-data array) length)
        (set! (cadr array) next-data))))

  (define (array-data array)
    (cadr array))

  (define (array-push! array obj)
    (let* ([data (array-data array)]
           [length (array-length array)])
      (vector-set! data length obj)
      (set-array-length! array (+ length 1))))

  (define (array->list array)
    (let ([data (array-data array)]
          [length (array-length array)])
      (let loop ([i 0]
                 [ret '()])
        (if (>= i length)
            (reverse ret)
            (loop (+ i 1) (cons (vector-ref data i) ret))))))

  ;; moved to freeproc.cpp start
  ;; N.B. these procedures are still required by vm.scm
  (define (make-code-builder)
    (make-array))

  (define (code-builder-put-extra1! cb x)
    (array-push! cb x))

  (define (code-builder-put-extra2! cb a b)
    (array-push! cb a)
    (array-push! cb b)
    )


  (define (code-builder-put-extra3! cb a b c)
    (array-push! cb a)
    (array-push! cb b)
    (array-push! cb c)
    )

  (define (code-builder-put-extra4! cb a b c d)
    (array-push! cb a)
    (array-push! cb b)
    (array-push! cb c)
    (array-push! cb d)
    )

  (define (code-builder-put-extra5! cb a b c d e)
    (array-push! cb a)
    (array-push! cb b)
    (array-push! cb c)
    (array-push! cb d)
    (array-push! cb e)
    )

  (define (code-builder-append! cb1 cb2)
    (let loop ([e (array->list cb2)])
      (cond
       [(null? e)
        '()]
       [else
        (code-builder-put-extra1! cb1 (car e))
        (loop (cdr e))])))

  (define (code-builder-emit cb)
    (array->list cb))

  (define code-builder-put-insn-arg2! code-builder-put-extra3!)
  (define code-builder-put-insn-arg1! code-builder-put-extra2!)
  (define code-builder-put-insn-arg0! code-builder-put-extra1!)
  ;; moved to freeproc.cpp end
  ])

;; code-builder synonym
(define-macro (cput! cb . more)
  (match more
    [() '()]
    [(a b c d e . f)
     `(begin (code-builder-put-extra5! ,cb ,a ,b ,c ,d ,e)
             (cput! ,cb ,@f))]
    [(a b c d . e)
     `(begin (code-builder-put-extra4! ,cb ,a ,b ,c ,d)
             (cput! ,cb ,@e))]
    [(a b c . d)
     `(begin (code-builder-put-extra3! ,cb ,a ,b ,c)
             (cput! ,cb ,@d))]
    [(a b . c)
     `(begin (code-builder-put-extra2! ,cb ,a ,b)
             (cput! ,cb ,@c))]
    [(a . b)
     `(begin (code-builder-put-extra1! ,cb ,a)
             (cput! ,cb ,@b))]))

(define-macro (cput-shift! cb n m)
  `(when (> ,m 0)
    (code-builder-put-insn-arg2! ,cb 'SHIFT ,n ,m)))

(define-macro (pass3/add-sets! sets new-sets)
  `(if (null? ,new-sets)
       ,sets
       (hashtable-set-true! (eq-hashtable-copy ,sets) ,new-sets)))

(define-macro (pass3/let-frame-size) 2)
(define-macro (pass3/frame-size) 4)

(define (pass3/collect-free cb frees-here locals frees)
  (let loop ([size 0]
             [reversed-frees (reverse frees-here)])
    (cond
     [(null? reversed-frees) size]
     [else
      (let1 stack-size (pass3/compile-refer cb (car reversed-frees) locals frees)
        (code-builder-put-insn-arg0! cb 'PUSH)
        (loop (+ size stack-size) (cdr reversed-frees)))])))

(define (pass3/symbol-lookup cb lvar locals frees return-local return-free)
  (let next-local ([locals locals] [n 0])
    (if (null? locals)
        (let next-free ([free frees] [n 0])
          (cond [(null? free)
                 (error "pass3/symbol-lookup bug? Unknown lvar:" lvar)]
                [(eq? (car free) lvar)
                 (return-free cb n)]
                [else
                 (next-free (cdr free) (+ n 1))]))
        (if (eq? (car locals) lvar)
            (return-local cb n)
            (next-local (cdr locals) (+ n 1))))))

;; N.B. Do NOT use anonymous closure for this usage. Because symbol-lookup will be called many times.
;; moved to CompilerProcedures.cpp
(define (pass3/return-refer-local cb n)
  (code-builder-put-insn-arg1! cb 'REFER_LOCAL n)
  0)

(define (pass3/return-refer-free cb n)
  (code-builder-put-insn-arg1! cb 'REFER_FREE n)
  0)

;; moved to CompilerProcedures.cpp
;; N.B. this procedure is still required by vm.scm
(define (pass3/compile-refer cb lvar locals frees)
  (pass3/symbol-lookup cb lvar locals frees pass3/return-refer-local pass3/return-refer-free))

(define (pass3/return-assign-local cb n)
  (code-builder-put-insn-arg1! cb 'ASSIGN_LOCAL n)
  0)

(define (pass3/return-assign-free cb n)
  (code-builder-put-insn-arg1! cb 'ASSIGN_FREE n)
  0)

(define (pass3/compile-assign cb lvar locals frees)
  (pass3/symbol-lookup cb lvar locals frees pass3/return-assign-local pass3/return-assign-free))

(define (pass3/make-boxes cb sets vars)
  ($for-each1-with-rindex (lambda (index var)
                            (if (memq var sets)
                                (code-builder-put-insn-arg1! cb 'BOX index)))
                          vars))

(define pass3/dispatch-table (make-vector $INSN-NUM))

(define (pass3/register insn proc)
  (vector-set! pass3/dispatch-table insn proc))

(define (pass3/$const cb iform locals frees can-frees sets tail depth display-count)
;;   (display "pass3/$const\n" (current-error-port))
;;   (pp-iform iform)

  (code-builder-put-insn-arg1! cb 'CONSTANT ($const.val iform))
  0)

(define (pass3/$it cb iform locals frees can-frees sets tail depth display-count) 0)

(define (pass3/$list cb iform locals frees can-frees sets tail depth display-count)
  (let1 args ($list.args iform)
    (begin0
      (fold (lambda (i accum)
              (let1 size (pass3/rec cb i locals frees can-frees sets tail depth display-count)
                (code-builder-put-insn-arg0! cb 'PUSH)
                (+ size accum))) 0 args)
      (code-builder-put-insn-arg1! cb 'LIST (length args)))))

;; $local-lef is classified into REFER_LOCAL and REFER_FREE
(define (pass3/$local-ref cb iform locals frees can-frees sets tail depth display-count)
  (pass3/compile-refer cb ($lvar.sym ($local-ref.lvar iform)) locals frees)
  (when (hashtable-ref sets ($local-ref.lvar iform) #f)
    (code-builder-put-insn-arg0! cb 'INDIRECT))
  0)

;; $local-assign is classified into ASSIGN_LOCAL and ASSIGN_FREE
(define (pass3/$local-assign cb iform locals frees can-frees sets tail depth display-count)
  (let ([val-stack-size (pass3/rec cb ($local-assign.val iform) locals frees can-frees sets #f depth display-count)]
        [var-stack-size (pass3/compile-assign cb ($lvar.sym ($local-assign.lvar iform)) locals frees)])
    (+ val-stack-size var-stack-size)))

;; $global-lef is classified into REFER_GLOBAL and REFER_FREE
(define (pass3/$global-ref cb iform locals frees can-frees sets tail depth display-count)
  (let1 sym ($global-ref.sym iform)
    (let next-free ([free frees] [n 0])
      (cond [(null? free)
             (code-builder-put-insn-arg1! cb 'REFER_GLOBAL sym)
             0]
            [(eq? (car free) sym)
             (code-builder-put-insn-arg1! cb 'REFER_FREE n)
             0]
            [else
             (next-free (cdr free) (+ n 1))]))))

(define (pass3/$global-assign cb iform locals frees can-frees sets tail depth display-count)
  (let1 sym ($global-assign.sym iform)
    (let next-free ([free frees] [n 0])
      (cond
       [(null? free)
        (begin0
          (pass3/rec cb ($global-assign.val iform) locals frees can-frees sets #f depth display-count)
          (code-builder-put-insn-arg1! cb
                 'ASSIGN_GLOBAL
                 sym))]
       [(eq? (car free) sym)
        (begin0
         (pass3/rec cb ($global-assign.val iform) locals frees can-frees sets #f depth display-count)
         (code-builder-put-insn-arg1! cb 'ASSIGN_FREE n))]
       [else
        (next-free (cdr free) (+ n 1))]))))

(define (pass3/$seq cb iform locals frees can-frees sets tail depth display-count)
  (let loop ([form ($seq.body iform)]
             [size 0])
    (cond
     [(null? form) size]
     [else
      (let1 tail? (if (null? (cdr form)) tail #f)
        (loop (cdr form)
              (+ size (pass3/rec cb (car form) locals frees can-frees sets tail? depth display-count))))])))

(define (pass3/$undef cb iform locals frees can-frees sets tail depth display-count)
  (code-builder-put-insn-arg0! cb 'UNDEF)
  0)

(define (pass3/$asm-1-arg cb insn arg1 locals frees can-frees sets depth display-count)
  (begin0
    (pass3/rec cb arg1 locals frees can-frees sets #f depth display-count)
    (code-builder-put-insn-arg0! cb insn)))

(define (pass3/$asm-2-arg cb insn arg1 arg2 locals frees can-frees sets depth display-count)
    (let ([x (pass3/compile-arg cb arg1 locals frees can-frees sets #f depth display-count)]
          [y (pass3/rec cb arg2 locals frees can-frees sets #f depth display-count)])
      (code-builder-put-insn-arg0! cb insn)
      (+ x y)))

(define (pass3/$asm-3-arg cb insn arg1 arg2 arg3 locals frees can-frees sets depth display-count)
  (let ([x (pass3/compile-arg cb arg1 locals frees can-frees sets #f depth display-count)]
        [y (pass3/compile-arg cb arg2 locals frees can-frees sets #f depth display-count)]
        [z (pass3/rec cb arg3 locals frees can-frees sets #f depth display-count)])
    (code-builder-put-insn-arg0! cb insn)
    (+ x y z)))

(define (pass3/$asm-n-args cb args locals frees can-frees sets depth display-count)
  (let loop ([args args]
             [stack-size 0])
    (cond
     [(null? args) stack-size]
     [(null? (cdr args)) ;; last argument is not pushed.
      (+ stack-size (pass3/rec cb (car args) locals frees can-frees sets #f depth display-count))]
     [else
      (loop (cdr args)
            (+ stack-size (pass3/compile-arg cb (car args) locals frees can-frees sets #f depth display-count)))])))

(define (pass3/$asm cb iform locals frees can-frees sets tail depth display-count)
  (let1 args ($asm.args iform)
    (case ($asm.insn iform)
      [(APPEND2)           (pass3/$asm-2-arg cb  'APPEND2         (first args) (second args) locals frees can-frees sets depth display-count)]
      [(NUMBER_ADD)        (pass3/$asm-2-arg cb  'NUMBER_ADD      (first args) (second args) locals frees can-frees sets depth display-count)]
      [(NUMBER_SUB)        (pass3/$asm-2-arg cb  'NUMBER_SUB      (first args) (second args) locals frees can-frees sets depth display-count)]
      [(NUMBER_MUL)        (pass3/$asm-2-arg cb  'NUMBER_MUL      (first args) (second args) locals frees can-frees sets depth display-count)]
      [(NUMBER_DIV)        (pass3/$asm-2-arg cb  'NUMBER_DIV      (first args) (second args) locals frees can-frees sets depth display-count)]
      [(NUMBER_EQUAL)      (pass3/$asm-2-arg cb  'NUMBER_EQUAL    (first args) (second args) locals frees can-frees sets depth display-count)]
      [(NUMBER_GE)         (pass3/$asm-2-arg cb  'NUMBER_GE       (first args) (second args) locals frees can-frees sets depth display-count)]
      [(NUMBER_GT)         (pass3/$asm-2-arg cb  'NUMBER_GT       (first args) (second args) locals frees can-frees sets depth display-count)]
      [(NUMBER_LT)         (pass3/$asm-2-arg cb  'NUMBER_LT       (first args) (second args) locals frees can-frees sets depth display-count)]
      [(NUMBER_LE)         (pass3/$asm-2-arg cb  'NUMBER_LE       (first args) (second args) locals frees can-frees sets depth display-count)]
      [(CONS)              (pass3/$asm-2-arg cb  'CONS            (first args) (second args) locals frees can-frees sets depth display-count)]
      [(CAR)               (pass3/$asm-1-arg cb   'CAR             (first args) locals frees can-frees sets depth display-count)]
      [(CDR)               (pass3/$asm-1-arg cb   'CDR             (first args) locals frees can-frees sets depth display-count)]
      [(CAAR)              (pass3/$asm-1-arg cb   'CAAR            (first args) locals frees can-frees sets depth display-count)]
      [(CADR)              (pass3/$asm-1-arg cb   'CADR            (first args) locals frees can-frees sets depth display-count)]
      [(CDAR)              (pass3/$asm-1-arg cb   'CDAR            (first args) locals frees can-frees sets depth display-count)]
      [(CDDR)              (pass3/$asm-1-arg cb   'CDDR            (first args) locals frees can-frees sets depth display-count)]
      [(SET_CDR)           (pass3/$asm-2-arg cb  'SET_CDR         (first args) (second args) locals frees can-frees sets depth display-count)]
      [(SET_CAR)           (pass3/$asm-2-arg cb  'SET_CAR         (first args) (second args) locals frees can-frees sets depth display-count)]
      [(MAKE_VECTOR)       (pass3/$asm-2-arg cb  'MAKE_VECTOR     (first args) (second args) locals frees can-frees sets depth display-count)]
      [(VECTOR_LENGTH)     (pass3/$asm-1-arg cb   'VECTOR_LENGTH   (first args)locals frees can-frees sets depth display-count)]
      [(VECTOR_SET)        (pass3/$asm-3-arg cb 'VECTOR_SET      (first args) (second args) (third args) locals frees can-frees sets depth display-count)]
      [(VECTOR_REF)        (pass3/$asm-2-arg cb  'VECTOR_REF      (first args) (second args) locals frees can-frees sets depth display-count)]
      [(SIMPLE_STRUCT_REF) (pass3/$asm-2-arg cb  'SIMPLE_STRUCT_REF (first args) (second args) locals frees can-frees sets depth display-count)]
      [(EQ)                (pass3/$asm-2-arg cb  'EQ              (first args) (second args) locals frees can-frees sets depth display-count)]
      [(EQV)               (pass3/$asm-2-arg cb  'EQV             (first args) (second args) locals frees can-frees sets depth display-count)]
      [(EQUAL)             (pass3/$asm-2-arg cb  'EQUAL           (first args) (second args) locals frees can-frees sets depth display-count)]
      [(PAIR_P)            (pass3/$asm-1-arg cb   'PAIR_P          (first args) locals frees can-frees sets depth display-count)]
      [(NULL_P)            (pass3/$asm-1-arg cb   'NULL_P          (first args) locals frees can-frees sets depth display-count)]
      [(SYMBOL_P)          (pass3/$asm-1-arg cb   'SYMBOL_P        (first args) locals frees can-frees sets depth display-count)]
      [(VECTOR_P)          (pass3/$asm-1-arg cb   'VECTOR_P        (first args) locals frees can-frees sets depth display-count)]
      [(NOT)               (pass3/$asm-1-arg cb   'NOT             (first args) locals frees can-frees sets depth display-count)]
      [(OPEN_INPUT_FILE)   (pass3/$asm-1-arg cb   'OPEN_INPUT_FILE (first args) locals frees can-frees sets depth display-count)]
      [(READ)              (pass3/$asm-1-arg cb   'READ            (first args) locals frees can-frees sets depth display-count)]
      [(READ_CHAR)         (pass3/$asm-1-arg cb   'READ_CHAR       (first args) locals frees can-frees sets depth display-count)]
      [(VALUES)
       (begin0
         (pass3/$asm-n-args cb args locals frees can-frees sets depth display-count)
         (code-builder-put-insn-arg1! cb 'VALUES (length args)))]
      [(VECTOR)
       (begin0
         (pass3/$asm-n-args cb args locals frees can-frees sets depth display-count)
         (code-builder-put-insn-arg1! cb 'VECTOR (length args)))]
      [(APPLY)
       (let1 end-of-frame (make-label)
         (code-builder-put-insn-arg1! cb 'FRAME (ref-label end-of-frame))
         (let1 arg2-size (pass3/rec cb (second args) locals frees can-frees sets #f depth display-count)
           (code-builder-put-insn-arg0! cb 'PUSH)
           (let1 arg1-size (pass3/rec cb (first args) locals frees can-frees sets #f depth display-count)
             (cput! cb 'APPLY end-of-frame)
             (+ arg1-size arg2-size))))]
      [else
       (error "unknown insn on pass3/$asm")])))

(define (pass3/$if cb iform locals frees can-frees sets tail depth display-count)
  (cond
   [(and (not (tag? ($if.then iform) $IT))
         (not (tag? ($if.else iform) $IT))
         (tag? ($if.test iform) $ASM)
         (eqv? ($asm.insn ($if.test iform)) 'NOT))
    (pass3/$if cb ($if (car ($asm.args ($if.test iform)))
                       ($if.else iform)
                       ($if.then iform))
               locals frees can-frees sets tail depth display-count)]
   ;; (if a b #f) => (if a b IT)
   [(and (tag? ($if.else iform) $CONST) (not ($const.val ($if.else iform))))
    ($if.set-else! iform ($it))
    (pass3/$if cb iform locals frees can-frees sets tail depth display-count)]
   [(tag? ($if.test iform) $ASM)
    (let1 insn ($asm.insn ($if.test iform))
      (cond
       [(eq? insn 'NULL_P)
        (pass3/branch-on-asm1 'BRANCH_NOT_NULL cb iform locals frees can-frees sets tail depth display-count)]
       [(eq? insn 'EQ)
        (pass3/branch-on-asm2 'BRANCH_NOT_EQ cb iform locals frees can-frees sets tail depth display-count)]
       [(eq? insn 'EQV)
        (pass3/branch-on-asm2 'BRANCH_NOT_EQV cb iform locals frees can-frees sets tail depth display-count)]
       [(eq? insn 'EQUAL)
        (pass3/branch-on-asm2 'BRANCH_NOT_EQUAL cb iform locals frees can-frees sets tail depth display-count)]
       [(eq? insn 'NUMBER_EQUAL)
        (pass3/branch-on-asm2 'BRANCH_NOT_NUMBER_EQUAL cb iform locals frees can-frees sets tail depth display-count)]
       [(eq? insn 'NUMBER_LE)
        (pass3/branch-on-asm2 'BRANCH_NOT_LE cb iform locals frees can-frees sets tail depth display-count)]
       [(eq? insn 'NUMBER_LT)
        (pass3/branch-on-asm2 'BRANCH_NOT_LT cb iform locals frees can-frees sets tail depth display-count)]
       [(eq? insn 'NUMBER_GT)
        (pass3/branch-on-asm2 'BRANCH_NOT_GT cb iform locals frees can-frees sets tail depth display-count)]
       [(eq? insn 'NUMBER_GE)
        (pass3/branch-on-asm2 'BRANCH_NOT_GE cb iform locals frees can-frees sets tail depth display-count)]
       [else
        (pass3/branch-on-false cb iform locals frees can-frees sets tail depth display-count)]))]
   [else
    (pass3/branch-on-false cb iform locals frees can-frees sets tail depth display-count)]))

(define (pass3/emit-then-else insn test-size cb iform locals frees can-frees sets tail depth display-count)
  (let ([end-of-else   (make-label)]
        [begin-of-else (make-label)])
    (code-builder-put-insn-arg1! cb insn (ref-label begin-of-else))
    (let1 then-size (pass3/rec cb ($if.then iform) locals frees can-frees sets tail depth display-count)
      (cond
       ;; When else clause is $IT, we can omit the jump after then clause.
       [(tag? ($if.else iform) $IT)
        (cput! cb
               (ref-label begin-of-else))
        (+ test-size then-size)]
       [else
        (cput! cb
               'UNFIXED_JUMP
               (ref-label end-of-else)
               begin-of-else)
        (let1 else-size (pass3/rec cb ($if.else iform) locals frees can-frees sets tail depth display-count)
          (cput! cb end-of-else)
          (+ test-size then-size else-size))]))))

(define (pass3/branch-on-asm1 insn cb iform locals frees can-frees sets tail depth display-count)
  (let* ([args ($asm.args ($if.test iform))]
         [arg-size (pass3/rec cb (first args) locals frees can-frees sets #f depth display-count)])
    (pass3/emit-then-else insn arg-size cb iform  locals frees can-frees sets tail depth display-count)))

(define (pass3/branch-on-asm2 insn cb iform locals frees can-frees sets tail depth display-count)
  (let* ([args ($asm.args ($if.test iform))]
         [arg1-size (pass3/compile-arg cb (first args) locals frees can-frees sets #f depth display-count)]
         [arg2-size (pass3/rec cb (second args) locals frees can-frees sets #f depth display-count)])
    (pass3/emit-then-else insn (+ arg1-size arg2-size) cb iform  locals frees can-frees sets tail depth display-count)))

(define (pass3/branch-on-false cb iform locals frees can-frees sets tail depth display-count)
  (let1 test-size (pass3/rec cb ($if.test iform) locals frees can-frees sets #f depth display-count)
    (pass3/emit-then-else 'TEST test-size cb iform  locals frees can-frees sets tail depth display-count)))

(define (pass3/$define cb iform locals frees can-frees sets tail depth display-count)
  (begin0
    (pass3/rec cb ($define.val iform) locals frees can-frees sets #f depth display-count)
    (cput! cb 'DEFINE_GLOBAL ($define.sym iform))))

(define (pass3/compile-arg cb arg locals frees can-frees sets tail depth display-count)
  (let1 size (pass3/rec cb arg locals frees can-frees sets #f depth display-count)
    (code-builder-put-insn-arg0! cb 'PUSH)
    (+ size 1)))

;; fold requires anonymous closure
;; So, if this procedure is called many times, it causes slow compilation.
(define (pass3/compile-args cb args locals frees can-frees sets tail depth display-count)
;  (format (current-error-port) "\npass3/compile-args<~a>\n" (length args))
  (let loop ([size 0]
             [iform args])
    (cond
     [(null? iform) size]
     [else
      (loop (+ size (pass3/compile-arg cb (car iform) locals frees can-frees sets tail depth display-count))
            (cdr iform))])))

;; N.B.
;; We don't append vars directory.
;; If vars's length is very long, it causes very slow compilations.
;; So we append the referrence of vars instread.
;; can-frees may be like following.
;; '((a b c) (x y) (i j))
(define-macro (pass3/add-can-frees1 can-frees vars)
  `(append ,can-frees (list ,vars)))

(define-macro (pass3/add-can-frees2 can-frees vars1 vars2)
  `(append (append ,can-frees (list ,vars1)) (list ,vars2)))

(define (pass3/$call cb iform locals frees can-frees sets tail depth display-count)
  (case ($call.type iform)
    [(jump)
     (let ([label ($lambda.body ($call.proc ($call.proc iform)))]
           [args-length (length ($call.args iform))])
       (begin0
         ;; This refers local variables at stack, so we do this first before emitting SHIFTJ.
         (pass3/compile-args cb ($call.args iform) locals frees can-frees sets #f depth display-count)
         ;;
         ;; Named let jump optimization needs to control stack like following.
         ;;
         ;;   let loop (var val) [jump destination] [... body ...] [jump point]
         ;;
         ;;   In [... body ...] we may have let or lambda, these expressions push arguments and frames to stack.
         ;;   So if we jump across the let or lambda boundary, we have to cleanup unnecessary arguments from stack.
         ;;   "depth" variable is used for this cleaning up.
         ;;   Just before the jump point, stack may be like following.
         ;;
         ;;     [first N values for loop]                <=== cleanup
         ;;     [M arguments and frames pushed in body]  <=== cleanup
         ;;     [next K values for loop]
         ;;
         ;;   We cleanup M + N objects in stack and shift.
         ;;
         ;;   Then we restore fp and display registers, and finally jump to [jump destination]
         ;;
         (cput! cb
                'SHIFTJ
                args-length
                (- depth ($call.depth ($call.proc iform)) (pass3/let-frame-size)) ;; we subtract let-frame size for this jump lambda's let-frame.
                (- display-count ($call.display-count ($call.proc iform)) 1)) ;; we subtract our display count.
         ($label.set-visited?! label #t)
         (cput! cb 'UNFIXED_JUMP label)))]
    [(embed)
     (let* ([label ($lambda.body ($call.proc iform))]
            [body ($label.body label)]
            [vars ($lambda.lvars ($call.proc iform))]
            [vars-sym (imap $lvar.sym-proc vars)]
            [frees-here (pass3/find-free body
                                         vars-sym
                                         (pass3/add-can-frees2 can-frees locals frees))]
            [sets-for-this-lvars (pass3/find-sets body vars)]
            [let-cb (make-code-builder)])
       ($call.set-depth! iform depth)           ;; record depth at jump destination point.
       ($call.set-display-count! iform display-count) ;; record display-count at jump destination point.
       (cput! cb 'LET_FRAME)
       (let* ([frees-here-length (length frees-here)]
              [free-size (if (> frees-here-length 0)
                             (pass3/collect-free let-cb frees-here locals frees)
                             0)]
              [need-display? (> frees-here-length 0)])
         (when need-display?
           (cput! let-cb 'DISPLAY frees-here-length))
         (let ([args-size (pass3/compile-args let-cb ($call.args iform) locals frees-here can-frees sets #f
                                              (+ depth (pass3/let-frame-size)) (if need-display? (+ display-count 1) display-count))]
               [args-length (length ($call.args iform))])
           (code-builder-put-insn-arg1! let-cb 'ENTER args-length)
           (cput! let-cb label)
           (pass3/make-boxes let-cb sets-for-this-lvars vars)
           ($label.set-visited?! label #t)
           (let1 body-size (pass3/rec let-cb
                                      body
                                      vars-sym
                                      frees-here
                                      (pass3/add-can-frees1 can-frees vars-sym)
                                      (pass3/add-sets! sets sets-for-this-lvars)
                                      (if tail (+ tail (length vars) (pass3/let-frame-size)) #f)
                                      (+ depth (length vars) (pass3/let-frame-size))
                                      (if need-display? (+ display-count 1) display-count))
             (code-builder-put-insn-arg1! let-cb 'LEAVE args-length)
             (cput! cb (+ args-size body-size free-size))
             (code-builder-append! cb let-cb)
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
         (code-builder-put-insn-arg1! cb 'FRAME (ref-label end-of-frame)))
       (let* ([args-size (pass3/compile-args cb ($call.args iform) locals frees can-frees sets #f depth display-count)]
              [proc-size (pass3/rec cb ($call.proc iform) locals frees can-frees sets #f depth display-count)]
              [args-length (length ($call.args iform))])
         (when tail
           (cput-shift! cb args-length tail))
         (if (eq? ($call.type iform) 'local)
             (code-builder-put-insn-arg1! cb 'LOCAL_CALL args-length)
             (code-builder-put-insn-arg1! cb 'CALL args-length))
         (unless tail
           (cput! cb end-of-frame))
         (+ args-size proc-size)))]))

(define (pass3/$call-cc cb iform locals frees can-frees sets tail depth display-count)
  (let1 end-of-frame (make-label)
    (unless tail
      (code-builder-put-insn-arg1! cb 'FRAME (ref-label end-of-frame)))
    (cput! cb 'MAKE_CONTINUATION (if tail 1 0))
    (code-builder-put-insn-arg0! cb 'PUSH)
    (begin0
      (pass3/rec cb ($call-cc.proc iform) locals frees can-frees sets #f depth display-count)
      (when tail
        (cput-shift! cb 1 tail))
      (code-builder-put-insn-arg1! cb 'CALL 1)
      (unless tail
        (cput! cb end-of-frame)))))

(define (pass3/$lambda cb iform locals frees can-frees sets tail depth display-count)
  (let* ([vars ($lambda.lvars iform)]
         [vars-sym (imap $lvar.sym-proc vars)]
         [body ($lambda.body iform)]
         [frees-here (pass3/find-free body
                                      vars
                                      (pass3/add-can-frees2 can-frees locals frees))]
         [sets-for-this-lvars (pass3/find-sets body vars)]
         [end-of-closure (make-label)]
         [lambda-cb (make-code-builder)]
         [frees-here-length (length frees-here)]
         [free-size (if (> frees-here-length 0)
                        (pass3/collect-free cb frees-here locals frees)
                        0)]
         [vars-length (length vars)])
    (cput! cb
           'CLOSURE
           (ref-label end-of-closure)
           vars-length                                              ;; length of arguments
           (> ($lambda.optarg iform) 0)                             ;; optional-arg?
           frees-here-length)                                       ;; number of free variables
      ;; we want to know stack size of lambda body, before emit.
      (let1 body-size (pass3/rec lambda-cb
                                 body
                                 vars-sym
                                 frees-here
                                 (pass3/add-can-frees1 can-frees vars-sym) ;; can-frees and vars don't have common lvars.
                                 (pass3/add-sets! sets sets-for-this-lvars)
                                 vars-length
                                 (+ (length vars) (pass3/frame-size) depth)
                                 display-count)
        (cput! cb
               (+ body-size free-size vars-length 4) ;; max-stack 4 is sizeof frame
               ($lambda.src iform))                    ;; source code information
        (pass3/make-boxes cb sets-for-this-lvars vars)
        (code-builder-append! cb lambda-cb)
        (code-builder-put-insn-arg1! cb 'RETURN vars-length)
        (cput! cb end-of-closure)
        0)))

(define (pass3/$receive cb iform locals frees can-frees sets tail depth display-count)
  (let* ([vars ($receive.lvars iform)]
         [vars-sym (imap $lvar.sym-proc vars)]
         [body ($receive.body iform)]
         [frees-here (append
                      (pass3/find-free ($receive.vals iform) locals (pass3/add-can-frees2 can-frees locals frees))
                      (pass3/find-free body
                                       vars-sym
                                       (pass3/add-can-frees2 can-frees locals frees)))]
         [sets-for-this-lvars (pass3/find-sets body vars)]
         [let-cb (make-code-builder)])
    (cput! cb 'LET_FRAME)
    (let* ([frees-here-length (length frees-here)]
           [free-size (if (> frees-here-length 0)
                          (pass3/collect-free let-cb frees-here locals frees)
                          0)]
           [need-display? (> frees-here-length 0)])
      (when need-display?
        (cput! let-cb 'DISPLAY frees-here-length))
      (let ([vals-size (pass3/rec let-cb ($receive.vals iform) locals frees-here can-frees sets #f
                                  (+ depth (pass3/let-frame-size)) ;; when evaluating vals, we have ready been in let-frame.
                                  (if need-display? (+ display-count 1) display-count))] ;; display++
            [vars-length (length vars)])
        (cput! let-cb 'RECEIVE ($receive.reqargs iform) ($receive.optarg  iform))
        (pass3/make-boxes let-cb sets-for-this-lvars vars)
        (code-builder-put-insn-arg1! let-cb 'ENTER vars-length)
        (let1 body-size (pass3/rec let-cb
                                   body
                                   vars-sym
                                   frees-here
                                   (pass3/add-can-frees1 can-frees vars-sym)
                                   (pass3/add-sets! sets sets-for-this-lvars)
                                   (if tail (+ tail vars-length (pass3/let-frame-size)) #f)
                                   (+ depth vars-length (pass3/let-frame-size))
                                   (if need-display? (+ display-count 1) display-count))
          (code-builder-put-insn-arg1! let-cb 'LEAVE vars-length)
          (cput! cb (+ body-size vals-size free-size))
          (code-builder-append! cb let-cb)
          (+ body-size vals-size free-size))))))

(define (pass3/$let cb iform locals frees can-frees sets tail depth display-count)
  (if (eq? ($let.type iform) 'rec)
      (pass3/letrec cb iform locals frees can-frees sets tail depth display-count)
      (let* ([vars ($let.lvars iform)]
             [vars-sym (imap $lvar.sym-proc vars)]
             [body ($let.body iform)]
             [frees-here (append
                          ($append-map1 (lambda (i) (pass3/find-free i locals (pass3/add-can-frees2 can-frees frees locals))) ($let.inits iform))
                          (pass3/find-free body
                                           vars-sym
                                           (pass3/add-can-frees2 can-frees frees locals)))]
             [sets-for-this-lvars (pass3/find-sets body vars)]
             [frees-here-length   (length frees-here)]
             [vars-length         (length vars)]
             [let-cb (make-code-builder)]
             [need-display? (> frees-here-length 0)])
        (cput! cb 'LET_FRAME)
        (let1 free-size (if (> frees-here-length 0) (pass3/collect-free let-cb frees-here locals frees) 0)
          (when need-display?
            (cput! let-cb 'DISPLAY frees-here-length))
          (let1 args-size (pass3/compile-args let-cb ($let.inits iform) locals frees-here can-frees sets tail
                                              (+ depth (pass3/let-frame-size)) (if need-display? (+ display-count 1) display-count))
            (pass3/make-boxes let-cb sets-for-this-lvars vars)
            (code-builder-put-insn-arg1! let-cb 'ENTER vars-length)
            (let1 body-size (pass3/rec let-cb
                                       body
                                       vars-sym
                                       frees-here
                                       (pass3/add-can-frees1 can-frees vars-sym)
                                       (pass3/add-sets! sets sets-for-this-lvars)
                                       (if tail (+ tail vars-length (pass3/let-frame-size)) #f)
                                       (+ depth vars-length (pass3/let-frame-size))
                                       (if need-display? (+ display-count 1) display-count))
              (code-builder-put-insn-arg1! let-cb 'LEAVE vars-length)
              (cput! cb (+ body-size args-size free-size))
              (code-builder-append! cb let-cb)
              (+ body-size args-size free-size)))))))

(define (raise-compile-error cb who message irritants)
  (cput! cb 'COMPILE_ERROR who message irritants))

(define (pass3/letrec cb iform locals frees can-frees sets tail depth display-count)
  (let* ([vars ($let.lvars iform)]
         [vars-sym (imap $lvar.sym-proc vars)]
         [body ($let.body iform)]
         [frees-here (append
                      ($append-map1 (lambda (i) (pass3/find-free i vars
(pass3/add-can-frees2 can-frees locals frees))) ($let.inits iform))
                      (pass3/find-free body
                                       vars-sym
                                       (pass3/add-can-frees2 can-frees locals frees)))]
         ;; each vars can be set!
         [sets-for-this-lvars  (append vars (pass3/find-sets body vars) ($append-map1 (lambda (i) (pass3/find-sets i vars)) ($let.inits iform)))]
         [args ($let.inits iform)]
         [frees-here-length (length frees-here)]
         [vars-length (length vars)]
         [let-cb (make-code-builder)]
         [need-display? (> frees-here-length 0)])
    (when ($let.error iform)
      (raise-compile-error cb (first ($let.error iform))
                              (second ($let.error iform))
                              (third ($let.error iform))))
    (cput! cb 'LET_FRAME)
    (let1 free-size (if (> frees-here-length 0) (pass3/collect-free let-cb frees-here locals frees) 0)
      (when need-display?
        (cput! let-cb 'DISPLAY frees-here-length))
      (let loop ([args args]) ;; init code
        (cond
         [(null? args) '()]
         [else
          (cput! let-cb 'UNDEF)
          (code-builder-put-insn-arg0! let-cb 'PUSH)
          (loop (cdr args))]))
      (pass3/make-boxes let-cb sets-for-this-lvars vars)
      (code-builder-put-insn-arg1! let-cb 'ENTER vars-length)
      (let* ([new-can-frees (pass3/add-can-frees1 can-frees vars-sym)]
             [assign-size
              (let loop ([args  args]
                         [size  0]
                         [index 0])
                (cond
                 [(null? args) size]
                 [else
                  (let1 stack-size (pass3/rec let-cb (car args) vars-sym frees-here
                                              new-can-frees
                                              (pass3/add-sets! sets sets-for-this-lvars)
                                              #f
                                              (+ depth (pass3/let-frame-size))
                                              (if need-display? (+ display-count 1) display-count))
                    (code-builder-put-insn-arg1! let-cb 'ASSIGN_LOCAL index)
                    (loop (cdr args)
                          (+ stack-size size)
                          (+ index 1)))]))])
             (let1 body-size (pass3/rec let-cb
                                        body
                                        vars-sym
                                        frees-here
                                        new-can-frees
                                        (pass3/add-sets! sets sets-for-this-lvars)
                                        (if tail (+ tail vars-length (pass3/let-frame-size)) #f)
                                        (+ depth vars-length (pass3/let-frame-size))
                                        (if need-display? (+ display-count 1) display-count))
          (code-builder-put-insn-arg1! let-cb 'LEAVE vars-length)
          (cput! cb (+ free-size assign-size body-size))
          (code-builder-append! cb let-cb)
          (+ free-size assign-size body-size))))))

(define (pass3/$label cb iform locals frees can-frees sets tail depth display-count)
  (cond
   [($label.visited? iform)
    (cput! cb 'UNFIXED_JUMP iform)
    0]
   [else
    ;; As far as I know, any code doesn't come here.
    ;; So, not *tested*.
    ($label.set-visited?! iform #t)
    (cput! cb iform) ;; place the label.
    (pass3/rec cb ($label.body iform)locals frees can-frees sets tail depth display-count)]))


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
(pass3/register $IT            pass3/$it)
(pass3/register $RECEIVE       pass3/$receive)
(pass3/register $LABEL       pass3/$label)

;; depth is the depth of frame, used for 'jump' and 'embeded' call and indicate the size of frame to discard.
(define (pass3/rec cb iform locals frees can-frees sets tail depth display-count)
  ((vector-ref pass3/dispatch-table (vector-ref iform 0)) cb iform locals frees can-frees sets tail depth display-count))

(define (pass3 iform)
  (let1 cb (make-code-builder)
    (pass3/rec cb iform '() *free-vars-decl* '() (make-eq-hashtable) #f 0 0)
    (code-builder-emit cb)))

(define (pass4 lst)
  (pass4/fixup-labels (list->vector (append lst '(HALT)))))

(define (pass4-w/o-halt lst)
  (pass4/fixup-labels (list->vector lst)))

;; merge-insn for Mosh is written in CodeBuilder.cpp
(cond-expand
 [mosh
  (define-macro (merge-insn sexp) sexp)]
 [else
  (define (merge-insn sexp)
    (define (iter s)
      (cond
       [(null? s) '()]
       [else
        (match s
          [('REFER_LOCAL_PUSH n 'CONSTANT . more)
           (iter `(REFER_LOCAL_PUSH_CONSTANT ,n ,@more))]
          [('REFER_LOCAL_PUSH_CONSTANT a b 'BRANCH_NOT_LE . more)
           (iter `(REFER_LOCAL_PUSH_CONSTANT_BRANCH_NOT_LE ,a ,b ,@more))]
          [('REFER_LOCAL_PUSH_CONSTANT a b 'BRANCH_NOT_GE . more)
           (iter `(REFER_LOCAL_PUSH_CONSTANT_BRANCH_NOT_GE ,a ,b ,@more))]
          [((and x (not 'CONSTANT)) 'REFER_LOCAL_PUSH_CONSTANT a b 'BRANCH_NOT_NUMBER_EQUAL . more)
           (iter `(,x REFER_LOCAL_PUSH_CONSTANT_BRANCH_NOT_NUMBER_EQUAL ,a ,b ,@more))]
          [((and x (not 'CONSTANT)) 'REFER_LOCAL n 'BRANCH_NOT_NULL . more)
           (iter `(,x REFER_LOCAL_BRANCH_NOT_NULL ,n ,@more))]
          [((and x (not 'CONSTANT)) 'REFER_LOCAL n 'BRANCH_NOT_LT . more)
           (iter `(,x REFER_LOCAL_BRANCH_NOT_LT ,n ,@more))]
          [((and x (not 'CONSTANT)) 'REFER_FREE n 'CALL . more)
           (iter `(,x REFER_FREE_CALL ,n ,@more))]
          [((and x (not 'CONSTANT)) 'REFER_LOCAL n 'CALL . more)
           (iter `(,x REFER_LOCAL_CALL ,n ,@more))]
          [((and x (not 'CONSTANT)) 'REFER_GLOBAL n 'PUSH . more)
           (iter `(,x REFER_GLOBAL_PUSH ,n ,@more))]
;;           [((and x (not 'CONSTANT)) 'PUSH_CONSTANT n 'VECTOR_SET . more)
;;            (iter `(,x PUSH_CONSTANT_VECTOR_SET ,n ,@more))]
;;           [((and x (not 'CONSTANT)) 'REFER_LOCAL n 'VECTOR_SET . more)
;;            (iter `(,x REFER_LOCAL_VECTOR_SET ,n ,@more))]
;;           [((and x (not 'CONSTANT)) 'REFER_LOCAL n 'VECTOR_REF . more)
;;            (iter `(,x REFER_LOCAL_VECTOR_REF ,n ,@more))]
;;           [((and x (not 'CONSTANT)) 'VECTOR_REF 'PUSH . more)
;;            (iter `(,x VECTOR_REF_PUSH ,@more))]
;;           [((and x (not 'CONSTANT)) 'REFER_LOCAL n 'CAR . more)
;;            (iter `(,x REFER_LOCAL_CAR ,n ,@more))]
;;           [((and x (not 'CONSTANT)) 'REFER_LOCAL n 'CDR . more)
;;            (iter `(,x REFER_LOCAL_CDR ,n ,@more))]
;;           [((and x (not 'CONSTANT)) 'REFER_LOCAL n 'CONS . more)
;;            (iter `(,x REFER_LOCAL_CONS ,n ,@more))]
          ;; N.B.
          ;; compiled pass3/$asm code has list '(CONSTANT NUMBER_SUB PUSH), ignore it.
          [((and x (not 'CONSTANT)) 'NUMBER_SUB 'PUSH . rest)
           (iter `(, x NUMBER_SUB_PUSH ,@rest))]
          [('PUSH 'ENTER . rest) ;; done
           (iter (cons 'PUSH_ENTER rest))]
          [('CONSTANT v 'PUSH . rest) ;; done
           (iter `(CONSTANT_PUSH ,v ,@rest))]
          [('REFER_FREE n 'PUSH . rest) ;; done
           (iter `(REFER_FREE_PUSH ,n ,@rest))]
          [('REFER_FREE_PUSH n 'REFER_FREE_PUSH . rest) ;; done
           (iter `(REFER_FREE_PUSH_REFER_FREE_PUSH ,n ,@rest))]
          [('NUMBER_ADD 'PUSH . rest)
           (iter `(NUMBER_ADD_PUSH ,@rest))]
          [('PUSH 'CONSTANT . rest)
           (iter `(PUSH_CONSTANT ,@rest))]
          [('PUSH 'FRAME . rest)
           (iter `(PUSH_FRAME ,@rest))]
          ;; N.B.
          ;; compiled pass3/$asm code has list '(CONSTANT_PUSH PUSH FRAME), ignore it.
          [((and x (not 'CONSTANT_PUSH)) 'PUSH 'FRAME . rest)
           (iter `(, x PUSH_FRAME ,@rest))]
          [('CAR 'PUSH . rest);;done
           (iter `(CAR_PUSH ,@rest))]
          [('CDR 'PUSH . rest);;done
           (iter `(CDR_PUSH ,@rest))]
          [('SHIFT m n 'CALL o . rest)
           (iter `(SHIFT_CALL ,m ,n ,o ,@rest))]
          [('NOT 'TEST . rest)
           (iter `(NOT_TEST ,@rest))]
          [('REFER_GLOBAL lib-id 'CALL n . rest)
           (iter `(REFER_GLOBAL_CALL ,lib-id ,n ,@rest))]
          [('REFER_LOCAL n 'PUSH . rest) ;; done
           (iter `(REFER_LOCAL_PUSH ,n ,@rest))]
          [else
           (cons (car s) (iter (cdr s)))])]))
    (iter sexp))])

(define (compile-partial sexp)
  (let1 ss (pass1/expand sexp)
    (vector->list
     (pass4/fixup-labels
      (list->vector
       (merge-insn
         (pass3
          (and (pass2/optimize
           (pass1/sexp->iform ss '() #f) '())))))))))

;; todo local macro
(define-macro (pass4/fixup-labels-clollect insn)
  `(begin
     (vector-set! ret j ,insn)
     (vector-set! ret (+ j 1) (vector-ref v (+ i 1)))
     (loop (+ i 2) (+ j 2))))

(define-macro (pass4/fixup-labels-clollect2 insn)
  `(begin
     (vector-set! ret j ,insn)
     (vector-set! ret (+ j 1) (vector-ref v (+ i 1)))
     (vector-set! ret (+ j 2) (vector-ref v (+ i 2)))
     (loop (+ i 3) (+ j 3))))



(define-macro (pass4/fixup-labels-clollect3 insn)
  `(begin
     (vector-set! ret j ,insn)
     (vector-set! ret (+ j 1) (vector-ref v (+ i 1)))
     (vector-set! ret (+ j 2) (vector-ref v (+ i 2)))
     (vector-set! ret (+ j 3) (vector-ref v (+ i 3)))
     (loop (+ i 4) (+ j 4))))


(define-macro (pass4/fixup-labels-insn insn)
  `(let1 label (hashtable-ref labels(vector-ref code (+ i 1)) #f)
     (cond
      [label
       (vector-set! code i ,insn)
       (vector-set! code (+ i 1) (- label i 1)) ;; jump point
       (loop (+ i 2))]
      [else
       (loop (+ i 1))])))

(define-macro (pass4/fixup-labels-insn2 insn)
  `(let1 label (hashtable-ref labels(vector-ref code (+ i 2)) #f)
     (cond
      [label
       (vector-set! code i ,insn)
       (vector-set! code (+ i 2) (- label i 2)) ;; jump point
       (loop (+ i 3))]
      [else
       (loop (+ i 1))])))


(define-macro (pass4/fixup-labels-insn3 insn)
  `(let1 label (hashtable-ref labels(vector-ref code (+ i 3)) #f)
     (cond
      [label
       (vector-set! code i ,insn)
       (vector-set! code (+ i 3) (- label i 3)) ;; jump point
       (loop (+ i 4))]
      [else
       (loop (+ i 1))])))


(cond-expand
 [vm?
  ;; moved to CompilerProcedures.cpp
  ;; N.B. this procedure is still required by vm.scm
  (define (peephole-optimization v)
    (let ([len (vector-length v)])
      (let loop ([i 0])
        (if (= i len)
            '()
            (let1 insn (vector-ref v i)
              (cond
               [(or (eq? insn 'LOCAL_JMP)
                    (and (eq? insn 'FRAME) (number? (vector-ref v (+ i 1))))) ;; avoid compile-instruction
                (let* ([offset (+ (vector-ref v (+ i 1)) 1)]
                       [destination-index (+ i offset)])
                  (cond
                   [(eq? (vector-ref v destination-index) 'LOCAL_JMP)
                    (vector-set! v (+ i 1) (+ offset (vector-ref v (+ destination-index 1))))]
                   [(eq? (vector-ref v destination-index) 'RETURN)
                    (vector-set! v i 'RETURN)
                    (vector-set! v (+ i 1) (vector-ref v (+ destination-index 1)))
                    ]
                    ))]
               [(and (memq insn '(TEST BRANCH_NOT_GE BRANCH_NOT_GT BRANCH_NOT_LT BRANCH_NOT_LE BRANCH_NOT_NUMBER_EQUAL BRANCH_NOT_NULL))
                     (number? (vector-ref v (+ i 1))))
                (let* ([offset (+ (vector-ref v (+ i 1)) 1)]
                       [destination-index (+ i offset)])
                  (when (or (eq? (vector-ref v destination-index) 'TEST) (eq? (vector-ref v destination-index) 'LOCAL_JMP))
                    (vector-set! v (+ i 1) (+ offset (vector-ref v (+ destination-index 1)))))
                    )])
              (loop (+ i 1)))))))

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
               [(eq? insn 'CONSTANT_BRANCH_NOT_LE)                  (pass4/fixup-labels-clollect 'CONSTANT_BRANCH_NOT_LE)]
               [(eq? insn 'BRANCH_NOT_NULL)                  (pass4/fixup-labels-clollect 'BRANCH_NOT_NULL)]
               [(eq? insn 'BRANCH_NOT_NUMBER_EQUAL)                  (pass4/fixup-labels-clollect 'BRANCH_NOT_NUMBER_EQUAL)]
               [(eq? insn 'BRANCH_NOT_LE)                  (pass4/fixup-labels-clollect 'BRANCH_NOT_LE)]
               [(eq? insn 'BRANCH_NOT_LT)                  (pass4/fixup-labels-clollect 'BRANCH_NOT_LT)]
               [(eq? insn 'BRANCH_NOT_GE)                  (pass4/fixup-labels-clollect 'BRANCH_NOT_GE)]
               [(eq? insn 'BRANCH_NOT_GT)                  (pass4/fixup-labels-clollect 'BRANCH_NOT_GT)]
               [(eq? insn 'BRANCH_NOT_EQ)                  (pass4/fixup-labels-clollect 'BRANCH_NOT_EQ)]
               [(eq? insn 'BRANCH_NOT_EQV)                  (pass4/fixup-labels-clollect 'BRANCH_NOT_EQV)]
               [(eq? insn 'BRANCH_NOT_EQUAL)                  (pass4/fixup-labels-clollect 'BRANCH_NOT_EQUAL)]
               [(eq? insn 'NOT_TEST)              (pass4/fixup-labels-clollect 'NOT_TEST)]
               [(eq? insn 'REFER_LOCAL_PUSH_CONSTANT_BRANCH_NOT_LE) (pass4/fixup-labels-clollect3 'REFER_LOCAL_PUSH_CONSTANT_BRANCH_NOT_LE)]
               [(eq? insn 'REFER_LOCAL_PUSH_CONSTANT_BRANCH_NOT_GE) (pass4/fixup-labels-clollect3 'REFER_LOCAL_PUSH_CONSTANT_BRANCH_NOT_GE)]
               [(eq? insn 'REFER_LOCAL_PUSH_CONSTANT_BRANCH_NOT_NUMBER_EQUAL) (pass4/fixup-labels-clollect3 'REFER_LOCAL_PUSH_CONSTANT_BRANCH_NOT_NUMBER_EQUAL)]
               [(eq? insn 'REFER_LOCAL_BRANCH_NOT_LT) (pass4/fixup-labels-clollect2 'REFER_LOCAL_BRANCH_NOT_LT)]
               [(eq? insn 'REFER_LOCAL_BRANCH_NOT_NULL) (pass4/fixup-labels-clollect2 'REFER_LOCAL_BRANCH_NOT_NULL)]
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
               [(eq? insn 'CONSTANT_BRANCH_NOT_LE)        (pass4/fixup-labels-insn 'CONSTANT_BRANCH_NOT_LE)]
               [(eq? insn 'BRANCH_NOT_NUMBER_EQUAL)                  (pass4/fixup-labels-insn 'BRANCH_NOT_NUMBER_EQUAL)]
               [(eq? insn 'BRANCH_NOT_NULL)                  (pass4/fixup-labels-insn 'BRANCH_NOT_NULL)]
               [(eq? insn 'BRANCH_NOT_LE)                  (pass4/fixup-labels-insn 'BRANCH_NOT_LE)]
               [(eq? insn 'BRANCH_NOT_GE)                  (pass4/fixup-labels-insn 'BRANCH_NOT_GE)]
               [(eq? insn 'BRANCH_NOT_LT)                  (pass4/fixup-labels-insn 'BRANCH_NOT_LT)]
               [(eq? insn 'BRANCH_NOT_GT)                  (pass4/fixup-labels-insn 'BRANCH_NOT_GT)]
               [(eq? insn 'BRANCH_NOT_EQ)                  (pass4/fixup-labels-insn 'BRANCH_NOT_EQ)]
               [(eq? insn 'BRANCH_NOT_EQV)                  (pass4/fixup-labels-insn 'BRANCH_NOT_EQV)]
               [(eq? insn 'BRANCH_NOT_EQUAL)                  (pass4/fixup-labels-insn 'BRANCH_NOT_EQUAL)]
               [(eq? insn 'NOT_TEST)              (pass4/fixup-labels-insn 'NOT_TEST)]
               [(eq? insn 'REFER_LOCAL_PUSH_CONSTANT_BRANCH_NOT_LE) (pass4/fixup-labels-insn3 'REFER_LOCAL_PUSH_CONSTANT_BRANCH_NOT_LE)]
               [(eq? insn 'REFER_LOCAL_PUSH_CONSTANT_BRANCH_NOT_GE) (pass4/fixup-labels-insn3 'REFER_LOCAL_PUSH_CONSTANT_BRANCH_NOT_GE)]
               [(eq? insn 'REFER_LOCAL_PUSH_CONSTANT_BRANCH_NOT_NUMBER_EQUAL) (pass4/fixup-labels-insn3 'REFER_LOCAL_PUSH_CONSTANT_BRANCH_NOT_NUMBER_EQUAL)]
               [(eq? insn 'REFER_LOCAL_BRANCH_NOT_NULL) (pass4/fixup-labels-insn2 'REFER_LOCAL_BRANCH_NOT_NULL)]
               [(eq? insn 'REFER_LOCAL_BRANCH_NOT_LT) (pass4/fixup-labels-insn2 'REFER_LOCAL_BRANCH_NOT_LT)]
               [(eq? insn 'FRAME)                 (pass4/fixup-labels-insn 'FRAME)]
               [(eq? insn 'PUSH_FRAME)            (pass4/fixup-labels-insn 'PUSH_FRAME)]
               [else (loop (+ i 1))]))])))
      (peephole-optimization code)
      code
    ))]
 [else #t])


;; We call this from eval.
(define (compile-w/o-halt sexp)
  (pass4-w/o-halt
   (merge-insn
    (pass3 (pass2/optimize (pass1/sexp->iform (pass1/expand sexp) '() #t) '())))))


(define (compile sexp)
  (pass4 (merge-insn
          ;; default is tail context == #t
          (pass3 (let1 x (pass2/optimize (pass1/sexp->iform (pass1/expand sexp) '() #t) '())
                   x)
                 ))))

(define (compile-no-optimize sexp)
  (pass4 (pass3 (pass1/sexp->iform (pass1/expand sexp) '() #t))))

(cond-expand
 [vm-outer?
  (define (main args)
    (if (= (length args) 2)
        (let1 port (open-string-input-port (second args))
          (write (compile (read port))))))
  (main (command-line))]
 [mosh
  #f]
 [else
  (define (main args)
    (if (= (length args) 2)
        (let1 port (open-string-input-port (second args))
          (write (compile (read port))))))])
