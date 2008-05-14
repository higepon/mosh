; library.scm - main library
;
;   Copyright (c) 2008  Higepon(Taro Minowa)  <higepon@users.sourceforge.jp>
;
;   Redistribution and use in source and binary forms, with or without
;   modification, are permitted provided that the following conditions
;   are met:
;
;   1. Redistributions of source code must retain the above copyright
;      notice, this list of conditions and the following disclaimer.
;
;   2. Redistributions in binary form must reproduce the above copyright
;      notice, this list of conditions and the following disclaimer in the
;      documentation and/or other materials provided with the distribution.
;
;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;
;  $Id$

; document is written at manual.scm
(define-macro (receive . args)
  `(call-with-values (lambda () ,(cadr args)) (lambda ,(car args) ,@(cddr args))))

; document is written at manual.scm
(define-macro (acond . clauses)
  (if (null? clauses)
      '()
      (let ((cl1 (car clauses))
            (sym (gensym)))
        `(let ((,sym ,(car cl1)))
           (if ,sym
               (let ((it ,sym)) ,@(cdr cl1))
               (acond ,@(cdr clauses)))))))

(define-macro (define-doc . more)
  `())

; ==============================================================================================================================================================
;;; Base library.
;;; R6RS chapter 11.

;; <p>The eqv? procedure defines a useful equivalence relation on objects.
;; <p>Briefly, it returns #t if obj1 and obj2 should normally be regarded as the same object and #f otherwise.</p>
;; <p>The eqv? procedure returns #t if one of the following holds:</p>
;; <p>Obj1 and obj2 are both booleans and are the same according to the boolean=? procedure.</p>
;; <p>Obj1 and obj2 are both symbols and are the same according to the symbol=? procedure.</p>
;; <p>Obj1 and obj2 are both exactnumber objects and are numerically equal.</p>
;; <p>Obj1 and obj2 are both inexactnumber objects, are numerically equal, and yield the same results (in the sense of eqv?) when passed as arguments to any other procedure that can be defined as a finite composition of Scheme's standard arithmetic procedures.</p>
;; <p>Obj1 and obj2 are both characters and are the same character according to the char=? procedure .</p>
;; <p>Both obj1 and obj2 are the empty list.</p>
;; <p>Obj1 and obj2 are objects such as pairs, vectors, bytevectors , strings, hashtables, records, ports, or hashtables  that refer to the same locations in the store.</p>
;; <p>Obj1 and obj2 are record-type descriptors that are specified to be eqv? in library.</p>
;; <p>The eqv? procedure returns #f if one of the following holds:</p>
;; <p>Obj1 and obj2 are of different types.</p>
;; <p>Obj1 and obj2 are booleans for which the boolean=? procedure returns #f.</p>
;; <p>Obj1 and obj2 are symbols for which the symbol=? procedure returns #f.</p>
;; <p>One of obj1 and obj2 is an exact number object but the other is an inexact number object.</p>
;; <p>Obj1 and obj2 are rational number objects for which the = procedure returns #f.</p>
;; <p>Obj1 and obj2 yield different results (in the sense of eqv?) when passed as arguments to any other procedure that can be defined as a finite composition of Scheme's standard arithmetic procedures.</p>
;; <p>Obj1 and obj2 are characters for which the char=? procedure returns #f.</p>
;; <p>One of obj1 and obj2 is the empty list, but the other is not.</p>
;; <p>Obj1 and obj2 are objects such as pairs, vectors, bytevectors (library chapter on "Bytevectors"), strings, records (library chapter on "Records"), ports, or hashtables that refer to distinct locations.</p>
;; <p>Obj1 and obj2 are pairs, vectors, strings, or records, or hashtables, where the applying the same accessor (i.e. car, cdr, vector-ref, string-ref, or record accessors) to both yields results for which eqv? returns #f.</p>
;; <p>Obj1 and obj2 are procedures that would behave differently (return different values or have different side effects) for some arguments.</p>
;; .form (eqv? obj1 obj2)
;; .returns #t if equal
;; .example (eqv? 'a 'a)                             =>  #t
;; .example (eqv? 'a 'b)                             =>  #f
;; .example (eqv? 2 2)                               =>  #t
;; .example (eqv? '() '())                           =>  #t
;; .example (eqv? 100000000 100000000)               =>  #t
;; .example (eqv? (cons 1 2) (cons 1 2))             =>  #f
;; .example (eqv? (lambda () 1)
;;                (lambda () 2))                    =>  #f
;; .example (eqv? #f 'nil)                          =>  #f
(define-doc (eqv?) ...)

;; A lambda expression evaluates to a procedure.
;; <p>The environment in effect when the lambda expression is evaluated is remembered as part of the procedure.</p>
;; <p>When the procedure is later called with some arguments, the environment in which the lambda expression was evaluated is extended by binding the variables in the parameter list to fresh locations, and the resulting argument values are stored in those locations.</p>
;; <p>Then, the expressions in the body of the lambda expression are evaluated sequentially in the extended environment. The results of the last expression in the body are returned as the results of the procedure call.</p>
;; .form (lambda [formals] [body])
;; .returns A procedure
(define-doc (lambda) ...)

;; Quasiquote
;; .form (quasiquote [qq template])
;; .reference "Quasiquote" "quasiquote" "http://www.r6rs.org/final/html/r6rs/r6rs-Z-H-14.html#node_idx_768"
(define-doc (quasiquote) ...)

;; Evaluates to the datum value represented by [datum].
;; .form (quote [datum])
;; .returns quoted object
(define-doc (quote) ...)

;; <p>An if expression is evaluated as follows: first, [test] is evaluated.</p>
;; <p>If it yields a true value, then [consequent] is evaluated and its values are returned.Otherwise <alternate> is evaluated and its values are returned.</p>
;; <p>If <test> yields #f and no [alternate] is specified, then the result of the expression is unspecified.</p>
;; .form (if [test] [consequent] [alternate]) (if [test] [consequent])
(define-doc (if) ...)

;; <p>(let [bindings] [body])</p>
;; <p>Syntax: [Bindings] must have the form</p>
;; <p>  (([variable1] [init1]) ...),</p>
;; <p>where each [init] is an expression.</p>
;; <p>Any variable must not appear more than once in the [variable]s.</p>
;; .form (let [bindings] [body])
;; .example (let ((x 2) (y 3))\
;;            (* x y)) =>  6
;; .example (let ((x 2) (y 3))\
;;            (let ((x 7)\
;;              (z (+ x y)))\
;;                (* z x)))  => 35
(define-doc (let) ...)

;; <p>(let* [bindings] [body])</p>
;; <p>Syntax: [Bindings] must have the form</p>
;; <p>(([variable1] [init1]) ...),</p>
;; <p>Semantics: The let* form is similar to let, but the [init]s are evaluated and bindings created sequentially from left to right, with the regionof each binding including the bindings to its right as well as [body].</p>
;; <p>Thus the second [init] is evaluated in an environment in which the first binding is visible and initialized, and so on.</p>
;; .form (let* [bindings] [body])
;; .example (let ((x 2) (y 3))\
;;            (let* ((x 7)\
;;                  (z (+ x y)))\
;;                    (* z x))) =>70
(define-doc (let*) ...)

;; <p>(letrec [bindings] [body])</p>
;; <p>Syntax: [Bindings] must have the form</p>
;; <p>(([variable1] [init1]) ...),</p>
;; <p>Any variable must not appear more than once in the [variable]s.</p>
;; <p>Semantics: The [variable]s are bound to fresh locations, the [init]s are evaluated in the resulting environment (in some unspecified order), each [variable] is assigned to the result of the corresponding [init], the [body] is evaluated in the resulting environment, and the values of the last expression in <body> are returned.</p>
;; <p>Each binding of a [variable] has the entire letrec expression as its region, making it possible to define mutually recursive procedures.</p>
;; .form (letrec [bindings] [body])
(define-doc (letrec) ...)

;; Returns #t if obj is #f, and returns #f otherwise.
;; .form (not obj)
;; .returns #t if obj is #f, and returns #f otherwise.
;; .example (not #t)           =>  #f
;; .example (not 3)            =>  #f
;; .example (not (list 3))     =>  #f
;; .example (not #f)           =>  #t
;; .example (not '())          =>  #f
;; .example (not (list))       =>  #f
;; .example (not 'nil)         =>  #f
(define-doc (not) ...)

;; If there are no [test]s, #f is returned. Otherwise, the [test] expressions are evaluated from left to right until a [test] returns a true value val or the last [test] is reached.
;; <p>In the former case, the or expression returns val without evaluating the remaining expressions.
;; In the latter case, the last expression is evaluated and its values are returned.</p>
;; .form (or [test1] ...)
;; .example (or (= 2 2) (> 2 1))                    =>  #t
;; .example (or (= 2 2) (< 2 1))                    =>  #t
;; .example (or #f #f #f)                           =>  #f
;; .example (or '(b c) (/ 3 0))                     =>  (b c)
(define-doc (or) ...)

;; <p>[Expression] is evaluated, and the resulting value is stored in the location to which [variable] is bound. </p><p>[Variable] must be bound either in some regionenclosing the set! expression or at the top level.</p>
;; .form (set! [variable] [expression])
;; .returns unspecified
(define-doc (set!) ...)

;; Returns #t if obj is a symbol, otherwise returns #f.
;; .form (symbol? obj)
;; .returns #t if obj is a symbol, otherwise returns #f.
(define-doc (symbol?) ...)

;; Returns the number of elements in vector as an exact integer object.
;; .form (vector-length vector)
;; .returns The number of elements in vector as an exact integer object.
(define-doc (vector-length) ...)

;; Returns the contents of elementk of vector.
;; .pre-condition K must be a valid index of vector.
;; .form (vector-ref vector k)
;; .returns The contents of elementk of vector.
;; .example (vector-ref '#(1 1 2 3 5 8 13 21) 5) =>  8
(define-doc (vector-ref) ...)

;; stores obj in element k of vector, and returns unspecified values.
;; .pre-condition K must be a valid index of vector.
;; .form (vector-set! vector k obj)
;; .returns unspecified
;; .example <pre>(let ((vec (vector 0 '(2 2 2 2) "Anna")))\
;;                    (vector-set! vec 1 '("Sue" "Sue"))\
;;                    vec)\
;;                =>  #(0 ("Sue" "Sue") "Anna")</pre>
(define-doc (vector-set!) ...)

;; If there are no [test]s, #t is returned. Otherwise, the [test] expressions are evaluated from left to right until a [test] returns #f or the last [test] is reached.
;; <p>In the former case, the and expression returns #f without evaluating the remaining expressions.
;; In the latter case, the last expression is evaluated and its values are returned.</p>
;; .form (and [test1] ...)
(define-doc (and) ...)


;; The apply procedure calls proc with the elements of the list (append (list arg1 ...) rest-args) as the actual arguments.
;; .pre-condition Rest-args must be a list. Proc should accept n arguments, where n is number of args plus the length of rest-args.
;; .form (apply proc arg1 ... rest-args)
;; .returns The apply procedure calls proc with the elements of the list (append (list arg1 ...) rest-args) as the actual arguments.
(define-doc (apply) ...)

;; <p>The [begin] keyword has two different roles, depending on its context:</p>
;; <p>It may appear as a form in a [body] , [library body] , or [top-level body], or directly nested in a begin form that appears in a body.
;; In this case, the begin form must have the shape specified in the first header line.</p>
;; <p>This use of begin acts as a splicing form the forms inside the [body] are spliced into the surrounding body, as if the begin wrapper were not actually present.</p>
;; <p>A begin form in a [body] or [library body] must be non-empty if it appears after the first [expression] within the body.
;; It may appear as an ordinary expression and must have the shape specified in the second header line.</p>
;; <p>In this case, the [expression]s are evaluated sequentially from left to right, and the values of the last [expression] are returned.
;; This expression type is used to sequence side effects such as assignments or input and output.</p>
;; .form (begin [form] ...) (begin [expression] [expression] ...)
(define-doc (begin) ...)

;; <p>The procedure call-with-current-continuation (which is the same as the procedure call/cc) packages the current continuation as an "escape procedure"and passes it as an argument to proc.</p>
;; <p>The escape procedure is a Scheme procedure that, if it is later called, will abandon whatever continuation is in effect at that later time and will instead reinstate the continuation that was in effect when the escape procedure was created.</p>
;; .form (call-with-current-continuation proc)
(define-doc (call-with-current-continuation) ...)

;; <p>case expression is evaluated as follows.</p>
;; <p>[Key] is evaluated and its result is compared using eqv? against the data represented by the [datum]s of each [case clause] in turn, proceeding in order from left to right through the set of clauses.</p>
;; <p>If the result of evaluating [key] is equivalent to a datum of a [case clause], the corresponding [expression]s are evaluated from left to right and the results of the last expression in the [case clause] are returned as the results of the case expression. Otherwise, the comparison process continues.</p>
;; <p>If the result of evaluating [key] is different from every datum in each set, then if there is an else clause its expressions are evaluated and the results of the last are the results of the case expression; otherwise the case expression returns unspecified values.</p>
;; .form (case [key] [case clause1] [case clause2] ...)
;; .example <pre>(case (* 2 3)\
;;                  ((2 3 5 7) 'prime)\
;;                  ((1 4 6 8 9) 'composite))  =>  composite</pre>
(define-doc (case) ...)

;; <p>A cond expression is evaluated by evaluating the [test] expressions of successive [cond clause]s in order until one of them evaluates to a true value.</p>
;; <p>When a [test] evaluates to a true value, then the remaining [expression]s in its [cond clause] are evaluated in order, and the results of the last [expression] in the [cond clause] are returned as the results of the entire cond expression.</p>
;; <p>If the selected [cond clause] contains only the [test] and no [expression]s, then the value of the [test] is returned as the result.
;; If the selected [cond clause] uses the => alternate form, then the [expression] is evaluated. Its value must be a procedure.</p>
;; <p>This procedure should accept one argument; it is called on the value of the [test] and the values returned by this procedure are returned by the cond expression.</p>
;; <p>If all [test]s evaluate to #f, and there is no else clause, then the conditional expression returns unspecified values; if there is an else clause, then its [expression]s are evaluated, and the values of the last one are returned.</p>
;; .form (cond [cond clause1] [cond clause2] ...)
;; .example <pre>(cond ((> 3 2) 'greater)\
;;                 ((< 3 2) 'less)) =>  greater</pre>
;; .example <pre>(cond ((> 3 3) 'greater)\
;;                 ((< 3 3) 'less)\
;;                 (else 'equal))   =>  equal</pre>
;; .example <pre>(cond ('(1 2 3) => cadr)\
;;                 (else #f))       =>  2</pre>
(define-doc (cond) ...)

;; The define form is used to create variable bindings and may appear anywhere other definitions may appear.
;; .form (define [variable] [expression]) (define [variable]) (define ([variable] [formals]) [body]) (define ([variable] . [formal]) [body])
(define-doc (define) ...)

;; The define-macro form is used to create traditional macro.
;; .form (define-macro (name . args) body)
(define-doc (define-macro) ...)

;; Returns the product of their arguments.
;; .form (* z1 ...)
;; .returns The product of their arguments.
(define-doc (*) ...)

;; Returns the sum of their arguments.
;; .form (+ z1 ...)
;; .returns The sum of their arguments.
(define-doc (+) ...)

;; With two or more arguments, this procedures returns the difference of its arguments, associating to the left. With one argument, however, it returns the additive inverse of its argument.
;; .form (- z1 z2 ...) (- z)
;; .returns With two or more arguments, this procedures returns the difference of its arguments, associating to the left. With one argument, however, it returns the additive inverse of its argument.
(define-doc (-) ...)

;; Returns #t if its arguments are monotonically increasing and #f otherwise.
;; .form (< x1 x2 x3 ...)
;; .returns #t if its arguments are monotonically increasing and #f otherwise.
(define-doc (<) ...)

;; Returns #t if its arguments are monotonically nondecreasing and #f otherwise.
;; .form (<= x1 x2 x3 ...)
;; .returns #t if its arguments are monotonically nondecreasing and #f otherwise.
(define-doc (<=) ...)

;; Returns #t if its arguments are equal and #f otherwise.
;; .form (= x1 x2 x3 ...)
;; .returns #t if its arguments are equal and #f otherwise.
(define-doc (=) ...)

;; Returns #t if its arguments are monotonically decreasing and #f otherwise.
;; .form (> x1 x2 x3 ...)
;; .returns #t if its arguments are monotonically decreasing and #f otherwise.
(define-doc (>) ...)

;; Returns #t if its arguments are monotonically increasing and #f otherwise.
;; .form (> x1 x2 x3 ...)
;; .returns #t if its arguments are monotonically increasing and #f otherwise.
(define-doc (>=) ...)

;; Returns a newly allocated vector whose elements contain the given arguments. Analogous to list.
;; .form (vector obj ...)
;; .returns A newly allocated vector whose elements contain the given arguments. Analogous to list.
(define-doc (vector) ...)

;; Returns #t if obj is a pair, and otherwise returns #f.
;; .form (pair? obj)
;; .returns #t if obj is a pair, and otherwise returns #f.
(define-doc (pair?) ...)

;; Delivers all of its arguments to its continuation.
;; .form (values obj ...)
;; .returns Delivers all of its arguments to its continuation.
;; .internal-references "Base Library" "call-with-values"
(define-doc (values) ...)

;; This error procedure will be replaced with R6RS (error)
;; .form (error message)
;; .returns This error procedure will be replaced with R6RS (error)
(define-doc (error) ...)

;; Returns the name of symbol as an immutable string.
;; .form (symbol->string symbol)
;; .returns Returns the name of symbol as an immutable string.
(define-doc (symbol->string) ...)

;; Returns #t if obj is either #t or #f and returns #f otherwise.
;; .form (boolean? obj)
;; .returns #t if obj is either #t or #f and returns #f otherwise.
(define-doc (boolean?) ...)

;; <p>The eq? predicate is similar to eqv? except that in some cases it is capable of discerning distinctions finer than those detectable by eqv?.</p>
;; <p>The eq? and eqv? predicates are guaranteed to have the same behavior on symbols, booleans, the empty list, pairs, procedures, non-empty strings, bytevectors, and vectors, and records.</p>
;; .returns #t if eq?
;; .example (eq? 'a 'a) =>  #t
;; .example (eq? '(a) '(a))                         =>  #f (unspecified on R6RS)
;; .example (eq? (list 'a) (list 'a))               =>  #f
;; .example (eq? "a" "a")                           =>  #f (unspecified on R6RS)
;; .example (eq? "" "")                             =>  #f (unspecified on R6RS)
;; .example (eq? '() '())                           =>  #t
;; .example (eq? 2 2)                               =>  #t (unspecified on R6RS)
;; .example (eq? #\A #\A)                           =>  #t (unspecified on R6RS)
;; .example (eq? car car)                           =>  #t
;; .example <pre>(let ((n (+ 2 3)))\
;;                 (eq? n n))                       =>  #t (unspecified on R6RS)</pre>
;; .example <pre>(let ((x '(a)))\
;;                 (eq? x x))                       =>  #t (unspecified on R6RS)</pre>
;; .example <pre>(let ((x '#()))\
;;                  (eq? x x))                      =>  #t (unspecified on R6RS)</pre>
;; .example <pre>(let ((p (lambda (x) x)))\
;;                 (eq? p p))                       =>  #t (unspecified on R6RS)</pre>
(define-doc (eq?) ...)

;; Returns #t if obj is a vector. Otherwise the procedure returns #f.
;; .form (vector? obj)
;; .returns #t if obj is a vector. Otherwise the procedure returns #f.
(define-doc (vector?) ...)

; used internal
(define-doc (sys-display) ...)

;; The equal? predicate returns #t if and only if the unfoldings of its arguments into regular trees are equal as ordered trees.
;; .form (equal? obj1 obj2)
;; .returns #t if and only if the unfoldings of its arguments into regular trees are equal as ordered trees.
;; .example (equal? 'a 'a)                                 =>  #t
;; .example (equal? '(a) '(a))                             =>  #t
;; .example (equal? '(a (b) c) '(a (b) c))                 =>  #t
;; .example (equal? "abc" "abc")                           =>  #t
;; .example (equal? 2 2)                                   =>  #t
;; .example (equal? (make-vector 5 'a) (make-vector 5 'a)) =>  #t
(define-doc (equal?) ...)

;; Returns #t if obj is a character, otherwise returns #f.
;; .form (char? obj)
;; .returns #t if obj is a character, otherwise returns #f.
(define-doc (char?) ...)

;; Returns #t if all given charctors are same charctor.
;; .form (char=? char1 char2 char3 ...)
;; .returns #t if all given charctors are same charctor.
(define-doc (char=?) ...)

;; Given a character, char->integer returns its Unicode scalar value as an exact integer object.
;; .form (char->integer char)
;; .returns Given a character, char->integer returns its Unicode scalar value as an exact integer object.
(define-doc (char->integer) ...)

;; For a Unicode scalar value sv, integer->char returns its associated character.
;; .form (integer->char sv)
;; .returns For a Unicode scalar value sv, integer->char returns its associated character.
(define-doc (integer->char) ...)

;; (not implemented yet) Returns #t if obj is a list, #f otherwise. By definition, all lists are chains of pairs that have finite length and are terminated by the empty list.
;; .form (list? obj)
;; .returns #t if obj is a list, #f otherwise. By definition, all lists are chains of pairs that have finite length and are terminated by the empty list.
(define-doc (list?) ...)

;; Returns a newly allocated pair whose car is obj1 and whose cdr is obj2.
;; The pair is guaranteed to be different (in the sense of eqv?) from every existing object.
;; .returns A newly allocated pair whose car is obj1 and whose cdr is obj2.
;; .form (cons obj1 obj2)
;; .example (cons 'a '())                   =>  (a)
;; .example (cons '(a) '(b c d))            =>  ((a) b c d)
;; .example (cons "a" '(b c))               =>  ("a" b c)
;; .example (cons 'a 3)                     =>  (a . 3)
;; .example (cons '(a b) 'c)                =>  ((a b) . c)
(define-doc (cons) ...)

;; Returns the contents of the car field of pair.
;; .returns The contents of the car field of pair.
;; .form (car pair)
;; .example (car '(a b c))                  =>  a
;; .example (car '((a) b c d))              =>  (a)
;; .example (car '(1 . 2))                  =>  1
;; .example (car '())                       &assertion exception (not implemented yet)
(define-doc (car) ...)

;; Returns the contents of the cdr field of pair.
;; .returns The contents of the cdr field of pair.
;; .form (cdr pair)
;; .example (cdr '((a) b c d))              =>  (b c d)
;; .example (cdr '(1 . 2))                  =>  2
;; .example (cdr '())                       &assertion exception (not implemented yet)
(define-doc (cdr) ...)

;; Returns #t if obj is the empty list, #f otherwise.
;; .returns #t if obj is the empty list, #fotherwise.
;; .form (null? obj)
(define-doc (null?) ...)

;; <p>Returns #t if the strings are the same length and contain the same characters in the same positions.</p>
;; <p>Otherwise, the string=? procedure returns #f.Returns #t if obj is a string, otherwise returns #f.</p>
;; .form (string=? string1 string2 string3 ...)
;; .returns Returns #t if the strings are the same length and contain the same characters in the same positions.
(define-doc (string=?) ...)

;; Returns a newly allocated string of length k.
;; If char is given, then all elements of the string are initialized to char, otherwise the contents of the string are unspecified.
;; .form (make-string k &optional char)
;; .returns allocated string of length k.
(define-doc (make-string) ...)

;; Returns #t if obj is a string, otherwise returns #f.
;; .form (string? obj)
;; .returns #t if obj is a string, otherwise returns #f. 
(define-doc (string?) ...)

;; Returns the number of characters in the given string as an exact integer object.
;; .returns the number of characters in the given string as an exact integer object.
;; .form (string-length string)
(define-doc (string-length) ...)

;; character at index k in string
;; .pre-condition K must be a valid index of string.
;; .returns character at index k in string
;; .form (string-ref string k)
(define-doc (string-ref) ...)

;; stores char in element k of string.
;; .returns unspecified
;; .form (string-set! string k char)
(define-doc (string-set!) ...)

;; Returns the symbol whose name is string.
;; .form (string->symbol string)
;; .example (eq? 'JollyWog\
;;     (string->symbol\
;;       (symbol->string 'JollyWog))) => #t
(define-doc (string->symbol) ...)

;; Returns a number by the given string.
;; .form (string->number string)
;; .returns number
(define-doc (string->number) ...)

;; Returns a newly allocated string whose characters form the concatenation of the given strings.
;; .form (string-append string ...)
;; .returns A newly allocated string whose characters form the concatenation of the given strings.
(define-doc (string-append) ...)

;; Splits string by splitter and returns a list of strings.
;; splitter can be a character.
;; .form (string-split string splitter)
;; .returns a list of splitted strings.
(define-doc (string-split) ...)

;; Takes a number object and returns as a string an external representation of the given number object.
;; .form (number->string z)
;; .returns Takes a number object and returns as a string an external representation of the given number object.
(define-doc (number->string) ...)

;; Takes a number object and a radix and returns as a string an external representation of the given number object in the given radix.
;; .form (number->string z radix)
;; .parameter radix must be 16. (2, 8, 10 is not implemented yet)
;; .returns Takes a number object and a radix and returns as a string an external representation of the given number object in the given radix.
(define-doc (number->string) ...)

;; return #t if obj is number object and #f otherwise.
;; .returns #t if obj is number object and #f otherwise
(define-doc (number? obj) ...)

;; returns (car (car p))
;; .pre-condition p is pair
;; .returns (car (car p))
(define (caar p) (car (car p)))

;; returns (cdr (car p))
;; .pre-condition p is pair
;; .returns (cdr (car p))
(define (cdar p) (cdr (car p)))

;; returns (car (cdr p))
;; .pre-condition p is pair
;; .returns (car (cdr p))
(define (cadr p) (car (cdr p)))

;; returns (cdr (cdr p))
;; .pre-condition p is pair
;; .returns (cdr (cdr p))
(define (cddr p) (cdr (cdr p)))

;; returns (car (car (car p)))
;; .pre-condition p is pair
;; .returns (car (car (car p)))
(define (caaar p) (car (car (car p))))

;; returns (cdr (car (car p)))
;; .pre-condition p is pair
;; .returns (cdr (car (car p)))
(define (cdaar p) (cdr (car (car p))))

;; returns (car (cdr (car p)))
;; .pre-condition p is pair
;; .returns (car (cdr (car p)))
(define (cadar p) (car (cdr (car p))))

;; returns (cdr (cdr (car p)))
;; .pre-condition p is pair
;; .returns (cdr (cdr (car p)))
(define (cddar p) (cdr (cdr (car p))))

;; returns (car (car (cdr p)))
;; .pre-condition p is pair
;; .returns (car (car (cdr p)))
(define (caadr p) (car (car (cdr p))))

;; returns (cdr (car (cdr p)))
;; .pre-condition p is pair
;; .returns (cdr (car (cdr p)))
(define (cdadr p) (cdr (car (cdr p))))

;; returns (car (cdr (cdr p)))
;; .pre-condition p is pair
;; .returns (car (cdr (cdr p)))
(define (caddr p) (car (cdr (cdr p))))

;; returns (cdr (cdr (cdr p)))
;; .pre-condition p is pair
;; .returns (cdr (cdr (cdr p)))
(define (cdddr p) (cdr (cdr (cdr p))))

;; returns (car (car (car (car p))))
;; .pre-condition p is pair
;; .returns (car (car (car (car p))))
(define (caaaar p) (car (car (car (car p)))))

;; returns (cdr (car (car (car p))))
;; .pre-condition p is pair
;; .returns (cdr (car (car (car p))))
(define (cdaaar p) (cdr (car (car (car p)))))

;; returns (car (cdr (car (car p))))
;; .pre-condition p is pair
;; .returns (car (cdr (car (car p))))
(define (cadaar p) (car (cdr (car (car p)))))

;; returns (cdr (cdr (car (car p))))
;; .pre-condition p is pair
;; .returns (cdr (cdr (car (car p))))
(define (cddaar p) (cdr (cdr (car (car p)))))

;; returns (car (car (cdr (car p))))
;; .pre-condition p is pair
;; .returns (car (car (cdr (car p))))
(define (caadar p) (car (car (cdr (car p)))))

;; returns (cdr (car (cdr (car p))))
;; .pre-condition p is pair
;; .returns (cdr (car (cdr (car p))))
(define (cdadar p) (cdr (car (cdr (car p)))))

;; returns (car (cdr (cdr (car p))))
;; .pre-condition p is pair
;; .returns (car (cdr (cdr (car p))))
(define (caddar p) (car (cdr (cdr (car p)))))

;; returns (cdr (cdr (cdr (car p))))
;; .pre-condition p is pair
;; .returns (cdr (cdr (cdr (car p))))
(define (cdddar p) (cdr (cdr (cdr (car p)))))

;; returns (car (car (car (cdr p))))
;; .pre-condition p is pair
;; .returns (car (car (car (cdr p))))
(define (caaadr p) (car (car (car (cdr p)))))

;; returns (cdr (car (car (cdr p))))
;; .pre-condition p is pair
;; .returns (cdr (car (car (cdr p))))
(define (cdaadr p) (cdr (car (car (cdr p)))))

;; returns (car (cdr (car (cdr p))))
;; .pre-condition p is pair
;; .returns (car (cdr (car (cdr p))))
(define (cadadr p) (car (cdr (car (cdr p)))))

;; returns (cdr (cdr (car (cdr p))))
;; .pre-condition p is pair
;; .returns (cdr (cdr (car (cdr p))))
(define (cddadr p) (cdr (cdr (car (cdr p)))))

;; returns (car (car (cdr (cdr p))))
;; .pre-condition p is pair
;; .returns (car (car (cdr (cdr p))))
(define (caaddr p) (car (car (cdr (cdr p)))))

;; returns (cdr (car (cdr (cdr p))))
;; .pre-condition p is pair
;; .returns (cdr (car (cdr (cdr p))))
(define (cdaddr p) (cdr (car (cdr (cdr p)))))

;; returns (car (cdr (cdr (cdr p))))
;; .pre-condition p is pair
;; .returns (car (cdr (cdr (cdr p))))
(define (cadddr p) (car (cdr (cdr (cdr p)))))

;; returns (cdr (cdr (cdr (cdr p))))
;; .pre-condition p is pair
;; .returns (cdr (cdr (cdr (cdr p))))
(define (cddddr p) (cdr (cdr (cdr (cdr p)))))

;; returns (car (car (car (car (car p)))))
;; .pre-condition p is pair
;; .returns (car (car (car (car (car p)))))
(define (caaaaar p) (car (car (car (car (car p))))))

;; returns (cdr (car (car (car (car p)))))
;; .pre-condition p is pair
;; .returns (cdr (car (car (car (car p)))))
(define (cdaaaar p) (cdr (car (car (car (car p))))))

;; returns (car (cdr (car (car (car p)))))
;; .pre-condition p is pair
;; .returns (car (cdr (car (car (car p)))))
(define (cadaaar p) (car (cdr (car (car (car p))))))

;; returns (cdr (cdr (car (car (car p)))))
;; .pre-condition p is pair
;; .returns (cdr (cdr (car (car (car p)))))
(define (cddaaar p) (cdr (cdr (car (car (car p))))))

;; returns (car (car (cdr (car (car p)))))
;; .pre-condition p is pair
;; .returns (car (car (cdr (car (car p)))))
(define (caadaar p) (car (car (cdr (car (car p))))))

;; returns (cdr (car (cdr (car (car p)))))
;; .pre-condition p is pair
;; .returns (cdr (car (cdr (car (car p)))))
(define (cdadaar p) (cdr (car (cdr (car (car p))))))

;; returns (car (cdr (cdr (car (car p)))))
;; .pre-condition p is pair
;; .returns (car (cdr (cdr (car (car p)))))
(define (caddaar p) (car (cdr (cdr (car (car p))))))

;; returns (cdr (cdr (cdr (car (car p)))))
;; .pre-condition p is pair
;; .returns (cdr (cdr (cdr (car (car p)))))
(define (cdddaar p) (cdr (cdr (cdr (car (car p))))))

;; returns (car (car (car (cdr (car p)))))
;; .pre-condition p is pair
;; .returns (car (car (car (cdr (car p)))))
(define (caaadar p) (car (car (car (cdr (car p))))))

;; returns (cdr (car (car (cdr (car p)))))
;; .pre-condition p is pair
;; .returns (cdr (car (car (cdr (car p)))))
(define (cdaadar p) (cdr (car (car (cdr (car p))))))

;; returns (car (cdr (car (cdr (car p)))))
;; .pre-condition p is pair
;; .returns (car (cdr (car (cdr (car p)))))
(define (cadadar p) (car (cdr (car (cdr (car p))))))

;; returns (cdr (cdr (car (cdr (car p)))))
;; .pre-condition p is pair
;; .returns (cdr (cdr (car (cdr (car p)))))
(define (cddadar p) (cdr (cdr (car (cdr (car p))))))

;; returns (car (car (cdr (cdr (car p)))))
;; .pre-condition p is pair
;; .returns (car (car (cdr (cdr (car p)))))
(define (caaddar p) (car (car (cdr (cdr (car p))))))

;; returns (cdr (car (cdr (cdr (car p)))))
;; .pre-condition p is pair
;; .returns (cdr (car (cdr (cdr (car p)))))
(define (cdaddar p) (cdr (car (cdr (cdr (car p))))))

;; returns (car (cdr (cdr (cdr (car p)))))
;; .pre-condition p is pair
;; .returns (car (cdr (cdr (cdr (car p)))))
(define (cadddar p) (car (cdr (cdr (cdr (car p))))))

;; returns (cdr (cdr (cdr (cdr (car p)))))
;; .pre-condition p is pair
;; .returns (cdr (cdr (cdr (cdr (car p)))))
(define (cddddar p) (cdr (cdr (cdr (cdr (car p))))))

;; returns (car (car (car (car (cdr p)))))
;; .pre-condition p is pair
;; .returns (car (car (car (car (cdr p)))))
(define (caaaadr p) (car (car (car (car (cdr p))))))

;; returns (cdr (car (car (car (cdr p)))))
;; .pre-condition p is pair
;; .returns (cdr (car (car (car (cdr p)))))
(define (cdaaadr p) (cdr (car (car (car (cdr p))))))

;; returns (car (cdr (car (car (cdr p)))))
;; .pre-condition p is pair
;; .returns (car (cdr (car (car (cdr p)))))
(define (cadaadr p) (car (cdr (car (car (cdr p))))))

;; returns (cdr (cdr (car (car (cdr p)))))
;; .pre-condition p is pair
;; .returns (cdr (cdr (car (car (cdr p)))))
(define (cddaadr p) (cdr (cdr (car (car (cdr p))))))

;; returns (car (car (cdr (car (cdr p)))))
;; .pre-condition p is pair
;; .returns (car (car (cdr (car (cdr p)))))
(define (caadadr p) (car (car (cdr (car (cdr p))))))

;; returns (cdr (car (cdr (car (cdr p)))))
;; .pre-condition p is pair
;; .returns (cdr (car (cdr (car (cdr p)))))
(define (cdadadr p) (cdr (car (cdr (car (cdr p))))))

;; returns (car (cdr (cdr (car (cdr p)))))
;; .pre-condition p is pair
;; .returns (car (cdr (cdr (car (cdr p)))))
(define (caddadr p) (car (cdr (cdr (car (cdr p))))))

;; returns (cdr (cdr (cdr (car (cdr p)))))
;; .pre-condition p is pair
;; .returns (cdr (cdr (cdr (car (cdr p)))))
(define (cdddadr p) (cdr (cdr (cdr (car (cdr p))))))

;; returns (car (car (car (cdr (cdr p)))))
;; .pre-condition p is pair
;; .returns (car (car (car (cdr (cdr p)))))
(define (caaaddr p) (car (car (car (cdr (cdr p))))))

;; returns (cdr (car (car (cdr (cdr p)))))
;; .pre-condition p is pair
;; .returns (cdr (car (car (cdr (cdr p)))))
(define (cdaaddr p) (cdr (car (car (cdr (cdr p))))))

;; returns (car (cdr (car (cdr (cdr p)))))
;; .pre-condition p is pair
;; .returns (car (cdr (car (cdr (cdr p)))))
(define (cadaddr p) (car (cdr (car (cdr (cdr p))))))

;; returns (cdr (cdr (car (cdr (cdr p)))))
;; .pre-condition p is pair
;; .returns (cdr (cdr (car (cdr (cdr p)))))
(define (cddaddr p) (cdr (cdr (car (cdr (cdr p))))))

;; returns (car (car (cdr (cdr (cdr p)))))
;; .pre-condition p is pair
;; .returns (car (car (cdr (cdr (cdr p)))))
(define (caadddr p) (car (car (cdr (cdr (cdr p))))))

;; returns (cdr (car (cdr (cdr (cdr p)))))
;; .pre-condition p is pair
;; .returns (cdr (car (cdr (cdr (cdr p)))))
(define (cdadddr p) (cdr (car (cdr (cdr (cdr p))))))

;; returns (car (cdr (cdr (cdr (cdr p)))))
;; .pre-condition p is pair
;; .returns (car (cdr (cdr (cdr (cdr p)))))
(define (caddddr p) (car (cdr (cdr (cdr (cdr p))))))

;; returns (cdr (cdr (cdr (cdr (cdr p)))))
;; .pre-condition p is pair
;; .returns (cdr (cdr (cdr (cdr (cdr p)))))
(define (cdddddr p) (cdr (cdr (cdr (cdr (cdr p))))))

;; (define (gen-cxxr n)
;;   (define (association lst1 lst2)
;;     (fold-right (lambda (x y) (append (map (cut cons x <>) lst2) y)) '() lst1))
;;   (define (combination n)
;;     (let loop ([n (- n 1)])
;;       (if (<= n 0)
;;           '((a) (d))
;;           (association '(a d) (loop (- n 1))))))
;;   (define (gen lst)
;;     (let loop ([name ""]
;;                [body "p"]
;;                [lst lst])
;;       (if (null? lst)
;;           (begin
;;             (format #t ";; returns ~a\n" body)
;;             (format #t ";; .pre-condition p is pair\n")
;;             (format #t ";; .returns ~a\n" body)
;;             (format #t "(define (c~ar p) ~a)\n\n" name body))
;;           (loop (format "~a~a" (car lst) name)
;;                 (format "(~a ~a)" (if (eq? 'a (car lst)) 'car 'cdr) body)
;;                 (cdr lst)))))
;;   (let loop ([m 2])
;;     (cond
;;      [(< n m) '()]
;;      [else
;;       (for-each gen (combination m))
;;       (loop (+ m 1))])))

;; (gen-cxxr 5)

;; Calls producer with no arguments and a continuation that, when passed some values, calls the consumer procedure with those values as arguments.
;; The continuation for the call to consumer is the continuation of the call to call-with-values.
;; .parameter producer must be a procedure and should accept zero arguments
;; .parameter consumer must be a procedure and should accept as many values as producer returns
;; .returns result of consumer
;; .internal-references "SRFI-8 Binding to multiple values." "receive"
(define (call-with-values producer consumer)
  (apply consumer (producer)))

;; returns the subchain of pairs of list obtained by omitting the first k elements
;; .pre-condition l should be a list of size at least k.
;; .returns the subchain of pairs of list obtained by omitting the first k elements
;; .example (list-tail '(a b c d) 2) => (c d)
(define (list-tail l k)
  (if (= k 0)
      l
      (list-tail (cdr l) (- k 1))))

;; returns the kth element of list.
;; .pre-condition l must be a list whose length is at least k + 1.
;; .returns returns the kth element of list.
;; .example (list-ref '(a b c d) 2) => c
(define (list-ref l k)
  (car (list-tail l k)))

;; returns a newly allocated list of the objects contained in the elements of vector
;; .returns returns a newly allocated list of the objects contained in the elements of vector
;; .example (vector->list '#(dah dah didah)) => (dah dah didah)
(define (vector->list v)
  (let loop ((pos (- (vector-length v) 1)) (l '()))
    (if (< pos 0)
        l
        (loop (- pos 1) (cons (vector-ref v pos) l)))))

;; tests if the number object is = to zero
;; .returns #t is (= n 0)
(define (zero? n)
  (= 0 n))

;; Returns a newly allocated list of its arguments.
;; .returns a newly allocated list of its arguments.
;; .form (list obj ...)
;; .example (list 'a (+ 3 4) 'c) => (a 7 c)
;; .example (list) => ()
(define (list . a) a)

;; Returns the absolute value of its argument.
;; .returns Returns the absolute value of its argument.
;; .example (abs -7) => 7
(define (abs n)
  (if (>= n 0)
      n
      (* -1 n)))

;; The map procedure applies proc element-wise to the elements of the lists and returns a list of the results, in order.
;; .parameter ll should all have the same length.
;; .parameter proc should accept as many arguments as there are lists and return a single value. should not mutate any of the lists.
;; .returns a list of the results
;; .form (map proc list1 list2 ...)
;; .example (map cadr '((a b) (d e) (g h))) => (b e h)
;; .example (map (lambda (n) (expt n n)) '(1 2 3 4 5)) => (1 4 27 256 3125)
;; .example (map + '(1 2 3) '(4 5 6)) => (5 7 9)
(define (map proc . ll)
  (if (null? (car ll))
      '()
      (let ((tetes (map1 car ll))
            (queues (map1 cdr ll)))
        (cons (apply proc tetes)
              (apply map (cons proc queues))))))

;; Returns the length of list.
;; .returns the length of list.
;; (length '(a b c)) => 3
;; (length '(a (b) (c d e))) => 3
;; (length '()) => 0
(define (length l)
  (let loop ((l l) (len 0))
    (if (null? l)
        len
        (loop (cdr l) (+ len 1)))))

;; Returns a newly allocated list consisting of the elements of list in reverse order.
;; .returns a newly allocated list consisting of the elements of list in reverse order.
;; .example (reverse '(a b c)) => (c b a)
;; .example (reverse '(a (b c) d (e (f)))) => ((e (f)) d (b c) a)
(define (reverse l)
  (let loop ((l l) (rl '()))
    (if (null? l)
        rl
        (loop (cdr l) (cons (car l) rl)))))

;; The list->string procedure returns a newly allocated string formed from the characters in list.
;; .returns The list->string procedure returns a newly allocated string formed from the characters in list.
(define (list->string l)
  (let* ((len (length l))
         (newstring (make-string len))
         (iter (lambda (iter l to)
                 (if (< to len)
                     (begin
                       (string-set! newstring to (car l))
                       (iter iter (cdr l) (+ to 1)))))))
    (iter iter l 0)
    newstring))

;; The for-each procedure applies proc element-wise to the elements of the lists for its side effects, in order from the first elements to the last.
;; .form (for-each proc list1 list2 ...)
;; .pre-condition The lists should all have the same length. Proc should accept as many arguments as there are lists. Proc should not mutate any of the lists.
;; .returns The return values of for-each are unspecified.
;; .example (let ((v (make-vector 5)))\
;;  (for-each (lambda (i)\
;;              (vector-set! v i (* i i)))\
;;            '(0 1 2 3 4))\
;;  v)  => #(0 1 4 9 16)
;; .example (for-each (lambda (x) x) '(1 2 3 4)) => unspecified
;; .example (for-each even? ’()) => unspecified
(define (for-each proc . ll)
  (if (null? (car ll))
      #f
      (let* ((tetes (map car ll))
             (queues (map cdr ll)))
        (apply proc tetes)
        (apply for-each (cons proc queues)))))

;; The list->vector procedure returns a newly created vector initialized to the elements of the list list.
;; .returns The list->vector procedure returns a newly created vector initialized to the elements of the list list.
;; .example (list->vector '(dididit dah)) => #(dididit dah)
(define (list->vector l)
  (let* ((len (length l))
         (v (make-vector len)))
    (let loop ((l l) (pos 0))
      (if (not (null? l))
          (begin
            (vector-set! v pos (car l))
            (loop (cdr l) (+ pos 1)))))
    v))

; used internal
(define (append2 l1 l2)
    (if (null? l1)
        l2
        (let ((tete (cons (car l1) l2)))
          (let loop ((cur tete) (l1 (cdr l1)))
            (if (null? l1)
                tete
                (begin
                  (set-cdr! cur (cons (car l1) l2))
                  (loop (cdr cur) (cdr l1))))))))

;; Returns a possibly improper list consisting of the elements of the first list followed by the elements of the other lists, with obj as the cdr of the final pair. An improper list results if obj is not a list.
;; .returns Returns a possibly improper list consisting of the elements of the first list followed by the elements of the other lists, with obj as the cdr of the final pair. An improper list results if obj is not a list.
;; .post-condition If append constructs a nonempty chain of pairs, it is always newly allocated. If no pairs are allocated, obj is returned.
;; .example (append '(x) '(y))                      =>  (x y)
;; .example (append '(a) '(b c d))                  =>  (a b c d)
;; .example (append '(a (b)) '((c)))                =>  (a (b) (c))
;; .example (append '(a b) '(c . d))                =>  (a b c . d)
;; .example (append '() 'a)                         =>  a
(define (append . ll)
    (foldr1 append2 (cons '() ll)))

; ==============================================================================================================================================================
;;; Bytevectors.
;;; R6RS library Chapter 2.
;; Stores octet in element k of bytevector.
;; .pre-condition K must be a valid index of bytevector.
;; .form (bytevector-u8-set! bytevector k octet)
;; .returns unspecified
(define-doc (bytevector-u8-set!) ...)

;; Returns, as an exact integer object, the number of bytes in bytevector.
;; .form (bytevector-length bytevector)
;; .returns As an exact integer object, the number of bytes in bytevector.
(define-doc (bytevector-length) ...)

;; Returns the byte at index k of bytevector, as an octet.
;; .pre-condition K must be a valid index of bytevector.
;; .form (bytevector-u8-ref bytevector k)
;; .returns the byte at index k of bytevector, as an octet.
(define-doc (bytevector-u8-ref) ...)

;; Returns a newly allocated (unless empty) string whose character sequence is encoded by the given bytevector. 
;; .form (utf8->string bytevector)
;; .returns A newly allocated (unless empty) string whose character sequence is encoded by the given bytevector. 
(define-doc (utf8->string) ...)

; ==============================================================================================================================================================
;;; List utilities.
;;; R6RS library Chapter 3.

;; Return the first sublist of list whose car satisfies a given condition with eq?, where the sublists of lists are the lists returned by (list-tail list k) for k less than the length of list.
;; .form (memq obj list)
;; .pre-condition Proc should accept one argument and return a single value. Proc should not mutate list.
;; .returns The first sublist of list whose car satisfies a given condition, where the sublists of lists are the lists returned by (list-tail list k) for k less than the length of list.
(define-doc (memq) ...)

;; Return the first sublist of list whose car satisfies a given condition with equal?, where the sublists of lists are the lists returned by (list-tail list k) for k less than the length of list.
;; .form (member obj list)
;; .pre-condition Proc should accept one argument and return a single value. Proc should not mutate list.
;; .returns The first sublist of list whose car satisfies a given condition, where the sublists of lists are the lists returned by (list-tail list k) for k less than the length of list.
;; (define-doc (member) ...)

; used internal
(define (foldl1 binop l)
  (if (null? (cdr l))
      (car l)
      (foldl1 binop (cons (binop (car l) (cadr l))
                          (cddr l)))))
; used internal
(define (foldl binop start l)
  (if (null? l)
      start
      (foldl binop (binop start (car l)) (cdr l))))

;; <p>The fold-left procedure iterates the combine procedure over an accumulator value and the elements of the lists from left to right, starting with an accumulator value of nil.</p>
;; <p>More specifically, fold-left returns nil if the lists are empty.
;; If they are not empty, combine is first applied to nil and the respective first elements of the lists in order.</p>
;; <p>The result becomes the new accumulator value, and combine is applied to the new accumulator value and the respective next elements of the list.</p>
;; <p>This step is repeated until the end of the list is reached; then the accumulator value is returned. Combine is always called in the same dynamic environment as fold-left itself.</p>
;; .form (fold-left combine nil list1 list2 ...listn)
;; .todo be compatible with R6RS
;; .pre-condition The lists should all have the same length. Combine must be a procedure. It should accept one more argument than there are lists and return a single value. It should not mutate the list arguments.
;; .returns accumlated pair
;; .example (fold-left + 0 '(1 2 3 4 5)) => 15
;; .example (fold-left (lambda (a e) (cons e a)) '()\
;;           '(1 2 3 4 5)) => (5 4 3 2 1)
;; .example (fold-left (lambda (count x) \
;;             (if (odd? x) (+ count 1) count)) \
;;             0 \
;;             '(3 1 4 1 5 9 2 6 5 3)) \
;;           => 7
;; .example (fold-left (lambda (max-len s) \
;;             (max max-len (string-length s))) \
;;             0 \
;;             '("longest" "long" "longer")) \
;;           => 7
;; .example (fold-left + 0 '(1 2 3) '(4 5 6)) \
;;           => 21
;; .example (fold-left cons '(q) '(a b c)) \
;;            => ((((q) . a) . b) . c)
(define fold-left foldl)

; used internal
(define (foldr binop start l)
    (if (null? l)
        start
        (binop (car l) (foldr binop start (cdr l)))))

; used internal
(define (foldr1 binop l)
    (if (null? (cdr l))
        (car l)
        (binop (car l) (foldr1 binop (cdr l)))))

; used internal
(define (foldr2 binop start l1 l2)
    (if (null? l1)
        start
        (binop (car l1) (car l2) (foldr2 binop start (cdr l1) (cdr l2)))))

; used internal
(define (fold kons knil lis1)
  (let lp ((lis lis1) (ans knil))
    (if (null? lis) ans
      (lp (cdr lis) (kons (car lis) ans)))))

;; <p>The fold-right procedure iterates the combine procedure over the elements of the lists from right to left and an accumulator value, starting with an accumulator value of nil.</p>
;; <p>More specifically, fold-right returns nil if the lists are empty.
;; If they are not empty, combine is first applied to the respective last elements of the lists in order and nil.</p>
;; <p>The result becomes the new accumulator value, and combine is applied to the respective previous elements of the lists and the new accumulator value.</p>
;; <p>This step is repeated until the beginning of the list is reached; then the accumulator value is returned.</p>
;; <p>Proc is always called in the same dynamic environment as fold-right itself.</p>
;; .form (fold-right combine nil list1 list2 ...listn)
;; .pre-condition The lists should all have the same length. Combine must be a procedure. It should accept one more argument than there are lists and return a single value. Combine should not mutate the list arguments.
;; .todo r6rs compatible
;; .returns accumulated pair
;; .example (fold-right + 0 '(1 2 3 4 5))\
;;           => 15
;; .example (fold-right cons '() '(1 2 3 4 5))\
;;          => (1 2 3 4 5)
;; .example (fold-right (lambda (x l)\
;;            (if (odd? x) (cons x l) l))\
;;            '()\
;;            '(3 1 4 1 5 9 2 6 5))\
;;           => (3 1 1 5 9 5)
;; .example (fold-right cons '(q) '(a b c))\
;;          => (a b c q)
;; .example (fold-right + 0 '(1 2 3) '(4 5 6))\
;;          => 21
(define fold-right foldr)

;; The remp procedure applies proc to each element of list and returns a list of the elements of list for which proc returned #f.
;; .pre-condition Proc should accept one argument and return a single value. Proc should not mutate list.
;; .returns The remp procedure applies proc to each element of list and returns a list of the elements of list for which proc returned #f.
;; .example (remp even? '(3 1 4 1 5 9 2 6 5)) => (3 1 1 5 9 5)
(define (remp pred l)
  (filter (lambda (a) (not (pred a))) l))

;; <p>The filter procedure applies proc to each element of list and returns a list of the elements of list for which proc returned a true value.</p>
;; <p>The elements of the result list(s) are in the same order as they appear in the input list.</p>
;; .parameter proc Proc should accept one argument and return a single value. Proc should not mutate list.
;; .returns returns a list of the elements of list for which proc returned a true value.
;; .form (filter proc list)
;; .example (filter even? '(3 1 4 1 5 9 2 6)) => (4 2 6)
(define (filter pred? l)
    (cond ((null? l) '())
      ((pred? (car l)) (cons (car l) (filter pred? (cdr l))))
      (else (filter pred? (cdr l)))))

;; .pre-condition Proc should accept one argument and return a single value. Proc should not mutate list.
;; <p>The find procedure applies proc to the elements of list in order. If proc returns a true value for an element, find immediately returns that element. If proc returns #f for all elements of the list, find returns #f.</p>
;; .returns The find procedure applies proc to the elements of list in order. If proc returns a true value for an element, find immediately returns that element. If proc returns #f for all elements of the list, find returns #f.
;; .example (find even? '(3 1 4 1 5 9)) => 4
;; .example (find even? '(3 1 5 1 5 9)) => #f
(define (find pred lst)
  (let loop ([lst lst])
    (if (null? lst)
        #f
        (if (pred (car lst))
            (car lst)
            (loop (cdr lst))))))

; internal use
(define (generic-assoc releq obj alist)
  (cond ((null? alist)
         #f)
        ((releq (car (car alist)) obj)
         (car alist))
        (else
         (generic-assoc releq obj (cdr alist)))))

;; <p>Find the first pair in alist whose car field satisfies a given condition, and returns that pair without traversing alist further. If no pair in alist satisfies the condition, then #f is returned.</p>
;; <p>The assoc procedure uses equal? to compare obj with the car fields of the pairs in alist, while assv uses eqv? and assq uses eq?.</p>
;; .pre-condition Alist (for "association list") should be a list of pairs. Proc should accept one argument and return a single value. Proc should not mutate alist.
;; .example (define e '((a 1) (b 2) (c 3))) (assq 'a e) => (a 1)
;; .example (assq 'b e) => (b 2)
;; .example (assq 'd e) => #f
;; .example (assq (list 'a) '(((a)) ((b)) ((c)))) => #f
;  .example (assq 5 '((2 3) (5 7) (11 13))) => unspecified
(define (assq obj alist) (generic-assoc eq? obj alist))


;; <p>Find the first pair in alist whose car field satisfies a given condition, and returns that pair without traversing alist further. If no pair in alist satisfies the condition, then #f is returned.</p>
;; <p>The assoc procedure uses equal? to compare obj with the car fields of the pairs in alist, while assv uses eqv? and assq uses eq?.</p>
;; .pre-condition Alist (for "association list") should be a list of pairs. Proc should accept one argument and return a single value. Proc should not mutate alist.
;; .example (assv 5 '((2 3) (5 7) (11 13))) => (5 7)
(define (assv obj alist) (generic-assoc eqv? obj alist))

;; <p>Find the first pair in alist whose car field satisfies a given condition, and returns that pair without traversing alist further. If no pair in alist satisfies the condition, then #f is returned.</p>
;; <p>The assoc procedure uses equal? to compare obj with the car fields of the pairs in alist, while assv uses eqv? and assq uses eq?.</p>
;; .pre-condition Alist (for "association list") should be a list of pairs. Proc should accept one argument and return a single value. Proc should not mutate alist.
;; .example (assoc (list 'a) '(((a)) ((b)) ((c)))) => ((a))
(define (assoc obj alist) (generic-assoc equal? obj alist))

; ==============================================================================================================================================================
;;; Control structures.
;;; R6RS library Chapter 5.

;; <p>The [init] expressions are evaluated (in some unspecified order), the [variable]s are bound to fresh locations, the results of the [init] expressions are stored in the bindings of the [variable]s, and then the iteration phase begins.</p>
;; <p>Each iteration begins by evaluating [test]; if the result is #f, then the [command]s are evaluated in order for effect, the [step] expressions are evaluated in some unspecified order, the [variable]s are bound to fresh locations holding the results, and the next iteration begins.</p>
;; <p>If [test] evaluates to a true value, the [expression]s are evaluated from left to right and the values of the last [expression] are returned.</p>
;; <p>If no [expression]s are present, then the do expression returns unspecified values.</p>
;; <p>The regionof the binding of a [variable] consists of the entire do expression except for the [init]s.</p>
;; <p>A [step] may be omitted, in which case the effect is the same as if ([variable] [init] [variable]) had been written instead of ([variable] [init]).</p>
;; .form (do (([variable1] [init1] [step1]) ...) ([test] [expression] ...) [command] ...)
;; .example <pre>(do ((vec (make-vector 5))\
;;            (i 0 (+ i 1)))\
;;            ((= i 5) vec)\
;;              (vector-set! vec i i)) =>  #(0 1 2 3 4)</pre>
;; .example <pre>(let ((x '(1 3 5 7 9)))\
;;            (do ((x x (cdr x))\
;;              (sum 0 (+ sum (car x))))\
;;              ((null? x) sum)))</pre>
(define-doc (do) ...)

;; Evaluates test. If it yields false value, body … are evaluated sequentially, and the result(s) of the last evaluation is(are) returned. Otherwise, unspecified value is returned.
;; .form (unless test body ...)
(define-doc (unless) ...)

;; Evaluates test. If it yields true value, body … are evaluated sequentially, and the result(s) of the last evaluation is(are) returned. Otherwise, unspecified value is returned.
;; .form (when test body ...)
(define-doc (when) ...)

; ==============================================================================================================================================================
;;; I/O.
;;; R6RS library Chapter 8.

;; Reads an external representation from textual-input-port and returns the datum it represents.
;; .form (read) (read textual-input-port)
;; .returns datum
(define-doc (read) ...)

;; Returns an output port for the named file.
;; .form (open-file-output-port filename)
;; .returns An output port for the named file.
(define-doc (open-file-output-port) ...)

;; Returns an input port for the named file.
;; .form (open-file-input-port filename)
;; .returns An input port for the named file.
(define-doc (open-file-input-port) ...)

;; Closes input-port or output-port, respectively.
;; .form (close-input-port input-port)
;; .returns unspecified
(define-doc (close-input-port) ...)

;; Returns a fresh binary input port connected to standard input. Whether the port supports the port-position and set-port-position! operations is implementation-dependent.
;; .form (standard-input-port)
;; .returns A fresh binary input port connected to standard input. Whether the port supports the port-position and set-port-position! operations is implementation-dependent.
(define-doc (standard-input-port) ...)

;; <p>Reads from binary-input-port, blocking as necessary, until count bytes are available from binary-input-port or until an end of file is reached.</p>
;; <p>If count bytes are available before an end of file, get-bytevector-n returns a bytevector of size count.
;; If fewer bytes are available before an end of file, get-bytevector-n returns a bytevector containing those bytes.</p>
;; <p>In either case, the input port is updated to point just past the bytes read.</p>
;; <p>If an end of file is reached before any bytes are available, get-bytevector-n returns the end-of-file object. </p>
;; .pre-condition Count must be an exact, non-negative integer object representing the number of bytes to be read.
;; .form (get-bytevector-n binary-input-port count)
;; .returns bytevector
(define-doc (get-bytevector-n) ...)

;; Returns the end-of-file object. 
;; .form (eof-object)
;; .returns the end-of-file object.
(define-doc (eof-object) ...)

;; <p>Returns a new textual port with the specified transcoder.</p>
;; <p>Otherwise the new textual port's state is largely the same as that of binary-port.</p>
;; <p>If binary-port is an input port, the new textual port will be an input port and will transcode the bytes that have not yet been read from binary-port.
;; If binary-port is an output port, the new textual port will be an output port and will transcode output characters into bytes that are written to the byte sink represented by binary-port.</p>
;; .form (transcoded-port binary-port transcoder)
;; .returns A new textual port with the specified transcoder.
(define-doc (transcoded-port) ...)

;; Predefined codecs for the UTF-8 encoding schemes.
;; .form (utf-8-codec)
;; .returns Predefined codecs for the UTF-8 encoding schemes.
(define-doc (utf-8-codec) ...)

;; Returns transcoder with the behavior specified by its arguments.
;; .form (make-transcoder codec)
;; .returns transcoder with the behavior specified by its arguments.
(define-doc (make-transcoder) ...)

;; <p>Reads from binary-input-port, blocking as necessary, until a byte is available from binary-input-port or until an end of file is reached.</p>
;; <p>If a byte becomes available, get-u8 returns the byte as an octet and updates binary-input-port to point just past that byte.
;; If no input byte is seen before an end of file is reached, the end-of-file object is returned.</p>
;; .form (get-u8 binary-input-port)
;; .returns a byte
(define-doc (get-u8) ...)

;; <p>Note. This procedure is not implementednot all specification.</p>
;; <p>Returns a newly created binary input port whose byte source is an arbitrary algorithm represented by the read! procedure.</p>
;; <p>Id must be a string naming the new port, provided for informational purposes only. Read! must be a procedure and should behave as specified below; it will be called by operations that perform binary input.</p>
;; <p>Each of the remaining arguments may be #f; if any of those arguments is not #f, it must be a procedure and should behave as specified below.</p>
;; <p>(read! bytevector start count)</p>
;; <p>Start will be a non-negative exact integer object, count will be a positive exact integer object, and bytevector will be a bytevector whose length is at least start + count.</p>
;; <p>The read! procedure should obtain up to count bytes from the byte source, and should write those bytes into bytevector starting at index start.</p>
;; <p>The read! procedure should return an exact integer object.</p>
;; <p>This integer object should represent the number of bytes that it has read. To indicate an end of file, the read! procedure should write no bytes and return 0.</p>
;; <p>(get-position)</p>
;; <p>The get-position procedure (if supplied) should return an exact integer object representing the current position of the input port.</p>
;; <p>If not supplied, the custom port will not support the port-position operation.</p>
;; <p>(set-position! pos)</p>
;; <p>Pos will be a non-negative exact integer object. The set-position! procedure (if supplied) should set the position of the input port to pos.</p>
;; <p>If not supplied, the custom port will not support the set-port-position! operation.</p>
;; <p>(close)</p>
;; <p>The close procedure (if supplied) should perform any actions that are necessary when the input port is closed.</p>
;; .form (make-custom-binary-input-port id read! procedure get-position set-position! close)
;; .returns A newly created binary input port whose byte source is an arbitrary algorithm represented by the read! procedure. 
(define-doc (make-custom-binary-input-port) ...)

;; Returns default textual ports for regular error output. Normally, this port is associated with standard error.
;; .form (current-error-port)
;; .returns Default textual ports for regular error output. Normally, this port is associated with standard error.
(define-doc (current-error-port) ...)

;; Writes the external representation of obj to textual-output-port. The write procedure operates in the same way as put-datum. If textual-output-port is omitted, it defaults to the value returned by current-output-port.
;; .form (write obj &optional textual-output-port)
;; .returns unspecified
(define-doc (write) ...)

;; Returns #t if obj is the end-of-file object, #f otherwise.
;; .form (eof-object? obj)
;; .returns #t if obj is the end-of-file object, #f otherwise.
(define-doc (eof-object?) ...)

;; <p>Reads from textual-input-port, blocking as necessary until a character is available from textual-input-port, or the data that are available cannot be the prefix of any valid encoding, or an end of file is reached.</p>
;; <p>If a complete character is available before the next end of file, read-char returns that character, and updates the input port to point past that character.
;; If an end of file is reached before any data are read, read-char returns the end-of-file object.
;; If textual-input-port is omitted, it defaults to the value returned by current-input-port.</p>
;; .form (read-char &optional textual-input-port)
;; .returns character
(define-doc (read-char) ...)

;; Returns a textual input port whose characters are drawn from string.
;; .form (open-string-input-port string)
;; .returns A textual input port whose characters are drawn from string.
(define-doc (open-string-input-port) ...)

; used internal
(define-doc (sys-open-output-string) ...)

; used internal
(define-doc (sys-port-seek) ...)

; used internal
(define-doc (sys-get-output-string) ...)

;; used internal
(define-doc (set-current-input-port!) ...)

;; used internal
(define-doc (set-current-output-port!) ...)

;; Opens filename for output, with empty file options, and returns the obtained port.
;; .form (open-output-file filename)
;; .returns Opens filename for output, with empty file options, and returns the obtained port.
(define-doc (open-output-file) ...)

;; Returns a default textual port for input. Normally, this default port is associated with standard input, but can be dynamically re-assigned using the with-input-from-file procedure
;; .form (current-input-port)
;; .returns A default textual port for input. Normally, this default port is associated with standard input, but can be dynamically re-assigned using the with-input-from-file procedure
(define-doc (current-input-port) ...)

;; Returns a default textual port for output. Normally, this default port is associated with standard output, but can be dynamically re-assigned using the with-output-from-file procedure
;; .form (current-output-port)
;; .returns A default textual port for output. Normally, this default port is associated with standard output, but can be dynamically re-assigned using the with-output-from-file procedure
(define-doc (current-output-port) ...)

;; Closes output-port.
;; .form (close-output-port output-port)
;; .returns unspecified
(define-doc (close-output-port) ...)

;; .pre-condition Proc should accept one argument.
;; <p>Open the file named by filename for output, with no specified file options, and call proc with the obtained port as an argument.</p>
;; <p>If proc returns, the port is closed automatically and the values returned by proc are returned.
;; If proc does not return, the port is not closed automatically, unless it is possible to prove that the port will never again be used for an I/O operation.</p>
;; .returns the values returned by proc.
(define (call-with-output-file filename proc)
  (let* ((port (open-file-output-port filename))
         (ret (proc port)))
    (close-output-port port)
    ret))

;; .pre-condition Proc should accept one argument.
;; <p>Open the file named by filename for input, with no specified file options, and call proc with the obtained port as an argument.</p>
;; <p>If proc returns, the port is closed automatically and the values returned by proc are returned.
;; If proc does not return, the port is not closed automatically, unless it is possible to prove that the port will never again be used for an I/O operation.</p>
;; .returns the values returned by proc.
(define (call-with-input-file filename proc)
  (let* ([port (open-file-input-port filename)]
         [ret (proc port)])
    (close-input-port port)
    ret))

;; <p>Returns two values: a textual output port and an extraction procedure.</p>
;; <p>The output port accumulates the characters written to it for later extraction by the procedure.</p>
;; <p>The extraction procedure takes no arguments.</p>
;; <p>When called, it returns a string consisting of all of the port's accumulated characters (regardless of the current position),removes the accumulated characters from the port, and resetsthe port's position</p>
;; .returns string-output-port, extraction procedure
(define  (open-string-output-port)
  (let* ([port (sys-open-output-string)]
         [proc (lambda () (let1 s (sys-get-output-string port)
                            (sys-port-seek port 0)
                            s))])
    (values port proc)))

;; <p>Creates a textual output port that accumulates the characters written to it and calls proc with that output port as an argument.</p>
;; <p>Whenever proc returns, a string consisting of all of the port's accumulated characters (regardless of the port's current position) is returned and the port is closed.</p>
;; .parameter proc proc must accept one argument
;; .returns accumulated characters as string
(define (call-with-string-output-port proc)
  (receive (port get-string) (open-string-output-port)
    (proc port)
    (get-string)))

;; <p>Creates an output port that accumulates the bytes written to it and calls proc with that output port as an argument.</p>
;; <p>Whenever proc returns, a bytevector consisting of all of the port's accumulated bytes (regardless of the port’s currentposition) is returned and the port is closed.</p>
;; .parameter proc proc must accept one argument
;; .returns accumulated bytes as bytevector
(define (call-with-bytevector-output-port proc transcoder)
  (receive (port get-bytevector) (open-bytevector-output-port transcoder)
    (proc port)
    (get-bytevector)))

;; <p>Returns two values: an output port and an extraction procedure.</p>
;; <p>The output port accumulates the bytes written to it for later extraction by the procedure.</p>
;; <p>If transcoder is a transcoder, it becomes the transcoder associated with the port.
;; If maybe-transcoder is #f or absent, the port will be a binary port and will support the port-position and set-port-position! operations.</p>
;; <p>The extraction procedure takes no arguments.
;; When called, it returns a bytevector consisting of all the port's accumulated bytes (regardless of the port’s current position), removes the accumulated bytes from the port, and resets the port's position.</p>
;; .parameter transcoder transcoder
;; .returns bytevector-output-port and extraction procedure
;; .todo when transcoder is empty or #f
(define  (open-bytevector-output-port transcoder)
  (let* ([port (sys-open-bytevector-output-port transcoder)]
         [proc (lambda () (sys-get-bytevector port))])
    (values port proc)))

;; Applies proc element-wise to the elements of the bytevector for its side effects, in order from the first elements to the last.
;; .parameter bv bytevector
;; .parameter proc proc must accept one argument
;; .returns unspecified
(define (bytevector-for-each bv proc)
  (let1 len (bytevector-length bv)
    (let loop ([i 0])
      (if (>= i len)
          '()
          (begin (proc (bytevector-u8-ref bv i))
                 (loop (+ i 1)))))))

;; <p><p>Writes a representation of obj to the given textual-output-port.</p></p>
;; <p>Strings that appear in the written representation are not enclosed in doublequotes, and no characters are escaped within those strings.</p>
;; <p>Character objects appear in the representation as if written by write-char instead of by write.</p>
;; <p>The textual-output-port argument may be omitted, in which case it defaults to the value returned by current-output-port.</p>
;; .form (display obj) or (display obj textual-output-port)
;; .returns unspecified
;; .parameter obj object to write.
;; .parameter textual-output-port writes obj to textual-output-port
(define (display x . port)
  (if (null? port)
      (sys-display x)
      (sys-display x (car port))))

;; <p>This is equivalent to using write-char to write #\linefeed to textual-output-port.
;; If textual-output-port is omitted, it defaults to the value returned by current-output-port.</p>
;; .parameter textual-output-port port to write
;; .returns unspecified
;; .todo textual-output-port
(define (newline) (display "\n"))

; ==============================================================================================================================================================
;;; File system.
;;; R6RS library Chapter 9.
;; Returns #t if the named file exists at the time the procedure is called, #f otherwise.
;; .form (file-exists? filename)
;; .returns Returns #t if the named file exists at the time the procedure is called, #f otherwise.
(define-doc (file-exists?) ...)

; ==============================================================================================================================================================
;;; Hashtables.
;;; R6RS library Chapter 13.
;; <p>Returns a newly allocated mutable hashtable that accepts arbitrary objects as keys, and compares those keys with eq?. If an argument is given, the initial capacity of the hashtable is set to approximately k elements.</p>
;; .form (make-eq-hashtable) (make-eq-hashtable k)
;; .returns A newly allocated mutable hashtable that accepts arbitrary objects as keys, and compares those keys with eq?. If an argument is given, the initial capacity of the hashtable is set to approximately k elements.
(define-doc (make-eq-hashtable) ...)

;; Changes hashtable to associate key with obj, adding a new association or replacing any existing association for key.
;; .form (hashtable-set! hashtable key obj)
;; .returns unspecified
(define-doc (hash-table-set!) ...)

;; Returns the value in hashtable associated with key. If hashtable does not contain an association for key, default is returned. 
;; .form (hashtable-ref hashtable key default)
;; .returns the value in hashtable associated with key. If hashtable does not contain an association for key, default is returned. 
(define-doc (hash-table-ref) ...)

;; (not implmented) Returns a vector of all keys in hashtable. The order of the vector is unspecified.
;; .form (hashtable-keys hashtable)
;; .returns A vector of all keys in hashtable. The order of the vector is unspecified.
(define-doc (hash-table-keys) ...)

; ==============================================================================================================================================================
;;; Eval.
;;; R6RS library Chapter 16.
;; <p>Evaluates expression in the specified environment and returns its value.</p>
;; <p>Note that currently the environment argument is ignored.</p>
;; .returns result
;; .form (eval expression environment)
(define-doc (eval) ...)

; ==============================================================================================================================================================
;;; Mutable pairs.
;;; R6RS library Chapter 17.
;; Stores obj in the car field of pair.
;; .returns unspecified
;; .form (set-car! pair obj)
(define-doc (set-car!) ...)

;; Stores obj in the cdr field of pair.
;; .returns unspecified
;; .form (set-cdr! pair obj)
(define-doc (set-cdr!) ...)

; ==============================================================================================================================================================
;;; Regular expression.
;;; Regular expression is implmented with Oniguruma library.<br>

;; <p>Regexp is a regular expression object. A string string is matched by regexp. If it matches, the function returns a [regmatch] object. Otherwise it returns #f.</p>
;; <p>You can use (#/regexp/ string) style instead of (rxmatch #/regexp/ string).</p>
;; .returns If it matches, the function returns a [regmatch] object. Otherwise it returns #f.
;; .parameter regexp A regular expression object.
;; .parameter string String matched by regexp.
;; .form (rxmatch regexp string)
;; .example (rxmatch #/123/ "12") => #f
;; .example (rxmatch #/\d+/ "a345a") => [regmatch]
;; .internal-references "Regular expression." "regexp"
(define-doc (rxmatch) ...)

;; Regexp is a regular expression object. A string string is matched by regexp. If it matches, the function returns a [regmatch] object. Otherwise it returns #f.
;; .returns If it matches, the function returns a [regmatch] object. Otherwise it returns #f.
;; .parameter regexp A regular expression object.
;; .parameter string String matched by regexp.
;; .form (regexp string)
;; .example (#/123/ "12") => #f
;; .example (#/\d+/ "a345a") => [regmatch]
;; .internal-references "Regular expression." "rxmatch"
(define-doc (regexp) ...)


;; Returns #t if obj is a regexp object.
;; .form (regexp? obj)
;; .returns #t if obj is a regexp object.
;; .example (regexp? #/abc/) => #t
;; .example (regexp? "abc")  => #f
(define-doc (regexp?) ...)

;; Returns a source string describing the regexp regexp. The returned string is immutable.
;; .returns S source string describing the regexp regexp. The returned string is immutable.
;; .form (regexp->string regexp)
;; .example (regexp->string #/abc/) => "abc"
(define-doc (regexp->string) ...)

;; <p>If i equals to zero, the functions return start of entire match.
;; With positive integer I, it returns those of I-th submatches.</p>
;; <p>It is an error to pass other values to I.
;; It is allowed to pass #f to match for convenience.
;; The functions return #f in such case.</p>
;; .returns I-th submatches.
;; .form (rxmatch-start match &optional (i 0))
;; .parameter match [regmatch] object returned by rxmatch
;; .parameter i index
;; .example (rxmatch-start (#/\d+/ "aaaa")) => #f
;; .example (rxmatch-start (rxmatch #/(\d+)(a)/ "a345a") 2) => 4
(define-doc (rxmatch-start) ...)

;; <p>If i equals to zero, the functions return end of entire match.
;; With positive integer I, it returns those of I-th submatches.</p>
;; <p>It is an error to pass other values to I.
;; It is allowed to pass #f to match for convenience.
;; The functions return #f in such case.</p>
;; .returns I-th submatches.
;; .form (rxmatch-end match &optional (i 0))
;; .parameter match [regmatch] object returned by rxmatch
;; .parameter i index
;; .example (rxmatch #/\d+/ "a345a") => 4
(define-doc (rxmatch-end) ...)

;; <p>If i equals to zero, the functions return substring of entire match.
;; With positive integer I, it returns those of I-th submatches.</p>
;; <p>It is an error to pass other values to I.
;; It is allowed to pass #f to match for convenience.
;; The functions return #f in such case.</p>
;; .returns I-th submatches.
;; .form (rxmatch-substring match &optional (i 0))
;; .parameter match [regmatch] object returned by rxmatch
;; .parameter i index
;; .example (rxmatch-substring (#/\d+/ "a345a")) => "345"
(define-doc (rxmatch-substring) ...)

;; Returns substring of the input string after match.
;; If optional argument is given, the i-th submatch is used (0-th submatch is the entire match).
;; .returns Substring of the input string after match.
;; .form (rxmatch-after match &optional (i 0))
;; .example (rxmatch-after (#/abc/ "123abcdef")) => "def"
(define-doc (rxmatch-after) ...)

;; Returns substring of the input string before match.
;; If optional argument is given, the i-th submatch is used (0-th submatch is the entire match).
;; .returns Substring of the input string before match.
;; .form (rxmatch-before match &optional (i 0))
;; .example (rxmatch-before (#/abc/ "123abcdef")) => "123"
(define-doc (rxmatch-before) ...)

;; Works same as (rxmatch-substring regmatch index),
;; .form (regmatch &optional index)
;; .example ((#/abc/ "123abcdef") 0) => "abc"
;; .internal-references "Regular expression." "rxmatch-substring"
(define-doc (regmatch) ...)

;; Works same as (rxmatch-before regmatch)
;; .form  (regmatch 'before &optional index)
;; .example ((#/abc/ "123abcdef") 'before) => "123"
;; .internal-references "Regular expression." "rxmatch-before"
(define-doc (regmatch) ...)

;; Works same as (rxmatch-after regmatch)
;; .form  (regmatch 'after &optional index)
;; .example ((#/abc/ "123abcdef") 'after) => "def"
;; .internal-references "Regular expression." "rxmatch-after"
(define-doc (regmatch) ...)

;; Replaces the part of string that matched to regexp for substitution. Just replaces the first match of regexp.
;; .form (regexp-replace regexp string substitution)
;; .returns replaced string
;; .example (regexp-replace #/abc/ "123abc456" "ABC") => "123ABC456"
(define-doc (regexp-replace) ...)

;; Replaces the part of string that matched to regexp for substitution. Repeats the replacing throughout entire string.
;; .form (regexp-replace-all regexp string substitution)
;; .returns replaced string
(define-doc (regexp-replace-all) ...)

;; .form (string->regexp string)
;; Takes string as a regexp specification, and constructs [regexp] object.
;; .returns [regexp] object
;; .example (string->regexp "abc") #/abc
(define-doc (string->regexp) ...)

; ==============================================================================================================================================================
;;; System interfaces.

;; Returns the value of the environment variable name as a string, or #f if the environment variable is not defined.
;; .form (sys-getenv name)
;; .returns The value of the environment variable name as a string, or #f if the environment variable is not defined.
;; .example (sys-getenv "QUERY_STRING")
(define-doc (sys-getenv) ...)

;; Returns a list of strings of the directory entries.
;; .form (sys-readdir path)
;; .returns A list of strings of the directory entries.
(define-doc (sys-readdir) ...)

;; get-timeofday
;; .form (get-timeofday)
;; .returns (get-timeofday)
(define-doc (get-timeofday) ...)

; ==============================================================================================================================================================
;;; Generic.

;; Same as (let ((var val)) body ...)
;; .form (let1 var val body ...)
(define-doc (let1) ...)

;; Returns a new symbol.
;; .form (gensym)
;; .returns A new symbol.
;; .example (gensym) => G100
(define-doc (gensym) ...)

;; If given character char is a valid digit character in radix radix number, the corresponding integer is returned. Otherwise #f is returned.
;; .form (digit->integer char &optional (radix 10))
;; .returns If given character char is a valid digit character in radix radix number, the corresponding integer is returned. Otherwise #f is returned.
;; .example (digit->integer #\4) => 4
;; .example (digit->integer #\e 16) => 14
;; .example (digit->integer #\9 8) => #f
(define-doc (digit->integer) ...)

;; Writes a representation of obj to the current output port and (newline)
;; Strings that appear in the written representation are not enclosed in doublequotes, and no characters are escaped within those strings.
;; Character objects appear in the representation as if written by write-char instead of by write.
;; .returns unspecified values
(define (print obj)
  (display obj)
  (display "\n"))

;; The map1 procedure applies proc element-wise to the elements of the list and returns a list of the results, in order.
;; .parameter l list
;; .parameter proc should accept as many arguments as there are lists and return a single value.
;; .returns a list of the results
;; .form (map1 proc l)
(define (map1 f l)
    (if (null? l)
        l
        (cons (f (car l)) (map1 f (cdr l)))))

;; Returns the cdr of first pair whose car fields satisfies a given key.
;; .returns Returns the cdr of first pair whose car fields satisfies a given key.
(define (assoc-ref lst key)
  (cond [(assoc key lst) => cdr]
        [else #f]))

;; Reads one line (a sequence of characters terminated by newline or EOF) from port and returns a string.
;; .returns line as a string
(define (read-line . port)
  (let* ((char (apply read-char port)))
    (if (eof-object? char)
        char
        (do ((char char (apply read-char port))
             (clist '() (cons char clist)))
            ((or (eof-object? char) (char=? #\newline char))
             (list->string (reverse clist)))))))

;; Same as (call-with-output-file path (lambda (port) (display content obj)))
;; .returns unspecified.
;; .internal-references "R6RS" "call-with-output-file"
(define (write-to-file path content)
  (call-with-output-file path
    (lambda (port)
      (display content port))))

;; Read string from a file filename.
;; .returns whole file content as string
(define (file->string filename)
  (if (file-exists? filename)
      (call-with-input-file filename
        (lambda (p)
          (let loop ([ret '()][c (read-char p)])
            (if (eof-object? c)
                (list->string (reverse ret))
                (loop (cons c ret) (read-char p))))))
        ""))

;; <p>Convenient string I/O procedure.</p>
;; <pre>(define (call-with-string-io str proc)<br>
;;       (receive (out get-string) (open-string-output-port)<br>
;;         (let1 in (open-string-input-port str)<br>
;;           (proc in out)<br>
;;           (get-string))))</pre>
;; .returns output-string
(define (call-with-string-io str proc)
  (receive (out get-string) (open-string-output-port)
    (let1 in (open-string-input-port str)
      (proc in out)
      (get-string))))

;; Creates a textual input port from string str and calls proc with that input port as an argument.
;; .parameter proc proc must accept one argument
;; .returns values proc returns
(define (call-with-string-input-port str proc)
  (let ((in (open-string-input-port str)))
    (proc in)))

; ==============================================================================================================================================================
;;; CGI.
;;; Note this library will go away once it is switched to the new CGI library.

; used internal
(define cgi-header-out? #f)

;; Escape some not safe characters.
;; .returns escaped string
(define (cgi-escape text)
  (fold (lambda (x y) (regexp-replace-all (car x) y (cdr x)))
        text
        '(;(#/&/ . "&amp;")
          (#/</ . "&lt;")
          (#/>/ . "&gt;")
;          (#/\"/ . "&quot;")
          (#/[^\\]'/ . "\'"))))

;; Apply "% encode" to string and return the result. Assumes input is UTF-8.
;; .returns encoded string
(define (cgi-encode text)
  (call-with-string-output-port
   (lambda(out)
     (bytevector-for-each
      (call-with-bytevector-output-port
       (lambda (port)
         (display  text port))
       (make-transcoder (utf-8-codec)))
      (lambda (b)
        (display "%" out)
        (display (number->string b 16) out))))))

;; Initialize CGI library and returns get-parameter and get-request-method procedures.
;; .returns get-parameter and get-request-method procedures as multiple values.
;; .example (receive (get-parameter get-request-method) (cgi-init) ...)
(define (cgi-init)
  (let1 parsed (cgi-parse-query-string (get-request-body-cgi (get-request-method-cgi)))
    (values
     (lambda (key)
       (let ([value (assoc key parsed)])
         (if value
             (second value)
             #f)))
     get-request-method-cgi)))

;; Outputs HTTP header "Status: 200 OK\nContent-type: text/html; charset=utf-8\n"
;; .returns unspecified
(define (cgi-header)
  (unless cgi-header-out?
    (print "Status: 200 OK\nContent-type: text/html; charset=utf-8\n")
    (set! cgi-header-out? #t)))

;; Outputs HTTP header "Status: 302 Moved Temporarily\nLocation: [url]\n\n"
;; .parameter url redirect URL
;; .returns unspecified
(define (cgi-moved-header url)
   (format #t "Status: 302 Moved Temporarily\nLocation: ~a\n\n" url)
   (set! cgi-header-out? #t))

;; used internal
(define (cgi-parse-query-string input)
  (if (or (not (string? input)) (equal? "" input)) '()
  (fold-right
   (lambda (a b)
     (let [(params (string-split a #\=))]
       (cons (list (car params) (cadr params)) b)))
   '()
   (string-split input #\&))))

;; Apply "% decode" to string and return the result.  Assumes input is UTF-8.
;; .returns decoded string
(define (cgi-decode s)
  (call-with-string-io
   s
   (lambda (in out)
     (let1 p (transcoded-port
              (make-custom-binary-input-port
               "cgi decode"
               (lambda (bv start count)
                 (let1 read-byte (lambda ()
                                   (let1 c (read-char in)
                                     (cond
                                      [(eof-object? c) (eof-object)]
                                      [(eq? #\+ c) 32]
                                      [(eq? #\% c)
                                       (let ([a (digit->integer (read-char in) 16)]
                                             [b (digit->integer (read-char in) 16)])
                                         (+ (* a 16) b))]
                                      [else
                                       (char->integer c)])))
                   (let loop ([size 0]
                              [b (read-byte)])
                     (cond
                      [(eof-object? b) size]
                      [else
                       (bytevector-u8-set! bv (+ start size) b)
                       (if (>= (+ size 1) count)
                           '()
                           (loop (+ size 1) (read-byte)))]))))
               #f #f #f)
              (make-transcoder (utf-8-codec)))
       (let loop ([c (read-char p)])
         (cond
          [(eof-object? c) '()]
          [else
           (display c out)
           (loop (read-char p))]))))))

; used internal
(define (get-request-method-command-line) (if (equal? (third (command-line)) "GET") 'GET 'POST))

; used internal
(define (get-request-body-command-line method) (fourth (command-line)))

; used internal
(define (get-request-method-cgi) (if (equal? (sys-getenv "REQUEST_METHOD") "GET") 'GET 'POST))

; used internal
(define (get-request-body-cgi method)
  (case method
    [(POST)
     (let* ([content-length (sys-getenv "CONTENT_LENGTH")]
            [len            (if content-length (string->number content-length) 0)])
       (if (= 0 len)
           ""
           (utf8->string (get-bytevector-n (standard-input-port) len)))
             )]
    [else
     (sys-getenv "QUERY_STRING")]))

; used internal
(define (cgi-print-env key)
  (format #t "(~a ~a)<br>" key (sys-getenv key)))

; used internal for debug use.
(define (cgi-print-all-env)
  (for-each print-env '("HTTP_HOST" "CONTENT_LENGTH" "QUERY_STRING" "HTTP_COOKIE" "REQUEST_METHOD" "CONTENT_TYPE" "PATHINFO" "REQUEST_URI" "SCRIPT_NAME")))

; ==============================================================================================================================================================
;;; SRFI-1 List library.
;;; .section-id srfi-1

;; .form (first pair)
;; .returns (car pair)
;; .reference "SRFI-1" "SRFI-1 List Library" "http://srfi.schemers.org/srfi-1/srfi-1.html"
;; Synonym for car
(define first car)

;; .returns (cadr pair)
;; .reference "SRFI-1" "SRFI-1 List Library" "http://srfi.schemers.org/srfi-1/srfi-1.html"
;; Synonym for cadr
(define (second pair) (cadr pair))

;; .returns (caddr pair)
;; .reference "SRFI-1" "SRFI-1 List Library" "http://srfi.schemers.org/srfi-1/srfi-1.html"
;; Synonym for caddr
(define (third pair) (caddr pair))

;; .returns (cadddr pair)
;; .reference "SRFI-1" "SRFI-1 List Library" "http://srfi.schemers.org/srfi-1/srfi-1.html"
;; Synonym for cadddr
(define (fourth pair) (cadddr pair))

; ==============================================================================================================================================================
;;; SRFI-8 Binding to multiple values.
;; <p>This is the way to receive multiple values.</p>
;; <p>Formals can be a (maybe-improper) list of symbols.</p>
;; <p>Expression is evaluated, and the returned value(s) are bound to formals like the binding of lambda formals, then body ... are evaluated.</p>
;; .form (receive formals expression body ...)
;; .returns The results of the last expression in the body are the values of the receive-expression.
;; .example (receive (a b c) (values 1 2 3) (+ a b c)) => 6
(define-doc (receive . args) ...)

; ==============================================================================================================================================================
;;; SRFI-28 Basic format strings.
;; <p>Format arg ... according to string.</p>
;; <p>port specifies the destination;</p>
;; <p>if it is an output port, the formatted result is written to it;</p>
;; <p>if it is #t, the result is written to the current output port;</p>
;; <p>if it is #f, the formatted result is returned as a string.</p>
;; <p>Port can be omitted, as SRFI-28 format; it has the same effects as giving #f to the port.</p>
;; <p>string is a string that contains format directives.</p>
;; <p>A format directive is a character sequence begins with tilda, `~', and ends with some specific characters.</p>
;; <p>The rest of string is copied to the output as is.</p>
;; <p>Currently supported directive is ~a/~A (The corresponding argument is printed by display.), ~s/~S (The corresponding argument is printed by write.)</p>
;; .form (format string arg ...) (format port string arg ...)
;; .returns unspecified or string
;; .example (format #f "apple is ~a" "sweet") => "apple is sweet"
;; .example (call-with-output-string (lambda (out) (format out "apple is ~a" "sweet"))) => "apple is sweet"
(define-doc (format) ...)

(define (REPL . x)
  (define (rec)
    (display "mosh>")
    (print (eval (read (current-input-port)) '()))
    (rec))
  (rec))




