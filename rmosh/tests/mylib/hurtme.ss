;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: record types as classes
;;;Date: Thu Apr  1, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


;#!r6rs
(library (hurtme)
  (export

    ;; definitions
    define-class			define/with

    ;; constructors
    make
    make-record-maker			make-record-maker*

    ;; predicates
    is-a?				record-is-a?
    record-type-parent?

    ;; inspection
    record-type-of
    record-parent-list			record-parent-list*

    ;; field access
    with-fields
    let-fields				let*-fields
    letrec-fields			letrec*-fields

    ;; builtin conventional record type names
    <top> <builtin>
    <pair> <list>
    <char> <string> <vector> <bytevector> <hashtable>
    <record> <condition>
    <port> <binary-port> <input-port> <output-port> <textual-port>
    <fixnum> <flonum> <integer> <integer-valued> <rational> <rational-valued>
    <real> <real-valued> <complex> <number>

    with-record-fields-of-<top>
    with-record-fields-of-<builtin>
    with-record-fields-of-<pair>
    with-record-fields-of-<list>
    with-record-fields-of-<char>
    with-record-fields-of-<string>
    with-record-fields-of-<vector>
    with-record-fields-of-<bytevector>
    with-record-fields-of-<hashtable>
    with-record-fields-of-<record>
    with-record-fields-of-<condition>
    with-record-fields-of-<port>
    with-record-fields-of-<binary-port>
    with-record-fields-of-<input-port>
    with-record-fields-of-<output-port>
    with-record-fields-of-<textual-port>
    with-record-fields-of-<fixnum>
    with-record-fields-of-<flonum>
    with-record-fields-of-<integer>
    with-record-fields-of-<integer-valued>
    with-record-fields-of-<rational>
    with-record-fields-of-<rational-valued>
    with-record-fields-of-<real>
    with-record-fields-of-<real-valued>
    with-record-fields-of-<complex>
    with-record-fields-of-<number>

    ;; generic functions infrastructure
    define-generic declare-method add-method define-generic/merge
    call-next-method next-method?

    ;; predefined generic functions
    object->string)
  (import (rnrs)
    (rnrs mutable-pairs (6))
    (rename (only (system) make-parameter parameterize)
	    (parameterize parametrise)))


(define-syntax begin0
  ;;This  syntax  comes from  the  R6RS  original  document, Appendix  A
  ;;``Formal semantics''.
  (syntax-rules ()
    ((_ ?expr0 ?expr ...)
     (call-with-values
	 (lambda () ?expr0)
       (lambda args
	 ?expr ...
	 (apply values args))))))

(define-syntax with-accessor-and-mutator
  (syntax-rules ()
    ((_ ((?name ?thing ?accessor ?mutator) ?spec ...) ?body0 ?body ...)
     (let-syntax ((?name (identifier-syntax
			  (_              (?accessor ?thing))
			  ((set! _ ?expr) (?mutator ?thing ?expr)))))
       (with-accessor-and-mutator (?spec ...) ?body0 ?body ...)))
    ((_ ((?name ?thing ?accessor) ?spec ...) ?body0 ?body ...)
     (let-syntax ((?name (identifier-syntax (?accessor ?thing))))
       (with-accessor-and-mutator (?spec ...) ?body0 ?body ...)))
    ((_ () ?body0 ?body ...)
     (begin ?body0 ?body ...))))



;;;; helpers

(define-syntax take-left
  (syntax-rules ()
    ((_ ?dotted ?k)
     (let loop ((ret    '())
		(dotted ?dotted)
		(k      ?k))
       (if (zero? k)
	   (reverse ret)
	 (loop (cons (car dotted) ret)
	       (cdr dotted)
	       (- k 1)))))))

(define-syntax for-all*
  ;;Test that the lists have equal  length and all the elements are EQ?;
  ;;return true or false.
  ;;
  ;;This is  more than SRFI-1's EVERY,  because EVERY does  not test for
  ;;equal length.  It is not like R6RS's FOR-ALL, because FOR-ALL raises
  ;;an error if the length is different.
  ;;
  (syntax-rules ()
    ((_ ?eq ?ell1 ?ell2)
     (let loop ((ell1 ?ell1)
		(ell2 ?ell2))
       (cond ((null? ell1)
	      (null? ell2))
	     ((null? ell2)
	      (null? ell1))
	     ((?eq (car ell1) (car ell2))
	      (loop (cdr ell1) (cdr ell2)))
	     (else #f))))))

(define-syntax make-list
  (syntax-rules ()
    ((_ ?len ?fill)
     (let ((len ?len))
       (do ((i 0 (+ 1 i))
	    (result '() (cons ?fill result)))
	   ((= i ?len)
	    result))))))


(define-syntax define-class
  (syntax-rules (fields mutable immutable parent protocol sealed opaque parent-rtd nongenerative
			virtual-fields)

    ((_ (?name ?constructor ?predicate) ?clause ...)
     (%define-class/sort-clauses
      (?name ?constructor ?predicate)
      ()	;collected mutable fields
      ()	;collected immutable fields
      ()	;collected mutable virtual fields
      ()	;collected immutable virtual fields
      (fields) (parent) (protocol) (sealed) (opaque) (parent-rtd) (nongenerative)
      ?clause ...))

    ((_ ?name ?clause ...)
     (%define-class/expand-name ?name ?clause ...))))

(define-syntax %define-class/expand-name
  (lambda (stx)
    (define (%maker name)
      (string->symbol (string-append "make-" name)))
    (define (%predicate name)
      (string->symbol (string-append name "?")))
    (syntax-case stx (fields mutable immutable parent protocol sealed opaque parent-rtd nongenerative
			     virtual-fields)
      ((_ ?name ?clause ...)
       (let ((name (symbol->string (syntax->datum #'?name))))
	 (with-syntax ((MAKER     (datum->syntax #'?name (%maker     name)))
		       (PREDICATE (datum->syntax #'?name (%predicate name))))
	   #'(%define-class/sort-clauses
	      (?name MAKER PREDICATE)
	      () ;collected mutable fields
	      () ;collected immutable fields
	      () ;collected mutable virtual fields
	      () ;collected immutable virtual fields
	      (fields) (parent) (protocol) (sealed) (opaque) (parent-rtd) (nongenerative)
	      ?clause ...))))
      )))

(define-syntax %define-class/sort-clauses
  ;;Sorts all  the auxiliary  syntaxes.  Collects the  specifications of
  ;;mutable and immutable fields.
  ;;
  ;;(Note by Marco  Maggi, Thu Apr 1, 2010) The  expansion of this macro
  ;;adds all the auxiliary syntaxes which are missing in the input form;
  ;;later  the unused ones  are removed  by %DEFINE-CLASS/FILTER-UNUSED;
  ;;adding and removing  is useless, but for now it gives  me a sense of
  ;;control and expandability, so I keep it like this.
  ;;
  (syntax-rules (fields mutable immutable parent protocol sealed opaque parent-rtd nongenerative
			virtual-fields)

    ;;Gather the PARENT clause.
    ((%define-class/sort-clauses
      (?name ...)
      (?collected-mutable-field ...)
      (?collected-immutable-field ...)
      (?collected-mutable-virtual-field ...)
      (?collected-immutable-virtual-field ...)
      (fields		?fie ...)
      (parent		?par ...)
      (protocol		?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	?pad ...)
      (nongenerative	?non ...)
      (parent ?parent-name) ?clause ...)
     (%define-class/sort-clauses
      (?name ...)
      (?collected-mutable-field ...)
      (?collected-immutable-field ...)
      (?collected-mutable-virtual-field ...)
      (?collected-immutable-virtual-field ...)
      (fields		?fie ...)
      (parent		?par ... ?parent-name)
      (protocol		?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	?pad ...)
      (nongenerative	?non ...)
      ?clause ...))

    ;;Gather the PROTOCOL clause.
    ((%define-class/sort-clauses
      (?name ...)
      (?collected-mutable-field ...)
      (?collected-immutable-field ...)
      (?collected-mutable-virtual-field ...)
      (?collected-immutable-virtual-field ...)
      (fields		?fie ...)
      (parent		?par ...)
      (protocol		?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	?pad ...)
      (nongenerative	?non ...)
      (protocol ?protocol-proc) ?clause ...)
     (%define-class/sort-clauses
      (?name ...)
      (?collected-mutable-field ...)
      (?collected-immutable-field ...)
      (?collected-mutable-virtual-field ...)
      (?collected-immutable-virtual-field ...)
      (fields		?fie ...)
      (parent		?par ...)
      (protocol		?pro ... ?protocol-proc)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	?pad ...)
      (nongenerative	?non ...)
      ?clause ...))

    ;;Gather the SEALED clause.
    ((%define-class/sort-clauses
      (?name ...)
      (?collected-mutable-field ...)
      (?collected-immutable-field ...)
      (?collected-mutable-virtual-field ...)
      (?collected-immutable-virtual-field ...)
      (fields		?fie ...)
      (parent		?par ...)
      (protocol		?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	?pad ...)
      (nongenerative	?non ...)
      (sealed ?sealed) ?clause ...)
     (%define-class/sort-clauses
      (?name ...)
      (?collected-mutable-field ...)
      (?collected-immutable-field ...)
      (?collected-mutable-virtual-field ...)
      (?collected-immutable-virtual-field ...)
      (fields		?fie ...)
      (parent		?par ...)
      (protocol		?pro ...)
      (sealed		?sea ... ?sealed)
      (opaque		?opa ...)
      (parent-rtd	?pad ...)
      (nongenerative	?non ...)
      ?clause ...))

    ;;Gather the OPAQUE clause.
    ((%define-class/sort-clauses
      (?name ...)
      (?collected-mutable-field ...)
      (?collected-immutable-field ...)
      (?collected-mutable-virtual-field ...)
      (?collected-immutable-virtual-field ...)
      (fields		?fie ...)
      (parent		?par ...)
      (protocol		?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	?pad ...)
      (nongenerative	?non ...)
      (opaque ?opaque) ?clause ...)
     (%define-class/sort-clauses
      (?name ...)
      (?collected-mutable-field ...)
      (?collected-immutable-field ...)
      (?collected-mutable-virtual-field ...)
      (?collected-immutable-virtual-field ...)
      (fields		?fie ...)
      (parent		?par ...)
      (protocol		?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ... ?opaque)
      (parent-rtd	?pad ...)
      (nongenerative	?non ...)
      ?clause ...))

    ;;Gatherthe PARENT-RTD clause.
    ((%define-class/sort-clauses
      (?name ...)
      (?collected-mutable-field ...)
      (?collected-immutable-field ...)
      (?collected-mutable-virtual-field ...)
      (?collected-immutable-virtual-field ...)
      (fields		?fie ...)
      (parent		?par ...)
      (protocol		?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	?pad ...)
      (nongenerative	?non ...)
      (parent-rtd ?parent-rtd ?parent-cd) ?clause ...)
     (%define-class/sort-clauses
      (?name ...)
      (?collected-mutable-field ...)
      (?collected-immutable-field ...)
      (?collected-mutable-virtual-field ...)
      (?collected-immutable-virtual-field ...)
      (fields		?fie ...)
      (parent		?par ...)
      (protocol		?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	?pad ... ?parent-rtd ?parent-cd)
      (nongenerative	?non ...)
      ?clause ...))

    ;;Gather the NONGENERATIVE non-empty clause.
    ((%define-class/sort-clauses
      (?name ...)
      (?collected-mutable-field ...)
      (?collected-immutable-field ...)
      (?collected-mutable-virtual-field ...)
      (?collected-immutable-virtual-field ...)
      (fields		?fie ...)
      (parent		?par ...)
      (protocol		?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	?pad ...)
      (nongenerative	?non ...)
      (nongenerative ?nongenerative) ?clause ...)
     (%define-class/sort-clauses
      (?name ...)
      (?collected-mutable-field ...)
      (?collected-immutable-field ...)
      (?collected-mutable-virtual-field ...)
      (?collected-immutable-virtual-field ...)
      (fields		?fie ...)
      (parent		?par ...)
      (protocol		?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	?pad ...)
      (nongenerative	?non ... ?nongenerative)
      ?clause ...))

    ;;Gather the NONGENERATIVE empty clause.
    ((%define-class/sort-clauses
      (?name ...)
      (?collected-mutable-field ...)
      (?collected-immutable-field ...)
      (?collected-mutable-virtual-field ...)
      (?collected-immutable-virtual-field ...)
      (fields		?fie ...)
      (parent		?par ...)
      (protocol		?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	?pad ...)
      (nongenerative	?non ...)
      (nongenerative) ?clause ...)
     (%define-class/sort-clauses
      (?name ...)
      (?collected-mutable-field ...)
      (?collected-immutable-field ...)
      (?collected-mutable-virtual-field ...)
      (?collected-immutable-virtual-field ...)
      (fields		?fie ...)
      (parent		?par ...)
      (protocol		?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	?pad ...)
      (nongenerative	?non ... #t)
      ?clause ...))

    ;;Gather mutable FIELDS clause.
    ((%define-class/sort-clauses
      (?name ...)
      (?collected-mutable-field ...)
      (?collected-immutable-field ...)
      (?collected-mutable-virtual-field ...)
      (?collected-immutable-virtual-field ...)
      (fields		?fie ...)
      (parent		?par ...)
      (protocol		?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	?pad ...)
      (nongenerative	?non ...)
      (fields (mutable ?field0 ?field ...) ?field-clause ...) ?clause ...)
     (%define-class/sort-clauses
      (?name ...)
      (?collected-mutable-field ... (?field0 ?field ...))
      (?collected-immutable-field ...)
      (?collected-mutable-virtual-field ...)
      (?collected-immutable-virtual-field ...)
      (fields		?fie ... (mutable ?field0 ?field ...))
      (parent		?par ...)
      (protocol	?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	?pad ...)
      (nongenerative	?non ...)
      (fields ?field-clause ...) ?clause ...))

    ;;Gather immutable FIELDS clause.
    ((%define-class/sort-clauses
      (?name ...)
      (?collected-mutable-field ...)
      (?collected-immutable-field ...)
      (?collected-mutable-virtual-field ...)
      (?collected-immutable-virtual-field ...)
      (fields		?fie ...)
      (parent		?par ...)
      (protocol		?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	?pad ...)
      (nongenerative	?non ...)
      (fields (immutable ?field0 ?field ...) ?field-clause ...) ?clause ...)
     (%define-class/sort-clauses
      (?name ...)
      (?collected-mutable-field ...)
      (?collected-immutable-field ... (?field0 ?field ...))
      (?collected-mutable-virtual-field ...)
      (?collected-immutable-virtual-field ...)
      (fields		?fie ... (immutable ?field0 ?field ...))
      (parent		?par ...)
      (protocol		?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	?pad ...)
      (nongenerative	?non ...)
      (fields ?field-clause ...) ?clause ...))

    ;;Gather   immutable  FIELDS   clause  declared   without  IMMUTABLE
    ;;auxiliary syntax.
    ((%define-class/sort-clauses
      (?name ...)
      (?collected-mutable-field ...)
      (?collected-immutable-field ...)
      (?collected-mutable-virtual-field ...)
      (?collected-immutable-virtual-field ...)
      (fields		?fie ...)
      (parent		?par ...)
      (protocol		?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	?pad ...)
      (nongenerative	?non ...)
      (fields ?field ?field-clause ...) ?clause ...)
     (%define-class/sort-clauses
      (?name ...)
      (?collected-mutable-field ...)
      (?collected-immutable-field ... (?field))
      (?collected-mutable-virtual-field ...)
      (?collected-immutable-virtual-field ...)
      (fields		?fie ... (immutable ?field))
      (parent		?par ...)
      (protocol		?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	?pad ...)
      (nongenerative	?non ...)
      (fields ?field-clause ...) ?clause ...))

    ;;Remove empty, leftover, FIELDS clause.
    ((%define-class/sort-clauses
      (?name ...)
      (?collected-mutable-field ...)
      (?collected-immutable-field ...)
      (?collected-mutable-virtual-field ...)
      (?collected-immutable-virtual-field ...)
      (fields		?fie ...)
      (parent		?par ...)
      (protocol	?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	?pad ...)
      (nongenerative	?non ...)
      (fields) ?clause ...)
     (%define-class/sort-clauses
      (?name ...)
      (?collected-mutable-field ...)
      (?collected-immutable-field ...)
      (?collected-mutable-virtual-field ...)
      (?collected-immutable-virtual-field ...)
      (fields		?fie ...)
      (parent		?par ...)
      (protocol		?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	?pad ...)
      (nongenerative	?non ...)
      ?clause ...))

    ;;Gather mutable VIRTUAL-FIELDS clause.
    ((%define-class/sort-clauses
      (?name ...)
      (?collected-mutable-field ...)
      (?collected-immutable-field ...)
      (?collected-mutable-virtual-field ...)
      (?collected-immutable-virtual-field ...)
      (fields		?fie ...)
      (parent		?par ...)
      (protocol		?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	?pad ...)
      (nongenerative	?non ...)
      (virtual-fields (mutable ?field0 ?field ...) ?field-clause ...) ?clause ...)
     (%define-class/sort-clauses
      (?name ...)
      (?collected-mutable-field ...)
      (?collected-immutable-field ...)
      (?collected-mutable-virtual-field ...  (?field0 ?field ...))
      (?collected-immutable-virtual-field ...)
      (fields		?fie ...)
      (parent		?par ...)
      (protocol	?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	?pad ...)
      (nongenerative	?non ...)
      (virtual-fields ?field-clause ...) ?clause ...))

    ;;Gather immutable VIRTUAL-FIELDS clause.
    ((%define-class/sort-clauses
      (?name ...)
      (?collected-mutable-field ...)
      (?collected-immutable-field ...)
      (?collected-mutable-virtual-field ...)
      (?collected-immutable-virtual-field ...)
      (fields		?fie ...)
      (parent		?par ...)
      (protocol		?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	?pad ...)
      (nongenerative	?non ...)
      (virtual-fields (immutable ?field0 ?field ...) ?field-clause ...) ?clause ...)
     (%define-class/sort-clauses
      (?name ...)
      (?collected-mutable-field ...)
      (?collected-immutable-field ...)
      (?collected-mutable-virtual-field ...)
      (?collected-immutable-virtual-field ... (?field0 ?field ...))
      (fields		?fie ...)
      (parent		?par ...)
      (protocol		?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	?pad ...)
      (nongenerative	?non ...)
      (virtual-fields ?field-clause ...) ?clause ...))

    ;;Gather immutable VIRTUAL-FIELDS  clause declared without IMMUTABLE
    ;;auxiliary syntax.
    ((%define-class/sort-clauses
      (?name ...)
      (?collected-mutable-field ...)
      (?collected-immutable-field ...)
      (?collected-mutable-virtual-field ...)
      (?collected-immutable-virtual-field ...)
      (fields		?fie ...)
      (parent		?par ...)
      (protocol		?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	?pad ...)
      (nongenerative	?non ...)
      (virtual-fields ?field ?field-clause ...) ?clause ...)
     (%define-class/sort-clauses
      (?name ...)
      (?collected-mutable-field ...)
      (?collected-immutable-field ...)
      (?collected-mutable-virtual-field ...)
      (?collected-immutable-virtual-field ... (?field))
      (fields		?fie ...)
      (parent		?par ...)
      (protocol		?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	?pad ...)
      (nongenerative	?non ...)
      (fields ?field-clause ...) ?clause ...))

    ;;Remove empty, leftover, VIRTUAL-FIELDS clause.
    ((%define-class/sort-clauses
      (?name ...)
      (?collected-mutable-field ...)
      (?collected-immutable-field ...)
      (?collected-mutable-virtual-field ...)
      (?collected-immutable-virtual-field ...)
      (fields		?fie ...)
      (parent		?par ...)
      (protocol	?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	?pad ...)
      (nongenerative	?non ...)
      (virtual-fields) ?clause ...)
     (%define-class/sort-clauses
      (?name ...)
      (?collected-mutable-field ...)
      (?collected-immutable-field ...)
      (?collected-mutable-virtual-field ...)
      (?collected-immutable-virtual-field ...)
      (fields		?fie ...)
      (parent		?par ...)
      (protocol		?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	?pad ...)
      (nongenerative	?non ...)
      ?clause ...))

    ;;No    more    clauses    to    gather,    hand    everything    to
    ;;%DEFINE-CLASS/TOP-as-PARENT.
    ((%define-class/sort-clauses
      (?name ...)
      (?collected-mutable-field ...)
      (?collected-immutable-field ...)
      (?collected-mutable-virtual-field ...)
      (?collected-immutable-virtual-field ...)
      (fields		?fie ...)
      (parent		?par ...)
      (protocol		?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	?pad ...)
      (nongenerative	?non ...))
     (%define-class/top-as-parent
      (?name ...)
      (?collected-mutable-field ...)
      (?collected-immutable-field ...)
      (?collected-mutable-virtual-field ...)
      (?collected-immutable-virtual-field ...)
      (fields		?fie ...)
      (parent		?par ...)
      (protocol		?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	?pad ...)
      (nongenerative	?non ...)))
    ))

(define-syntax %define-class/top-as-parent
  ;;If  the class  definition used  neither  the PARENT  clause nor  the
  ;;PARENT-RTD clause,  make the type derived by  "<top>".  Finally hand
  ;;everything to %DEFINE-CLASS/ENSURE-NONGENERATIVE.
  ;;
  (syntax-rules ()

    ((_ (?name ...)
	(?collected-mutable-field ...)
	(?collected-immutable-field ...)
	(?collected-mutable-virtual-field ...)
	(?collected-immutable-virtual-field ...)
	(fields		?fie ...)
	(parent)
	(protocol	?pro ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd)
	(nongenerative	?non ...))
     (%define-class/ensure-nongenerative
      (?name ...)
      (?collected-mutable-field ...)
      (?collected-immutable-field ...)
      (?collected-mutable-virtual-field ...)
      (?collected-immutable-virtual-field ...)
      (fields		?fie ...)
      (parent		<top>)
      (protocol		?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd)
      (nongenerative	?non ...)))

    ((_ (?name ...)
	(?collected-mutable-field ...)
	(?collected-immutable-field ...)
	(?collected-mutable-virtual-field ...)
	(?collected-immutable-virtual-field ...)
	(fields		?fie ...)
	(parent		?par ...)
	(protocol	?pro ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...))
     (%define-class/ensure-nongenerative
      (?name ...)
      (?collected-mutable-field ...)
      (?collected-immutable-field ...)
      (?collected-mutable-virtual-field ...)
      (?collected-immutable-virtual-field ...)
      (fields		?fie ...)
      (parent		?par ...)
      (protocol		?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	?pad ...)
      (nongenerative	?non ...)))
    ))

(define-syntax %define-class/ensure-nongenerative
  ;;If the class  definition used no NONGENERATIVE clause,  add an empty
  ;;NONGENERATIVE     clause.      Finally     hand    everything     to
  ;;%DEFINE-CLASS/FILTER-UNUSED.
  ;;
  (syntax-rules ()

    ((_ (?name ...)
	(?collected-mutable-field ...)
	(?collected-immutable-field ...)
	(?collected-mutable-virtual-field ...)
	(?collected-immutable-virtual-field ...)
	(fields		?fie ...)
	(parent		?par ...)
	(protocol	?pro ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative))
     (%define-class/filter-unused
      (?name ...)
      ()	;collected clauses
      (?collected-mutable-field ...)
      (?collected-immutable-field ...)
      (?collected-mutable-virtual-field ...)
      (?collected-immutable-virtual-field ...)
      (fields		?fie ...)
      (parent		?par ...)
      (protocol		?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	?pad ...)
      (nongenerative)))

    ((_ (?name ...)
	(?collected-mutable-field ...)
	(?collected-immutable-field ...)
	(?collected-mutable-virtual-field ...)
	(?collected-immutable-virtual-field ...)
	(fields		?fie ...)
	(parent		?par ...)
	(protocol	?pro ...)
	(sealed		?sea ...)
	(opaque		?opa ...)
	(parent-rtd	?pad ...)
	(nongenerative	?non ...))
     (%define-class/filter-unused
      (?name ...)
      ()	;collected clauses
      (?collected-mutable-field ...)
      (?collected-immutable-field ...)
      (?collected-mutable-virtual-field ...)
      (?collected-immutable-virtual-field ...)
      (fields		?fie ...)
      (parent		?par ...)
      (protocol		?pro ...)
      (sealed		?sea ...)
      (opaque		?opa ...)
      (parent-rtd	?pad ...)
      (nongenerative	?non ...)))
    ))

(define-syntax %define-class/filter-unused
  ;;Removes auxiliary syntaxes which cannot be empty.
  ;;
  (lambda (stx)
    (syntax-case stx (fields parent protocol sealed opaque parent-rtd nongenerative)

      ;;Remove unused PARENT form.
      ((%define-class/filter-unused (?name ...) (?collected-clause ...)
				    (?collected-mutable-field ...)
				    (?collected-immutable-field ...)
				    (?collected-mutable-virtual-field ...)
				    (?collected-immutable-virtual-field ...)
				    (parent)
				    ?clause ...)
       #'(%define-class/filter-unused (?name ...) (?collected-clause ...)
				      (?collected-mutable-field ...)
				      (?collected-immutable-field ...)
				      (?collected-mutable-virtual-field ...)
				      (?collected-immutable-virtual-field ...)
				      ?clause ...))
      ;;Collect used PARENT form.
      ((%define-class/filter-unused (?name ...) (?collected-clause ...)
				    (?collected-mutable-field ...)
				    (?collected-immutable-field ...)
				    (?collected-mutable-virtual-field ...)
				    (?collected-immutable-virtual-field ...)
				    (parent ?e0 ?e ...)
				    ?clause ...)
       #'(%define-class/filter-unused (?name ...) (?collected-clause ... (parent ?e0 ?e ...))
				      (?collected-mutable-field ...)
				      (?collected-immutable-field ...)
				      (?collected-mutable-virtual-field ...)
				      (?collected-immutable-virtual-field ...)
				      ?clause ...))

      ;;Remove unused FIELDS form.
      ((%define-class/filter-unused (?name ...) (?collected-clause ...)
				    (?collected-mutable-field ...)
				    (?collected-immutable-field ...)
				    (?collected-mutable-virtual-field ...)
				    (?collected-immutable-virtual-field ...)
				    (fields)
				    ?clause ...)
       #'(%define-class/filter-unused (?name ...) (?collected-clause ...)
				      (?collected-mutable-field ...)
				      (?collected-immutable-field ...)
				      (?collected-mutable-virtual-field ...)
				      (?collected-immutable-virtual-field ...)
				      ?clause ...))
      ;;Collect used FIELDS form.
      ((%define-class/filter-unused (?name ...) (?collected-clause ...)
				    (?collected-mutable-field ...)
				    (?collected-immutable-field ...)
				    (?collected-mutable-virtual-field ...)
				    (?collected-immutable-virtual-field ...)
				    (fields ?e0 ?e ...)
				    ?clause ...)
       #'(%define-class/filter-unused (?name ...) (?collected-clause ... (fields ?e0 ?e ...))
				      (?collected-mutable-field ...)
				      (?collected-immutable-field ...)
				      (?collected-mutable-virtual-field ...)
				      (?collected-immutable-virtual-field ...)
				      ?clause ...))

      ;;Remove unused PROTOCOL form.
      ((%define-class/filter-unused (?name ...) (?collected-clause ...)
				    (?collected-mutable-field ...)
				    (?collected-immutable-field ...)
				    (?collected-mutable-virtual-field ...)
				    (?collected-immutable-virtual-field ...)
				    (protocol)
				    ?clause ...)
       #'(%define-class/filter-unused (?name ...) (?collected-clause ...)
				      (?collected-mutable-field ...)
				      (?collected-immutable-field ...)
				      (?collected-mutable-virtual-field ...)
				      (?collected-immutable-virtual-field ...)
				      ?clause ...))
      ;;Collect used PROTOCOL form.
      ((%define-class/filter-unused (?name ...) (?collected-clause ...)
				    (?collected-mutable-field ...)
				    (?collected-immutable-field ...)
				    (?collected-mutable-virtual-field ...)
				    (?collected-immutable-virtual-field ...)
				    (protocol ?e0 ?e ...)
				    ?clause ...)
       #'(%define-class/filter-unused (?name ...) (?collected-clause ... (protocol ?e0 ?e ...))
				      (?collected-mutable-field ...)
				      (?collected-immutable-field ...)
				      (?collected-mutable-virtual-field ...)
				      (?collected-immutable-virtual-field ...)
				      ?clause ...))

      ;;Remove unused SEALED form.
      ((%define-class/filter-unused (?name ...) (?collected-clause ...)
				    (?collected-mutable-field ...)
				    (?collected-immutable-field ...)
				    (?collected-mutable-virtual-field ...)
				    (?collected-immutable-virtual-field ...)
				    (sealed)
				    ?clause ...)
       #'(%define-class/filter-unused (?name ...) (?collected-clause ...)
				      (?collected-mutable-field ...)
				      (?collected-immutable-field ...)
				      (?collected-mutable-virtual-field ...)
				      (?collected-immutable-virtual-field ...)
				      ?clause ...))
      ;;Collect used SEALED form.
      ((%define-class/filter-unused (?name ...) (?collected-clause ...)
				    (?collected-mutable-field ...)
				    (?collected-immutable-field ...)
				    (?collected-mutable-virtual-field ...)
				    (?collected-immutable-virtual-field ...)
				    (sealed ?e0 ?e ...) ?clause ...)
       #'(%define-class/filter-unused (?name ...) (?collected-clause ... (sealed ?e0 ?e ...))
				      (?collected-mutable-field ...)
				      (?collected-immutable-field ...)
				      (?collected-mutable-virtual-field ...)
				      (?collected-immutable-virtual-field ...)
				      ?clause ...))

      ;;Remove unused OPAQUE form.
      ((%define-class/filter-unused (?name ...) (?collected-clause ...)
				    (?collected-mutable-field ...)
				    (?collected-immutable-field ...)
				    (?collected-mutable-virtual-field ...)
				    (?collected-immutable-virtual-field ...)
				    (opaque) ?clause ...)
       #'(%define-class/filter-unused (?name ...) (?collected-clause ...)
				      (?collected-mutable-field ...)
				      (?collected-immutable-field ...)
				      (?collected-mutable-virtual-field ...)
				      (?collected-immutable-virtual-field ...)
				      ?clause ...))
      ;;Collect used OPAQUE form.
      ((%define-class/filter-unused (?name ...) (?collected-clause ...)
				    (?collected-mutable-field ...)
				    (?collected-immutable-field ...)
				    (?collected-mutable-virtual-field ...)
				    (?collected-immutable-virtual-field ...)
				    (opaque ?e0 ?e ...) ?clause ...)
       #'(%define-class/filter-unused (?name ...) (?collected-clause ... (opaque ?e0 ?e ...))
				      (?collected-mutable-field ...)
				      (?collected-immutable-field ...)
				      (?collected-mutable-virtual-field ...)
				      (?collected-immutable-virtual-field ...)
				      ?clause ...))

      ;;Remove unused PARENT-RTD form.
      ((%define-class/filter-unused (?name ...) (?collected-clause ...)
				    (?collected-mutable-field ...)
				    (?collected-immutable-field ...)
				    (?collected-mutable-virtual-field ...)
				    (?collected-immutable-virtual-field ...)
				    (parent-rtd) ?clause ...)
       #'(%define-class/filter-unused (?name ...) (?collected-clause ...)
				      (?collected-mutable-field ...)
				      (?collected-immutable-field ...)
				      (?collected-mutable-virtual-field ...)
				      (?collected-immutable-virtual-field ...)
				      ?clause ...))
      ;;Collect used PARENT-RTD form.
      ((%define-class/filter-unused (?name ...) (?collected-clause ...)
				    (?collected-mutable-field ...)
				    (?collected-immutable-field ...)
				    (?collected-mutable-virtual-field ...)
				    (?collected-immutable-virtual-field ...)
				    (parent-rtd ?e0 ?e ...) ?clause ...)
       #'(%define-class/filter-unused (?name ...) (?collected-clause ... (parent-rtd ?e0 ?e ...))
				      (?collected-mutable-field ...)
				      (?collected-immutable-field ...)
				      (?collected-mutable-virtual-field ...)
				      (?collected-immutable-virtual-field ...)
				      ?clause ...))

      ;;Remove unused NONGENERATIVE form.
      ((%define-class/filter-unused (?name ...) (?collected-clause ...)
				    (?collected-mutable-field ...)
				    (?collected-immutable-field ...)
				    (?collected-mutable-virtual-field ...)
				    (?collected-immutable-virtual-field ...)
				    (nongenerative)
				    ?clause ...)
       #'(%define-class/filter-unused (?name ...) (?collected-clause ...)
				      (?collected-mutable-field ...)
				      (?collected-immutable-field ...)
				      (?collected-mutable-virtual-field ...)
				      (?collected-immutable-virtual-field ...)
				      ?clause ...))
      ;;Collect empty-but-used NONGENERATIVE form.
      ((%define-class/filter-unused (?name ...) (?collected-clause ...)
				    (?collected-mutable-field ...)
				    (?collected-immutable-field ...)
				    (?collected-mutable-virtual-field ...)
				    (?collected-immutable-virtual-field ...)
				    (nongenerative #t) ?clause ...)
       #'(%define-class/filter-unused (?name ...) (?collected-clause ... (nongenerative))
				      (?collected-mutable-field ...)
				      (?collected-immutable-field ...)
				      (?collected-mutable-virtual-field ...)
				      (?collected-immutable-virtual-field ...)
				      ?clause ...))
      ;;Collect used NONGENERATIVE form.
      ((%define-class/filter-unused (?name ...) (?collected-clause ...)
				    (?collected-mutable-field ...)
				    (?collected-immutable-field ...)
				    (?collected-mutable-virtual-field ...)
				    (?collected-immutable-virtual-field ...)
				    (nongenerative ?e0 ?e ...)
				    ?clause ...)
       #'(%define-class/filter-unused (?name ...) (?collected-clause ... (nongenerative ?e0 ?e ...))
				      (?collected-mutable-field ...)
				      (?collected-immutable-field ...)
				      (?collected-mutable-virtual-field ...)
				      (?collected-immutable-virtual-field ...)
				      ?clause ...))

      ;;No    more    clauses   to    filter,    hand   everything    to
      ;;%DEFINE-CLASS/EXPAND-FIELD-NAMES.
      ((%define-class/filter-unused (?name ?constructor ?predicate)
				    (?collected-clause ...)
				    (?collected-mutable-field ...)
				    (?collected-immutable-field ...)
				    (?collected-mutable-virtual-field ...)
				    (?collected-immutable-virtual-field ...))
       #'(begin
	   (define-record-type (?name ?constructor ?predicate) ?collected-clause ...)
	   (%define-class/expand-field-names ?name
					     ()	;expanded mutable fields
					     ()	;expanded immutable fields
					     ()	;expanded virtual mutable fields
					     ()	;expanded virtual immutable fields
					     (?collected-mutable-field ...)
					     (?collected-immutable-field ...)
					     (?collected-mutable-virtual-field ...)
					     (?collected-immutable-virtual-field ...))))
      )))

(define-syntax %define-class/expand-field-names
  ;;Expand the collected  lists of fields to have  explicit accessor and
  ;;mutator names.
  ;;
  (lambda (stx)
    (define (%accessor name field)
      (string->symbol (string-append name "-" field)))
    (define (%mutator name field)
      (string->symbol (string-append name "-" field "-set!")))
    (syntax-case stx ()

      ;;NOTE:  mutable  field  clauses  either have  both  accessor  and
      ;;mutator names or  have none; they cannot have  only the accessor
      ;;name.

      ;;Expand mutable field clause with neither accessor nor mutator names.
      ((_ ?name
	  (?expanded-mutable-field ...)
	  (?expanded-immutable-field ...)
	  (?expanded-virtual-mutable-field ...)
	  (?expanded-virtual-immutable-field ...)
	  ((?mutable-name) ?mutable-field ...)
	  (?immutable-field ...)
	  (?virtual-mutable-field ...)
	  (?virtual-immutable-field ...))
       (let ((name  (symbol->string (syntax->datum #'?name)))
	     (field (symbol->string (syntax->datum #'?mutable-name))))
	 (with-syntax ((ACCESSOR (datum->syntax #'?name (%accessor name field)))
		       (MUTATOR  (datum->syntax #'?name (%mutator  name field))))
	   #'(%define-class/expand-field-names
	      ?name
	      (?expanded-mutable-field ... (?mutable-name ACCESSOR MUTATOR))
	      (?expanded-immutable-field ...)
	      (?expanded-virtual-mutable-field ...)
	      (?expanded-virtual-immutable-field ...)
	      (?mutable-field ...)
	      (?immutable-field ...)
	      (?virtual-mutable-field ...)
	      (?virtual-immutable-field ...)))))

      ;;Pass through mutable field clause with accessor and mutator name.
      ((_ ?name
	  (?expanded-mutable-field ...)
	  (?expanded-immutable-field ...)
	  (?expanded-virtual-mutable-field ...)
	  (?expanded-virtual-immutable-field ...)
	  ((?mutable-name ?mutable-accessor ?mutable-mutator) ?mutable-field ...)
	  (?immutable-field ...)
	  (?virtual-mutable-field ...)
	  (?virtual-immutable-field ...))
       #'(%define-class/expand-field-names
	  ?name
	  (?expanded-mutable-field ... (?mutable-name ?mutable-accessor ?mutable-mutator))
	  (?expanded-immutable-field ...)
	  (?expanded-virtual-mutable-field ...)
	  (?expanded-virtual-immutable-field ...)
	  (?mutable-field ...)
	  (?immutable-field ...)
	  (?virtual-mutable-field ...)
	  (?virtual-immutable-field ...)))

      ;;Expand immutable field clause without accessor name.
      ((_ ?name
	  (?expanded-mutable-field ...)
	  (?expanded-immutable-field ...)
	  (?expanded-virtual-mutable-field ...)
	  (?expanded-virtual-immutable-field ...)
	  ()	;no more mutable field clauses
	  ((?immutable-name) ?immutable-field ...)
	  (?virtual-mutable-field ...)
	  (?virtual-immutable-field ...))
       (let ((name  (symbol->string (syntax->datum #'?name)))
	     (field (symbol->string (syntax->datum #'?immutable-name))))
	 (with-syntax ((ACCESSOR (datum->syntax #'?name (%accessor name field))))
	   #'(%define-class/expand-field-names
	      ?name
	      (?expanded-mutable-field ...)
	      (?expanded-immutable-field ... (?immutable-name ACCESSOR))
	      (?expanded-virtual-mutable-field ...)
	      (?expanded-virtual-immutable-field ...)
	      () ;no more mutable field clauses
	      (?immutable-field ...)
	      (?virtual-mutable-field ...)
	      (?virtual-immutable-field ...)))))

      ;;Pass through immutable field clause with accessor name.
      ((_ ?name
	  (?expanded-mutable-field ...)
	  (?expanded-immutable-field ...)
	  (?expanded-virtual-mutable-field ...)
	  (?expanded-virtual-immutable-field ...)
	  ()	;no more mutable field clauses
	  ((?immutable-name ?immutable-accessor) ?immutable-field ...)
	  (?virtual-mutable-field ...)
	  (?virtual-immutable-field ...))
       #'(%define-class/expand-field-names
	  ?name
	  (?expanded-mutable-field ...)
	  (?expanded-immutable-field ... (?immutable-name ?immutable-accessor))
	  (?expanded-virtual-mutable-field ...)
	  (?expanded-virtual-immutable-field ...)
	  ()	;no more mutable field clauses
	  (?immutable-field ...)
	  (?virtual-mutable-field ...)
	  (?virtual-immutable-field ...)))

      ;;Expand virtual mutable field clause with neither accessor nor mutator names.
      ((_ ?name
	  (?expanded-mutable-field ...)
	  (?expanded-immutable-field ...)
	  (?expanded-virtual-mutable-field ...)
	  (?expanded-virtual-immutable-field ...)
	  ()	;no more mutable field clauses
	  ()	;no more immutable field clauses
	  ((?virtual-mutable-name) ?virtual-mutable-field ...)
	  (?virtual-immutable-field ...))
       (let ((name  (symbol->string (syntax->datum #'?name)))
	     (field (symbol->string (syntax->datum #'?virtual-mutable-name))))
	 (with-syntax ((ACCESSOR (datum->syntax #'?name (%accessor name field)))
		       (MUTATOR  (datum->syntax #'?name (%mutator  name field))))
	   #'(%define-class/expand-field-names
	      ?name
	      (?expanded-mutable-field ...)
	      (?expanded-immutable-field ...)
	      (?expanded-virtual-mutable-field ... (?virtual-mutable-name ACCESSOR MUTATOR))
	      (?expanded-virtual-immutable-field ...)
	      ()	;no more mutable field clauses
	      ()	;no more immutable field clauses
	      (?virtual-mutable-field ...)
	      (?virtual-immutable-field ...)))))

      ;;Pass through virtual mutable field clause with accessor and mutator name.
      ((_ ?name
	  (?expanded-mutable-field ...)
	  (?expanded-immutable-field ...)
	  (?expanded-virtual-mutable-field ...)
	  (?expanded-virtual-immutable-field ...)
	  ()	;no more mutable field clauses
	  ()	;no more immutable field clauses
	  ((?virtual-mutable-name ?virtual-mutable-accessor ?virtual-mutable-mutator)
	   ?virtual-mutable-field ...)
	  (?virtual-immutable-field ...))
       #'(%define-class/expand-field-names
	  ?name
	  (?expanded-mutable-field ...)
	  (?expanded-immutable-field ...)
	  (?expanded-virtual-mutable-field ... (?virtual-mutable-name
						?virtual-mutable-accessor
						?virtual-mutable-mutator))
	  (?expanded-virtual-immutable-field ...)
	  ()	;no more mutable field clauses
	  ()	;no more immutable field clauses
	  (?virtual-mutable-field ...)
	  (?virtual-immutable-field ...)))

      ;;Expand virtual immutable field clause without accessor name.
      ((_ ?name
	  (?expanded-mutable-field ...)
	  (?expanded-immutable-field ...)
	  (?expanded-virtual-mutable-field ...)
	  (?expanded-virtual-immutable-field ...)
	  ()	;no more mutable field clauses
	  ()	;no more immutable field clauses
	  ()	;no more virtual mutable field clauses
	  ((?virtual-immutable-name) ?virtual-immutable-field ...))
       (let ((name  (symbol->string (syntax->datum #'?name)))
	     (field (symbol->string (syntax->datum #'?virtual-immutable-name))))
	 (with-syntax ((ACCESSOR (datum->syntax #'?name (%accessor name field))))
	   #'(%define-class/expand-field-names
	      ?name
	      (?expanded-mutable-field ...)
	      (?expanded-immutable-field ...)
	      (?expanded-virtual-mutable-field ...)
	      (?expanded-virtual-immutable-field ... (?virtual-immutable-name ACCESSOR))
	      ()	;no more mutable field clauses
	      ()	;no more immutable field clauses
	      ()	;no more virtual mutable field clauses
	      (?virtual-immutable-field ...)))))

      ;;Pass through immutable field clause with accessor name.
      ((_ ?name
	  (?expanded-mutable-field ...)
	  (?expanded-immutable-field ...)
	  (?expanded-virtual-mutable-field ...)
	  (?expanded-virtual-immutable-field ...)
	  ()	;no more mutable field clauses
	  ()	;no more immutable field clauses
	  ()	;no more virtual mutable field clauses
	  ((?virtual-immutable-name ?virtual-immutable-accessor) ?virtual-immutable-field ...))
       #'(%define-class/expand-field-names
	  ?name
	  (?expanded-mutable-field ...)
	  (?expanded-immutable-field ...)
	  (?expanded-virtual-mutable-field ...)
	  (?expanded-virtual-immutable-field ... (?virtual-immutable-name ?virtual-immutable-accessor))
	  ()	;no more mutable field clauses
	  ()	;no more immutable field clauses
	  ()	;no more virtual mutable field clauses
	  (?virtual-immutable-field ...)))

      ;;No    more    lists    to    expand,    hand    everything    to
      ;;%DEFINE-CLASS/OUTPUT-FORMS.
      ((_ ?name
	  (?expanded-mutable-field ...)
	  (?expanded-immutable-field ...)
	  (?expanded-virtual-mutable-field ...)
	  (?expanded-virtual-immutable-field ...)
	  ()	;no more mutable field clauses
	  ()	;no more immutable field clauses
	  ()	;no more virtual mutable field clauses
	  ())	;no more virtual immutable field clauses
       #'(%define-class/output-forms ?name
				     (?expanded-mutable-field ...)
				     (?expanded-immutable-field ...)
				     (?expanded-virtual-mutable-field ...)
				     (?expanded-virtual-immutable-field ...)))
      )))

(define-syntax %define-class/output-forms
  (lambda (stx)
    (define (%accessor name)
      (string->symbol (string-append "with-record-fields-of-" (symbol->string name))))
    (syntax-case stx ()
      ((_ ?name
	  (?expanded-mutable-field ...)
	  (?expanded-immutable-field ...)
	  (?expanded-virtual-mutable-field ...)
	  (?expanded-virtual-immutable-field ...))
       (with-syntax ((ACCESSOR (datum->syntax #'?name (%accessor (syntax->datum #'?name)))))
	 #'(define-syntax ACCESSOR
	     (syntax-rules ()
	       ((_ ?name ?body0 ?body (... ...))
		(%with-record-fields ?name (?expanded-mutable-field
					    ...
					    ?expanded-immutable-field ...
					    ?expanded-virtual-mutable-field ...
					    ?expanded-virtual-immutable-field ...)
				     ?body0 ?body (... ...))))))))))

(define-syntax %with-record-fields
  (lambda (stx)
    (define (%field name field)
      (string->symbol (string-append (symbol->string name) "." (symbol->string field))))
    (syntax-case stx ()

      ;;Process a field clause with both accessor and mutator.
      ((_ ?name ((?field ?accessor ?mutator) ?clause ...) ?body0 ?body ...)
       (with-syntax ((FIELD (datum->syntax #'?name (%field (syntax->datum #'?name)
							   (syntax->datum #'?field)))))
	 #'(with-accessor-and-mutator ((FIELD ?name ?accessor ?mutator))
				      (%with-record-fields ?name (?clause ...) ?body0 ?body ...))))

      ;;Process a field clause with accessor only.
      ((_ ?name ((?field ?accessor) ?clause ...) ?body0 ?body ...)
       (with-syntax ((FIELD (datum->syntax #'?name (%field (syntax->datum #'?name)
							   (syntax->datum #'?field)))))
	 #'(with-accessor-and-mutator ((FIELD ?name ?accessor))
				      (%with-record-fields ?name (?clause ...) ?body0 ?body ...))))

      ;;No more field clauses, output the body.
      ((_ ?name () ?body0 ?body ...)
       #'(begin ?body0 ?body ...))
      )))


;;;; fields access syntaxes

(define-syntax with-fields
  (lambda (stx)
    (define (%accessor class)
      (string->symbol (string-append "with-record-fields-of-" (symbol->string class))))
    (syntax-case stx ()

      ((_ ((?class ?name) ?clause ...) ?body0 ?body ...)
       (with-syntax ((ACCESSOR (datum->syntax #'?class (%accessor (syntax->datum #'?class)))))
	 #'(ACCESSOR ?name (with-fields (?clause ...) ?body0 ?body ...))))

      ((_ () ?body0 ?body ...)
       #'(begin ?body0 ?body ...)))))

(define-syntax let-fields
  (syntax-rules ()
    ((_ (((?var ?class) ?init) ...) ?body0 ?body ...)
     (let ((?var ?init) ...)
       (with-fields ((?class ?var) ...) ?body0 ?body ...)))

    ((_ ((?var ?init) ...) ?body0 ?body ...)
     (let ((?var ?init) ...) ?body0 ?body ...))))

(define-syntax let*-fields
  (syntax-rules ()
    ((_ (((?var0 ?class0) ?init0) ((?var ?class) ?init) ...)
	?body0 ?body ...)
     (let ((?var0 ?init0))
       (with-fields ((?class0 ?var0))
	 (let*-fields (((?var ?class) ?init) ...)
	   ?body0 ?body ...))))

    ((_ ((?var ?init) ...) ?body0 ?body ...)
     (let* ((?var ?init) ...) ?body0 ?body ...))))

(define-syntax letrec-fields
  (syntax-rules ()
    ((_ (((?var ?class) ?init) ...) ?body0 ?body ...)
     (let ((?var #f) ...)
       (with-fields ((?class ?var) ...)
	 (set! ?var ?init) ...
	 ?body0 ?body ...)))

    ((_ ((?var ?init) ...) ?body0 ?body ...)
     (letrec ((?var ?init) ...) ?body0 ?body ...))))

(define-syntax letrec*-fields
  (syntax-rules ()
    ((_ (((?var ?class) ?init) ...) ?body0 ?body ...)
     (let ((?var #f) ...)
       (with-fields ((?class ?var) ...)
	 (set! ?var ?init) ...
	 ?body0 ?body ...)))

    ((_ ((?var ?init) ...) ?body0 ?body ...)
     (letrec* ((?var ?init) ...) ?body0 ?body ...))))

(define-syntax define/with
  (syntax-rules ()
    ((_ (?name (?arg ?class) ...) ?body0 ?body ...)
     (define (?name ?arg ...)
       (with-fields ((?class ?arg) ...)
	 ?body0 ?body ...)))

    ((_ (?name ?arg ...) ?body0 ?body ...)
     (define (?name ?arg ...) ?body0 ?body ...))

    ((_ ?name ?expr)
     (define ?name ?expr))

    ((_ ?name)
     (define ?name))))


(define-record-type <top>
  (nongenerative nausicaa:builtin:<top>))

(define-syntax with-record-fields-of-<top>
  (syntax-rules ()
    ((_ ?name ?body0 ?body ...)
     (begin ?body0 ?body ...))))

(define-class <builtin>
  (nongenerative nausicaa:builtin:<builtin>))

;;; --------------------------------------------------------------------

(define-syntax define-builtin-class
  (lambda (stx)
    (define (%uid name)
      (string->symbol (string-append "nausicaa:builtin:" (symbol->string name))))
    (syntax-case stx ()
      ((_ ?name ?clause ...)
       (with-syntax ((UID (datum->syntax #'?name (%uid (syntax->datum #'?name)))))
	 #'(define-class ?name
	     (parent <builtin>)
	     (nongenerative UID)
	     ?clause ...))))))

(define-builtin-class <pair>
  (virtual-fields (immutable car car)
		  (immutable cdr cdr)))

(define-builtin-class <list>
  (virtual-fields (immutable car car)
		  (immutable cdr cdr)
		  (immutable length length)))

(define-builtin-class <char>
  (virtual-fields (immutable upcase	char-upcase)
		  (immutable downcase	char-downcase)
		  (immutable titlecase	char-titlecase)
		  (immutable foldcase	char-foldcase)))

(define-builtin-class <string>
  (virtual-fields (immutable length	string-length)
		  (immutable upcase	string-upcase)
		  (immutable downcase	string-downcase)
		  (immutable titlecase	string-titlecase)
		  (immutable foldcase	string-foldcase)))

(define-builtin-class <vector>
  (virtual-fields (immutable length vector-length)))

(define-builtin-class <bytevector>
  (virtual-fields (immutable length bytevector-length)))

(define-builtin-class <hashtable>
  (virtual-fields (immutable size hashtable-size)
		  (immutable keys hashtable-keys)
		  (immutable entries hashtable-entries)))

;;; --------------------------------------------------------------------

(define-builtin-class <record>)

(define-builtin-class <condition>
  (virtual-fields (immutable message	condition-message)
		  (immutable who	condition-who)
		  (immutable irritants	condition-irritants)))

;;; --------------------------------------------------------------------

(define-builtin-class <port>
  (virtual-fields (immutable transcoder port-transcoder)
		  (immutable textual? textual-port?)
		  (immutable binary? binary-port?)
		  (immutable has-port-position? port-has-port-position?)
		  (immutable has-set-port-position? port-has-set-port-position!?)
		  (mutable port-position port-position set-port-position!)
		  (immutable eof? port-eof?)
		  (immutable input? input-port?)
		  (immutable output? output-port?)))

(define-class <input-port>
  (parent <port>)
  (nongenerative nausicaa:builtin:<input-port>))

(define-class <output-port>
  (parent <port>)
  (nongenerative nausicaa:builtin:<output-port>))

(define-class <binary-port>
  (parent <port>)
  (nongenerative nausicaa:builtin:<binary-port>))

(define-class <textual-port>
  (parent <port>)
  (nongenerative nausicaa:builtin:<textual-port>))

;;; --------------------------------------------------------------------

(define-builtin-class <number>
  (virtual-fields (immutable exact	exact)
		  (immutable inexact	inexact)

		  (immutable exact?	exact?)
		  (immutable inexact?	inexact?)

		  (immutable zero?	zero?)
		  (immutable positive?	positive?)
		  (immutable negative?	negative?)

		  (immutable odd?	odd?)
		  (immutable even?	even?)

		  (immutable finite?	finite?)
		  (immutable infinite?	infinite?)
		  (immutable nan?	nan?)

		  (immutable real-part	real-part)
		  (immutable imag-part	imag-part)
		  (immutable magnitude	magnitude)
		  (immutable angle	angle)

		  (immutable numerator	numerator)
		  (immutable denominator denominator)

		  (immutable floor	floor)
		  (immutable ceiling	ceiling)
		  (immutable truncate	truncate)
		  (immutable round	round)))

(define-class <complex>
  (parent <number>)
  (nongenerative nausicaa:builtin:<complex>))

(define-class <real-valued>
  (parent <complex>)
  (nongenerative nausicaa:builtin:<real-valued>))

(define-class <real>
  (parent <real-valued>)
  (nongenerative nausicaa:builtin:<real>))

(define-class <rational-valued>
  (parent <real>)
  (nongenerative nausicaa:builtin:<rational-valued>))

(define-class <flonum>
  (parent <real>)
  (nongenerative nausicaa:builtin:<flonum>))

(define-class <rational>
  (parent <rational-valued>)
  (nongenerative nausicaa:builtin:<rational>))

(define-class <integer-valued>
  (parent <rational-valued>)
  (nongenerative nausicaa:builtin:<integer-valued>))

(define-class <integer>
  (parent <integer-valued>)
  (nongenerative nausicaa:builtin:<integer>))

(define-class <fixnum>
  (parent <integer>)
  (nongenerative nausicaa:builtin:<fixnum>))


;;;; constructors

(define make-record-maker
  (case-lambda
   ((rtd)
    (make-record-maker rtd #f))
   ((rtd init)
    (let ((init-values (make-list (fold-left
				   (lambda (sum rtd)
				     (+ sum (vector-length (record-type-field-names rtd))))
				   0
				   (record-parent-list rtd))
				  init))
	  (maker	(record-constructor (make-record-constructor-descriptor rtd #f #f))))
      (lambda ()
	(apply maker init-values))))))

(define-syntax make-record-maker*
  (syntax-rules ()
    ((_ ?record-name)
     (make-record-maker (record-type-descriptor ?record-name)))
    ((_ ?record-name ?init)
     (make-record-maker (record-type-descriptor ?record-name) ?init))))

(define-syntax make
  (syntax-rules ()
    ((_ ?record-name ?arg ...)
     (let-syntax
	 ((dummy (lambda (stx)
		   #`((quote #,(record-constructor
				(record-constructor-descriptor ?record-name)))
		      ?arg ...))))
       (dummy)))))


;;;; predicates

(define (record-type-parent? rtd1 rtd2)
  (cond ((eq? (record-type-uid rtd1) (record-type-uid rtd2))	#t)
	((eq? (record-type-uid rtd1) (record-type-uid (record-type-descriptor <top>)))   #f)
   	((eq? (record-type-uid rtd2) (record-type-uid (record-type-descriptor <top>)))   #t)
	(else
	 (memq (record-type-uid rtd2) (map record-type-uid (record-parent-list rtd1))))))

(define (record-is-a? obj rtd)
  (eq? (record-type-uid rtd) (record-type-uid (record-type-of obj))))

(define-syntax is-a?
  (syntax-rules ()
    ((_ ?obj ?record-name)
     ((%record-predicate (record-type-descriptor ?record-name)) ?obj))))

(define (%record-predicate rtd)
  ;;Return the  record type predicate  associated to RTD.   Support both
  ;;normal record types and conventional record types.
  ;;
  (case (record-type-name rtd)
    ((<fixnum>)			fixnum?)
    ((<integer>)		integer?)
    ((<rational>)		rational?)
    ((<integer-valued>)		integer-valued?)
    ((<rational-valued>)	rational-valued?)
    ((<flonum>)			flonum?)
    ((<real>)			real?)
    ((<real-valued>)		real-valued?)
    ((<complex>)		complex?)
    ((<number>)			number?)

    ((<char>)			char?)
    ((<string>)			string?)
    ((<vector>)			vector?)
    ((<bytevector>)		bytevector?)
    ((<hashtable>)		hashtable?)

    ((<input-port>)		input-port?)
    ((<output-port>)		output-port?)
    ((<binary-port>)		(lambda (obj)
				  (and (port? obj) (binary-port? obj))))
    ((<textual-port>)		(lambda (obj)
				  (and (port? obj) (textual-port? obj))))
    ((<port>)			port?)

    ((<condition>)		condition?)
    ((<record>)			record?)
    ((<pair>)			pair?)
    ((<list>)			list?)

    (else
     (record-predicate rtd))))


;;;; inspection

(define-syntax record-parent-list*
  (syntax-rules ()
    ((_ ?record-name)
     (record-parent-list (record-type-descriptor ?record-name)))))

(define (record-parent-list rtd)
  (let loop ((cls (list rtd))
	     (rtd (record-type-parent rtd)))
    (if rtd
	(loop (cons rtd cls) (record-type-parent rtd))
      (reverse cls))))

(define (record-type-of obj)
  ;;Return the  record type  descriptor associated to  OBJ, if obj  is a
  ;;RECORD; else  return the RTD  of <top>.  The  order of the  tests is
  ;;important.  More specialised types must come first.
  ;;
  (cond

   ;;This  is  here  as  a  special exception  because  in  Larceny  the
   ;;hashtable  is a  record.  We  have  to process  it before  applying
   ;;RECORD?
   ((hashtable?	obj)		(record-type-descriptor <hashtable>))

   ((record? obj)
    (record-rtd obj))

   ((number? obj)
    ;;Order does matter here!!!
    (cond ((fixnum?		obj)	(record-type-descriptor <fixnum>))
	  ((integer?		obj)	(record-type-descriptor <integer>))
	  ((rational?		obj)	(record-type-descriptor <rational>))
	  ((integer-valued?	obj)	(record-type-descriptor <integer-valued>))
	  ((rational-valued?	obj)	(record-type-descriptor <rational-valued>))
	  ((flonum?		obj)	(record-type-descriptor <flonum>))
	  ((real?		obj)	(record-type-descriptor <real>))
	  ((real-valued?	obj)	(record-type-descriptor <real-valued>))
	  ((complex?		obj)	(record-type-descriptor <complex>))
	  (else				(record-type-descriptor <number>))))
   ((char?		obj)		(record-type-descriptor <char>))
   ((string?		obj)		(record-type-descriptor <string>))
   ((vector?		obj)		(record-type-descriptor <vector>))
   ((bytevector?	obj)		(record-type-descriptor <bytevector>))
   ((port?		obj)
    ;;Order here is arbitrary.
    (cond ((input-port?		obj)	(record-type-descriptor <input-port>))
	  ((output-port?	obj)	(record-type-descriptor <output-port>))
	  ((binary-port?	obj)	(record-type-descriptor <binary-port>))
	  ((textual-port?	obj)	(record-type-descriptor <textual-port>))
	  (else				(record-type-descriptor <port>))))
   ((condition?		obj)		(record-type-descriptor <condition>))
   ((record?		obj)		(record-type-descriptor <record>))
   ((pair?		obj)
    ;;Order does matter  here!!!  Better leave these at  the end because
    ;;qualifying a long list can be time-consuming.
    (cond ((list?	obj)	(record-type-descriptor <list>))
	  (else			(record-type-descriptor <pair>))))
   (else (record-type-descriptor <top>))))


;;;; next method implementation

(define next-method-func-parm (make-parameter #f))
(define next-method-pred-parm (make-parameter #f))

(define-syntax call-next-method
  (syntax-rules ()
    ((_)
     (let ((f (next-method-func-parm)))
       (if f (f)
	 (assertion-violation 'call-next-method
	   "invoked call-next-method outside of a generic function"))))))

(define-syntax next-method?
  (syntax-rules ()
    ((_)
     (let ((f (next-method-pred-parm)))
       (if f (f)
	 (assertion-violation 'call-next-method
	   "invoked next-method? outside of a generic function"))))))


;;;; generic functions

(define-record-type special-argument
  (opaque #t)
  (sealed #t)
  (nongenerative))

(define :method-adder
  (make-special-argument))

(define :method-alist
  (make-special-argument))

(define :method-alist-set!
  (make-special-argument))

(define-syntax define-generic
  (syntax-rules ()
    ((_ ?name)
     (define ?name (make-generic-function)))))

(define-syntax define-generic/merge
  (syntax-rules ()
    ((_ ?name ?gf0 ?gf ...)
     (define ?name
       (merge-generic-functions ?gf0 ?gf ...)))))

(define (make-generic-function)
  (let* ((method-alist	'())
	 (cache		#f)
	 (method-adder	(lambda (signature has-rest closure)
			  (set! method-alist
				(%add-method-to-method-alist method-alist
							     signature has-rest closure)))))
    (define-syntax %assert-no-methods
      (syntax-rules ()
	((_ ?signature)
	 (assertion-violation #f "no method defined for the argument's types"
			      (map record-type-name ?signature)))
	((_)
	 (assertion-violation #f "no method defined for the argument's types"))))

    ;; (case-lambda
    ;;  (()
    ;;   (cond (method-with-no-args
    ;; 	     (method-with-no-args))
    ;; 	    (method-with-no-args-and-rest
    ;; 	     (method-with-no-args-and-rest))
    ;; 	    (else
    ;; 	     (%assert-no-methods))))
    ;;  ((arg)
    ;;   (cond ((eq? arg :method-adder)
    ;; 	     method-adder)
    ;; 	    ((eq? arg :method-alist)
    ;; 	     method-alist)))
    ;;  )

    (lambda args
      (if (and (pair? args)
      	       (special-argument? (car args)))
      	  (let ((arg (car args)))
      	    (cond ((eq? arg :method-adder)
		   (when cache
		     (hashtable-clear! cache))
      		   method-adder)
		  ((eq? arg :method-alist)
      		   method-alist)
		  ((eq? arg :method-alist-set!)
		   (when cache
		     (hashtable-clear! cache))
      		   (set! method-alist (cadr args)))
      		  (else
      		   (assertion-violation #f "internal error with invalid special argument" arg))))
	(let-syntax
	    ((apply-function/stx (syntax-rules ()
				   ((_ ?closure)
				    (apply ?closure args))))
	     (consume-closure (syntax-rules ()
				((_ ?closure-list)
				 (begin0
				     (car ?closure-list)
				   (set! ?closure-list (cdr ?closure-list)))))))
	  (letrec*
	      ((signature
		(map record-type-of args))

	       (applicable-methods
		(cond ((and cache (hashtable-ref cache signature #f))
		       => (lambda (methods) methods))
		      (else
		       (let ((methods (%compute-applicable-methods signature method-alist)))
			 (unless cache
			   (set! cache (make-hashtable signature-hash eq?)))
			 (hashtable-set! cache signature methods)
			 methods))))

	       (method-called?  #f)

	       (is-a-next-method-available?
		(lambda ()
		  (null? applicable-methods)))

	       (apply-function
		(lambda (f) (apply-function/stx f)))

	       (call-methods
		(lambda ()
		  (cond ((pair? applicable-methods)
			 (unless method-called?
			   (set! method-called? #t))
			 (apply-function/stx (consume-closure applicable-methods)))
			(method-called?
			 (assertion-violation #f
			   "called next method but no more methods available"))
			(else
			 (%assert-no-methods signature))))))
	    (parametrise ((next-method-func-parm call-methods)
			  (next-method-pred-parm is-a-next-method-available?))
	      (call-methods))))))))


;;;; syntaxes to define and add methods

(define-syntax declare-method
  ;;Define a new method and store it in the given generic function.
  ;;
  (syntax-rules ()

    ;;This is for the syntax:
    ;;
    ;;	(declare-method (doit (a <alpha>) (b <beta>))
    ;;	  ---)
    ;;
    ((_ (?generic-function . ?args) . ?body)
     (%collect-types-and-arguments ?generic-function ?args () () . ?body))

    ;;This is for the syntax:
    ;;
    ;;	(declare-method doit ((a <alpha>) (b <beta>))
    ;;	  ---)
    ;;
    ((_ ?generic-function ?args . ?body)
     (%collect-types-and-arguments ?generic-function ?args () () . ?body))))

(define-syntax %collect-types-and-arguments
  ;;Analyse the list  of method arguments collecting a  list of names, a
  ;;list of types and a boolean representing the rest argument.  Finally
  ;;call the ADD-METHOD syntax to add the method.
  ;;
  (syntax-rules ()
    ((_ ?generic-function ((?next-arg-name ?next-record-name) . ?args)
	(?record-name ...)
	(?arg-name    ...) . ?body)
     ;;Matches the  form when  the next argument  to be processed  has a
     ;;type.
     (%collect-types-and-arguments ?generic-function ?args
				   (?record-name ... ?next-record-name)
				   (?arg-name    ... ?next-arg-name)
				   . ?body))

    ((_ ?generic-function (?next-arg-name . ?args) (?record-name ...) (?arg-name ...) . ?body)
     ;;Matches the  form when the next  argument to be  processed has no
     ;;type.
     (%collect-types-and-arguments ?generic-function ?args
				   (?record-name ... <top>)
				   (?arg-name    ... ?next-arg-name)
				   . ?body))

    ((_ ?generic-function () (?record-name ...) (?arg-name ...) . ?body)
     ;;Matches the form  when all the arguments have  been processed and
     ;;NO  rest argument  is present.   This  MUST come  before the  one
     ;;below.
     (add-method ?generic-function (?record-name ...)
		 #f ;means no rest argument
		 (lambda (?arg-name ...) . ?body)))

    ((_ ?generic-function ?rest-name (?record-name ...) (?arg-name ...) . ?body)
     ;;Matches the form  when all the arguments have  been processed and
     ;;only the  rest argument is there.   This MUST come  after the one
     ;;above.
     (add-method ?generic-function (?record-name ...)
		 #t ;means rest argument is present
		 (lambda (?arg-name ... . ?rest-name) . ?body)))))

(define-syntax add-method
  (syntax-rules ()
    ((_ ?generic-function (?record-name ...) ?has-rest ?closure)
     ((?generic-function :method-adder)
      (list (record-type-descriptor ?record-name) ...) ;this is the signature
      ?has-rest ?closure))))


;;;; method alists
;;
;;The  collection of methods  in a  generic function  is an  alist; each
;;entry has the format:
;;
;;	((has-rest . signature) . closure)
;;
;;the key is a pair whose CAR  is the HAS-REST boolean, and whose CDR is
;;the  SIGNATURE of  the  method;  this allows  two  methods with  equal
;;signatures to be distinct if one supports rest arguments and the other
;;does not.
;;

(define (make-method-entry-key has-rest signature)
  (cons has-rest (map record-type-uid signature)))

(define-syntax method-alist-cons
  (syntax-rules ()
    ((_ ?key ?closure ?method-alist)
     (cons (cons ?key ?closure) ?method-alist))))

(define method-entry-key		car)
(define method-entry-closure		cdr)

(define method-entry-accept-rest?	caar)
(define method-entry-signature		cdar)

(define method-entry-closure-set!	set-cdr!)


;;;; actually adding methods

(define (%add-method-to-method-alist method-alist signature has-rest closure)
  ;;Add a  method's entry to the  alist of methods;  return the modified
  ;;method alist.
  ;;
  ;;A new method entry is added  only if no method with the selected key
  ;;already  exists.  If  a  method  with the  key  already exists,  its
  ;;closure is overwritten with the new one.
  ;;
  (let ((key (make-method-entry-key has-rest signature)))
    (cond ((find (lambda (method-entry)
		   (for-all* eq? key (method-entry-key method-entry)))
		 method-alist)
	   => (lambda (method-entry)
		(method-entry-closure-set! method-entry closure)
		method-alist))
	  (else
	   (method-alist-cons key closure method-alist)))))


;;;; methods dispatching

(define (%compute-applicable-methods call-signature method-alist)
  ;;Filter out from  METHOD-ALIST the methods not applicable  to a tuple
  ;;of arguments with types in  the tuple CALL-SIGNATURE.  Then sort the
  ;;list of applicable  methods so that the more  specific are the first
  ;;ones.  Return the sorted list of applicable closures.
  ;;
  (map method-entry-closure
    (list-sort
     (lambda (method1 method2)
       (%more-specific-method? method1 method2 call-signature))
     (filter
	 (lambda (method)
	   (%applicable-method? call-signature
				(method-entry-signature    method)
				(method-entry-accept-rest? method)))
       method-alist))))

(define (%applicable-method? call-signature signature has-rest)
  ;;Return true if a method with SIGNATURE as tuple of arguments' record
  ;;types can be  applied to a tuple of  arguments having CALL-SIGNATURE
  ;;as record types.  HAS-REST must  be true if the method supports rest
  ;;arguments.
  (let ((len      (length signature))
	(call-len (length call-signature)))
    (cond
     ;;If SIGNATURE has  the same length of the  call signature, test it
     ;;for applicability.
     ((= call-len len)
      (for-all* record-type-parent? call-signature signature))

     ;;If  the closure  supports rest  arguments, compare  only  as much
     ;;record types as there are in SIGNATURE.
     ((and has-rest (> call-len len))
      (for-all* record-type-parent? (take-left call-signature len) signature))

     ;;This method is not applicable.
     (else #f))))

(define (%more-specific-method? method1 method2 call-signature)
  ;;Return true if METHOD1 is more specific than METHOD2 with respect to
  ;;CALL-SIGNATURE.   This  function   must  be  applied  to  applicable
  ;;methods.  The longest signature is more specific, by definition.
  ;;
  (let* ((signature1	(cdar method1))
	 (signature2	(cdar method2))
	 (len1		(length signature1))
	 (len2		(length signature2)))
    (cond ((> len1 len2) #t)
	  ((< len1 len2) #f)
	  (else ;(= len1 len2)
	   (let loop ((signature1     signature1)
		      (signature2     signature2)
		      (call-signature call-signature))
	     (if (null? signature1)

		 ;;If we  are here: The two signatures  have EQ?  values
		 ;;(and  equal  length).    We  want  this:  If  METHOD2
		 ;;supports  rest arguments and  METHOD1 does  not, then
		 ;;METHOD1  is  more  specific.   This test  reduces  to
		 ;;testing if METHOD2 supports rest arguments.
		 (method-entry-accept-rest? method2)

	       (let ((rtd1 (car signature1))
		     (rtd2 (car signature2)))
		 (cond
		  ((eq? rtd1 rtd2)
		   (loop (cdr signature1) (cdr signature2) (cdr call-signature)))
		  ((record-type-parent? rtd1 rtd2) #t)
		  (else #f)))))))))


;;;; generic functions merging

(define (merge-generic-functions gf . generics)
  (let ((ma  (merge-method-alists (gf :method-alist)
				  (map (lambda (gf)
					 (gf :method-alist))
				    generics)))
	(new (make-generic-function)))
    (new :method-alist-set! ma)
    new))

(define-syntax list-copy
  (syntax-rules ()
    ((_ ?ell)
     (let loop ((ell ?ell))
       (if (pair? ell)
	   (cons (car ell) (loop (cdr ell)))
	 ell)))))

(define-syntax merge-method-alists
  (syntax-rules ()
    ((_ ?ma ?method-alists)
     (let loop ((ma		(list-copy ?ma))
		(method-alists	?method-alists))
       (if (null? method-alists)
	   ma
	 (loop (merge-two-method-alists ma (car method-alists))
	       (cdr method-alists)))))))

(define-syntax merge-two-method-alists
  ;;Merge ?MA1 into ?MA and return a new alist.
  ;;
  (syntax-rules ()
    ((_ ?ma ?ma1)
     (let loop ((ma  ?ma)
		(ma1 ?ma1))
       (if (null? ma1)
	   ma
	 (loop (maybe-merge-method (car ma1) ma) (cdr ma1)))))))

(define-syntax maybe-merge-method
  ;;Add CANDIDATE-METHOD-ENTRY to METHOD-ALIST and return the new alist.
  ;;Adding happens  only if  a method with  the same signature  and rest
  ;;arguments support does not already exist in METHOD-ALIST.
  ;;
  (syntax-rules ()
    ((_ ?candidate-method-entry ?method-alist)
     (let* ((candidate-method-entry	?candidate-method-entry)
	    (method-alist		?method-alist)
	    (key			(method-entry-key candidate-method-entry)))
       (unless (find (lambda (method-entry)
		       (for-all* eq? key (method-entry-key method-entry)))
		     method-alist)
	 (cons candidate-method-entry method-alist))))))


(define (signature-hash signature)
  (fold-left (lambda (nil rtd)
	       (+ nil (symbol-hash (record-type-name rtd))))
	     0
	     signature))
(define xx '#1=(#1#))


;;;; predefined generic functions

(define-generic object->string)

(declare-method (object->string o)
  (call-with-string-output-port
   (lambda (port)
     (display o port))))


;;;; done

)

;;; end of file
