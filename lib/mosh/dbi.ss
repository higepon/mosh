; dbi.ss - DBI(Database Interface)
;
;   Copyright (c) 2009  Higepon(Taro Minowa)  <higepon@users.sourceforge.jp>
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
;  $Id: dbi.ss 621 2008-11-09 06:22:47Z higepon $

#|
    Title:  Database independent interface

    Defines a set of functions to access Database.

    Example:
    (start code)
    (import (rnrs) (mosh dbi))
    (let* ([conn   (dbi-connect "dbi:mysql:mysql:127.0.0.1:3306" "root" "")]
           [query  (dbi-prepare conn "select * from user where user = ?")]
           [result (dbi-execute query "root")]
           [getter (dbi-getter result)])
      (for-each
       (lambda (row)
         (display  (getter row "host")))
       (dbi-result->list result))
      (dbi-close conn))
    (end code)

    library: (mosh dbi)

    Database independent interface library
|#
(library (mosh dbi)
  (export dbi-connect dbi-prepare dbi-execute dbi-getter dbi-result->list dbi-close
          dbi-do
          <connection> <query> <result>
          dbd-connect dbd-execute <dbd>)
  (import
     (only (rnrs) define quote cond => lambda assertion-violation values
                  let-values else and quasiquote unquote string->symbol let
                  let* null? apply string-append reverse if string=? car
                  cdr cons string? number? char? when display string->list
                  map)
     (only (mosh) symbol-value format)
     (only (clos user) define-class define-generic define-method initialize initialize-direct-slots
                       make slot-ref))

#|
    About: How to write new dbd.

    mosh/dbd/mysql.ss and test/dbi.scm are good sample.

    dbd SHOULD implement following functions
    - (dbd-connect <dbd-xxx> user password options) returns sub-class of <connection>
    - (dbd-execute <xxx-connection> sql) returns sub-class of <result>
    - (dbi-getter <xxxx-result>) returns getter closure. (getter row name)
    - (dbi-result->list <xxxx-result>) returns list of rows
    - (dbi-close <xxx-connection> returns unspecified.
|#

#|
    Variable: <connection>

    Connection class.
|#
(define-class <connection> ())

#|
    Variable: <dbd>

    Database driver class.
|#
(define-class <dbd> ())

#|
    Variable: <result>

    Result of query class.
|#
(define-class <result> ())

#|
    Variable: <query>

    Query class.
|#
(define-class <query> () prepared connection)

(define-generic dbd-connect)
(define-generic dbd-execute)
(define-generic dbi-prepare)

#|
    Function: dbi-result->list

    Returns list of result rows.

    Prototype:
    > (dbi-result->list result)

    Parameters:

      result - an instance of <result> or its subclass returned by <dbi-execute>

    Returns:

      list of result rows.
|#
(define-generic dbi-result->list)

#|
    Function: dbi-getter

    Returns getter for <result> which is returned by <dbi-execute>.

    Prototype:
    > (dbi-getter result)

    Parameters:

      result - an instance of <result> or its subclass returned by <dbi-execute>

    Returns:

      getter closure. getter closure accept two arguments, row and column-name.

      row - row in results.
      column-name - name of column to retrieve.
|#
(define-generic dbi-getter)

#|
    Function: dbi-close

    Destroy the connection and free resources associated to the connection.

    Prototype:
    > (dbi-close conn)

    Parameters:

      conn - an instance of <connection> or its subclass returned by <dbi-connect>

    Returns:

      unspecified
|#
(define-generic dbi-close)

(define-generic dbi-do)

(define (make-driver name)
  (let ([eval-r6rs (symbol-value 'eval-r6rs)])
    (eval-r6rs `(import (clos core)))
    (eval-r6rs `(import (dbd ,(string->symbol name))))
    (eval-r6rs `(make ,(string->symbol (format "<dbd-~a>" name))))))

#|
    Function: dbi-connect

    Connect to a database using a data source specified by dsn (data source name).

    Dsn is a string with the following syntax

    dbi:driver:options

    Driver part names a specific driver. You need to have the corresponding driver library, (mosh dbd driver), installed in your system.

    For example, if dsn begins with "dbi:mysql:", dbi-connect tries to load (mosh dbd mysql).

    Prototype:
    > (dbi-connect dsn user password)

    Parameters:

      dsn - data source name.
      user - user name.
      password - user password.

    Returns:

      Instance of <connection> class.
|#
(define (dbi-connect dsn user password)
  (define (parse-dsn dsn)
    (cond
     [(#/dbi:([^:]+):(.+)/ dsn) =>
      (lambda (m)
        (values (m 1) (m 2)))]
     [else
      (values #f #f)]))
  (let-values ([(name options) (parse-dsn dsn)])
    (cond
     [(and name options)
      (let ([driver (make-driver name)])
        (dbd-connect driver user password options))]
     [else
      (assertion-violation 'dbi-connect "invalid dsn. dbi:drivername:options required" dsn)])))

(define (prepare-helper obj)
  (cond
   [(string? obj)
    (format "~s" obj)]
   [(number? obj)
    (format "~a" obj)]
   [(char? obj)
    (format "~a" obj)]
   [else
    (assertion-violation 'dbi-execute "not supported argument for prepared sql" obj)]))

(define (dbi-set-prepared prepared args)
  (let* ([tokens (map (lambda (x) (format "~a" x)) (string->list prepared))])
    (let loop ([tokens tokens]
               [ret '()]
               [args args])
      (cond
       [(null? tokens)
        (apply string-append (reverse ret))]
       [else
        (cond
         [(string=? "?" (car tokens))
          (when (null? args)
            (assertion-violation 'dbi-execute "args to short for prepared sql" prepared args))
          (loop (cdr tokens) (cons (prepare-helper (car args)) ret) (cdr args))]
         [else
          (loop (cdr tokens) (cons (car tokens) ret) args)])]))))

#|
    Function: dbi-execute

    Executes a query created by dbi-prepare. You should pass the same number of parameters as the query expects.

    If the issued query is select statement, dbi-execute returns an object represents a relation. A relation encapsulates the values in rows and columns.
    If the query is other types, such as create, insert or delete, the return value of the query closure is unspecified.

    Prototype:
    > (dbi-execute query . args)

    Parameters:

      query - an instance of <query> or its subclass returned by <dbi-prepare>
      args - arguments for the query

    Returns:

      an instance of <result> or its subclass.
|#
(define (dbi-execute query . args)
  (dbd-execute (slot-ref query 'connection) (dbi-set-prepared (slot-ref query 'prepared) args)))

(define-method initialize ((q <query>) init-args)
  (initialize-direct-slots q <query> init-args))

#|
    Function: dbi-prepare

    From a string representation of SQL statement sql, creates and returns a query object (an instance of <query> or its subclass) for the database connection conn
    Sql may contain parameter slots, denoted by ?.

    dbi-prepare may be overloaded by dbd implmentation.

    Prototype:
    > (dbi-prepare conn sql)

    Parameters:

      conn - an instance of <connecton> or its subclass returned by <dbi-connect>
      sql - sql string

    Returns:

      an instance of <query> or its subclass.
|#
(define-method dbi-prepare ((conn <connection>) sql)
  (make <query> 'prepared sql 'connection conn))

#|
    Function: dbi-do

    This is a convenience procedure when you create a query and immediately execute it.

    Prototype:
    > (dbi-do conn sql)

    Parameters:

      conn - an instance of <connection> or its subclass returned by <dbi-connect>
      sql - sql string

    Returns:

      unspecified
|#
(define-method dbi-do ((conn <connection>) sql)
  (dbi-execute (dbi-prepare conn sql)))

)
