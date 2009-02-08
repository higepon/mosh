;; Copyright (c) 2009 Derick Eddington
;;
;; Permission is hereby granted, free of charge, to any person obtaining a
;; copy of this software and associated documentation files (the "Software"),
;; to deal in the Software without restriction, including without limitation
;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;; and/or sell copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; Except as contained in this notice, the name(s) of the above copyright
;; holders shall not be used in advertising or otherwise to promote the sale,
;; use or other dealings in this Software without prior written authorization.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;; DEALINGS IN THE SOFTWARE.

#!r6rs
(library (srfi :19 time)
  (export
    time make-time time? time-type time-nanosecond time-second
    date make-date date? date-nanosecond date-second date-minute 
    date-hour date-day date-month date-year date-zone-offset
    time-tai time-utc time-monotonic
    #|time-thread time-process|# time-duration
    read-leap-second-table copy-time current-time
    time-resolution time=? time>? time<? time>=? time<=?
    time-difference time-difference! add-duration
    add-duration! subtract-duration subtract-duration!
    time-tai->time-utc time-tai->time-utc! time-utc->time-tai
    time-utc->time-tai! time-monotonic->time-utc
    time-monotonic->time-utc! time-monotonic->time-tai
    time-monotonic->time-tai! time-utc->time-monotonic
    time-utc->time-monotonic! time-tai->time-monotonic
    time-tai->time-monotonic! time-tai->date time-utc->date
    time-monotonic->date date->time-utc date->time-tai
    date->time-monotonic leap-year? date-year-day
    date-week-day date-week-number current-date
    date->julian-day date->modified-julian-day
    time-utc->julian-day time-utc->modified-julian-day
    time-tai->julian-day time-tai->modified-julian-day
    time-monotonic->julian-day
    time-monotonic->modified-julian-day julian-day->time-utc
    julian-day->time-tai julian-day->time-monotonic
    julian-day->date modified-julian-day->date
    modified-julian-day->time-utc
    modified-julian-day->time-tai
    modified-julian-day->time-monotonic current-julian-day
    current-modified-julian-day date->string string->date)
  (import
   (only (rnrs) define begin define-syntax syntax-rules define-record-type fields mutable case-lambda get-line current-input-port eof-object quote vector expt / cond assoc => lambda if error cdr else let not eq? let* >= set! cons cadddr cdddr car read string-append open-input-file * - values letrec < caar cdar null? <= ... _ call-with-values + or and = > abs negative? char=? string-ref integer? substring string-length number->string truncate floor do make-string vector-ref vector-length string=? display list newline integer->char eof-object? char-numeric? read-char peek-char char-alphabetic? write-char cadr caddr)
   (only (rnrs r5rs) inexact->exact remainder quotient exact->inexact modulo)
   (only (rnrs mutable-strings) string-set!)
   (only (srfi :19 time compat) host:time-second host:time-nanosecond host:current-time host:time-resolution host:time-gmt-offset)
   (only (srfi :6 basic-string-ports) open-input-string get-output-string open-output-string)
   (only (srfi private include) include/resolve)
)
  
  (define read-line
    (case-lambda
      [()
       (get-line (current-input-port))]
      [(port)
       (get-line port)]))
  
  (define eof (eof-object))
  
  (include/resolve ("srfi" "19") "srfi-19.scm")
)

