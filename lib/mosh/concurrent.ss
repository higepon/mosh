; concurrent.ss
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
;  $Id: concurrent.ss 621 2008-11-09 06:22:47Z higepon $

#|
    Title: Concurrent

    Multi-Process library

    library: (mosh concurrent)

    Multi-Process Library
|#
(library (mosh concurrent)
  (export ! receive spawn self join! register whereis link unlink process-exit spawn-link
          make-process-error process-error? process-error process-arg)
  (import (only (mosh) main-vm? vm-set-value! vm-self make-condition-variable make-mutex mutex-lock! mutex-unlock! condition-variable-notify!
                whereis vm-start! make-vm symbol-value condition-variable-wait! vm-join! register time)
          (only (rnrs) begin define-record-type immutable mutable protocol lambda define for-each quote exit fields _ ... define-syntax
                syntax-case syntax integer? syntax->datum when let quasiquote unless error if let* memq remq cons cond pair? not car cadr
                else letrec unquote display define-condition-type &error quasisyntax unsyntax unquote)
          (only (mosh queue) make-queue queue-push! queue-empty? queue-pop! queue-append!)
          (only (rnrs mutable-pairs) set-car!)
          (only (match) match))

(define-condition-type &process &error
  make-process-error process-error?
  (error process-error))

(define-record-type mail-box
  (fields
   (immutable condition)
   (immutable mutex)
   (mutable mails))
  (protocol
   (lambda (c)
     (lambda ()
       (c (make-condition-variable) (make-mutex) (make-queue))))))

(define-record-type pid
  (fields
   (immutable vm)
   (immutable mail-box)
   (mutable links))
  (protocol
   (lambda (c)
     (lambda (vm)
       (c vm (make-mail-box) '())))))

#|
    Function: spawn

    Create a process which executes proc. On Mosh, a process means an independent VM instance.
    The process which is spawn-ed and calls spawn share nothing.
    Each processes has own independent namespace.

    Prototype:
    > (spawn proc-expr arg import-spec)

    Parameters:

      proc-expr - (lambda (arg) ...) expression to execute.
      arg - argument for proc-expr.
      import-spec - import spec.

    Example:
    (start code)
    (spawn (lambda (x) (display x)) 'hello '((rnrs)))
    (end code)

    Returns:

      pid
|#
(define-syntax spawn
  (lambda (x)
    (syntax-case x ()
      [(_ expr args import-spec)
       #'(spawn-internal 'expr args import-spec)])))

#|
    Function: spawn-link

    spawn and link.

    Prototype:
    > (spawn-link proc-expr arg import-spec)

    Parameters:

      proc-expr - (lambda (arg) ...) expression to execute.
      arg - argument for proc-expr.
      import-spec - import spec.

    Returns:

      pid
|#
(define-syntax spawn-link
  (lambda (x)
    (syntax-case x ()
      [(_ expr args import-spec)
       #'(let ([pid (spawn-internal 'expr args import-spec)])
           (link pid)
           pid)])))

#|
    Function: process-exit

    exit process with status.
    status is sent to linked process as '(exit status) message.

    Prototype:
    > (process-exit status)
|#
(define (process-exit status)
  (for-each
   (lambda (to)
     (! to `(exit ,status)))
   (pid-links (self)))
  (exit status))

#|
    Function: !

    Send a message object to pid process. ! procedure will not be blocked, returns immediately.

    Prototype:
    > (! pid obj)
|#
(define (! pid obj)
  (let ([p (if (pid? pid)
               pid
               (whereis pid))])
    (unless p
      (error '! "pid not found" pid))
    (let ([mb (pid-mail-box p)])
      (mutex-lock! (mail-box-mutex mb))
      (queue-push! (mail-box-mails mb) obj)
      (condition-variable-notify! (mail-box-condition mb))
      (mutex-unlock! (mail-box-mutex mb)))))

#|
    Function: link

    Link self and other process.
    Linked processes send '(exit why) message when exit to each other.

    Prototype:
    > (link pid)

    Parameters:

      pid - pid of process
|#
(define (link pid)
  (let* ([self (self)]
         [links (pid-links pid)])
    (unless (memq pid links)
      (pid-links-set! pid (cons pid links)))
    (let ([links (pid-links self)])
      (unless (memq self links)
        (pid-links-set! pid (cons self links))))))

#|
    Function: unlink

    unlink linked process

    Prototype:
    > (unlink pid)

    Parameters:

      pid - pid of process
|#
(define (unlink pid)
  (let* ([self (self)]
         [links (pid-links pid)])
    (when (memq pid links)
      (pid-links-set! pid (remq pid links)))
    (let ([links (pid-links self)])
      (when (memq self links)
        (pid-links-set! pid (remq self links))))))


(define (spawn-internal thunk args import-spec)
  (let* ([vm (make-vm `(lambda () (guard (c [#t (process-exit (make-process-error c))])
                                         (,thunk (process-arg)) (process-exit 'normal))) import-spec)]
         [pid (make-pid vm)])
    (vm-set-value! vm 'self pid)
    (vm-set-value! vm 'process-arg args)
    (vm-start! vm)
    pid))

#|
    Function: self

    Returns self pid.

    Prototype:
    > (self)

    Returns:

      pid
|#
(define (self)
  (symbol-value 'self))

(define (process-arg)
  (symbol-value 'process-arg))

#|
    Function: receive

    Receive a message which matches <match clause>. If there is no message or no matched message, receive is blocked.
    <match clause> may have [after timeout ...], receive wait timeout msec and returns

    Prototype:
    > (receive <match clause> ...)

    Example:
    (start code)
    ;; wait and receive ('exit why) message.
    (receive
      [('exit why)
        (display why)])

    ;; wait two pattern messages.
    (receive
      [('exit why)
        (display why)]
      [x
        (display "unknown message")
        (display x)])

    ;; wait with timeout
    (receive
      [('exit why)
        (display why)]
      [after 1000
        (display "exit why doesn't come in 1000 msec")])
    (end code)

|#
(define-syntax receive
  (lambda (x)
  (syntax-case x (after)
    [(_ [match-expr body0 body ...] ... [after timeout after-body0 after-body ...])
     (integer? (syntax->datum #'timeout))
     #'(let ([saved (make-queue)])
         (letrec ([rec (lambda ()
                         (match (receive-internal! timeout)
                           ['%timeout
                            ;; restore!
                              (mutex-lock! (mail-box-mutex (pid-mail-box (self))))
                            (let ([mails (mail-box-mails (pid-mail-box (self)))])
                              (queue-append! saved mails)
                              (mail-box-mails-set! (pid-mail-box (self)) saved)
                              (mutex-unlock! (mail-box-mutex (pid-mail-box (self)))))
                            after-body0 after-body ...]
                           [match-expr
                            (mutex-lock! (mail-box-mutex (pid-mail-box (self))))
                            (let ([mails (mail-box-mails (pid-mail-box (self)))])
                              (queue-append! saved mails)
                              (mail-box-mails-set! (pid-mail-box (self)) saved)
                              (mutex-unlock! (mail-box-mutex (pid-mail-box (self))))
                              body0 body ...)] ...
                           [x
                            (queue-push! saved x)
                            (rec)]))])
           (rec)))]
    [(_ [match-expr body0 body ...] ...)
     #'(let ([saved (make-queue)])
         (letrec ([rec (lambda ()
                         (match (receive-internal!)
                           [match-expr
                            (mutex-lock! (mail-box-mutex (pid-mail-box (self))))
                            (let ([mails (mail-box-mails (pid-mail-box (self)))])
                              (queue-append! saved mails)
                              (mail-box-mails-set! (pid-mail-box (self)) saved)
                              (mutex-unlock! (mail-box-mutex (pid-mail-box (self))))
                              body0 body ...)] ...
                           [x
                            (queue-push! saved x)
                            (rec)]))])
           (rec)))])))

(define (receive-internal! . timeout)
  (let ([mb (pid-mail-box (self))])
  (let loop ()
    (cond
     [(queue-empty? (mail-box-mails mb))
      (cond
       [(pair? timeout)
        (mutex-lock! (mail-box-mutex mb))
        (let ([timeout? (not (condition-variable-wait! (mail-box-condition mb) (mail-box-mutex mb) (car timeout)))])
          (if timeout?
              (begin (mutex-unlock! (mail-box-mutex mb)) '%timeout)
              (loop)))]
       [else
        (mutex-lock! (mail-box-mutex mb))
        (condition-variable-wait! (mail-box-condition mb) (mail-box-mutex mb))
        (mutex-unlock! (mail-box-mutex mb))
        (loop)])]
     [else
      (mutex-lock! (mail-box-mutex mb))
      (let ([val (queue-pop! (mail-box-mails mb))])
        (mutex-unlock! (mail-box-mutex mb))
        val)]))))

#|
    Function: join!

    Waits termination of process.

    Prototype:
    > (join! pid)

    Parameters:

      pid - pid of process.

    Example:
    (start code)
    (let ([pid (spawn (lambda (x) (display x)) 'hello '((rnrs)))])
       (join! pid))
    (end code)
|#
(define (join! pid)
  (vm-join! (pid-vm pid)))

#|
    Function: register

    Register a process by name.
    Registered process can be fetch with whereis procedure by name.

    Prototype:
    > (register name pid)

    Parameters:

      name - name to register
      pid - pid of process
|#

#|
    Function: whereis

    Look up process by name

    Prototype:
    > (whereis name)

    Parameters:

      name - name to look up

    Returns:
      pid of found process or #f
|#

#|
    Function: make-process-error

    Make a process-error

    Prototype:
    > (make-process-error state)

    Parameters:

      state - error state

    Returns:
      process-error condition
|#

#|
    Function: process-error?

    Returns #t if obj is process-error condition, otherwise #f.

    Prototype:
    > (process-error? obj)

    Returns:
      #t if obj is process-error condition, otherwise #f.
|#

#|
    Function: process-error

    Returns error state of process-error condition.

    Prototype:
    > (process-error process-error-condition)

    Returns:
      Returns error state of process-error condition.
|#

(when (main-vm?)
  (let ([pid (make-pid (vm-self))])
    (vm-set-value! (vm-self) 'self pid)))

)
