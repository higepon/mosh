(library
  (mosh)
  (export
    include
    os-constant
    time-usage
    time
    fasl-write
    fasl-read
    fast-equal?
    bignum?
    set-source-info!
    make-instruction
    make-compiler-instruction
    source-info
    make-file-options
    mosh-executable-path
    make-condition-variable
    condition-variable-wait!
    condition-variable-notify!
    condition-variable-notify-all!
    make-mutex
    mutex?
    mutex-lock!
    mutex-unlock!
    mutex-try-lock!
    make-vm
    vm-start!
    vm-self
    vm-eval
    main-vm?
    vm?
    vm-set-value!
    vm-join!
    register
    whereis
    sys-display
    get-command-line
    get-timeofday
    p
    current-directory
    expand-path
    set-current-directory!
    hashtable-for-each
    regexp-replace-all
    rxmatch
    string->regexp
    bytevector-for-each
    string-split
    call-with-string-io
    call-with-string-input-port
    digit->integer
    stat-mtime
    file-newer?
    standard-library-path
    library-path
    host-os
    format
    print
    assoc-ref
    alist->eq-hash-table
    ungensym
    read-line
    regexp?
    write/ss
    symbol-value
    set-symbol-value!)
  (import
    (for (primitives
      include
      os-constant
      time-usage
      ;time is macro
      fasl-write
      fasl-read
      fast-equal?
      bignum?
      set-source-info!
      make-instruction
      make-compiler-instruction
      source-info
      make-file-options
      mosh-executable-path
      make-condition-variable
      condition-variable-wait!
      condition-variable-notify!
      condition-variable-notify-all!
      make-mutex
      mutex?
      mutex-lock!
      mutex-unlock!
      mutex-try-lock!
      make-vm
      vm-start!
      vm-self
      vm-eval
      main-vm?
      vm?
      vm-set-value!
      vm-join!
      register
      whereis
      sys-display
      get-command-line
      get-timeofday
      p
      current-directory
      expand-path
      set-current-directory!
      hashtable-for-each
      regexp-replace-all
      rxmatch
      string->regexp
      bytevector-for-each
      string-split
      call-with-string-io
      call-with-string-input-port
      digit->integer
      stat-mtime
      file-newer?
      standard-library-path
      host-os
      format
      print
      assoc-ref
      alist->eq-hash-table
      ungensym
      read-line
      regexp?
      symbol-value
      write/ss
      set-symbol-value!) run expand)
    (for (only (rnrs base) map car cadr caddr - lambda let* syntax-rules define-syntax list string-append define) expand run)
    )
 (define (library-path) (list (string-append (standard-library-path) "/lib")))
 (define-syntax time
   (syntax-rules () ;from psyntax/expander
     ((_ expr)
      (let* ([start (time-usage)]
	     [result ((lambda () expr))]
	     [end (time-usage)]
	     [used (map - end start)]
	     [real (car used)]
	     [user (cadr used)]
	     [sys (caddr used)])
	(format #t "~%;;~a real ~a user ~a sys~%~!" real user sys)
	result))))
  
  )
