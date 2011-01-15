(library (yuni scheme r6rs io ports)
	 (export
	   binary-port?  buffer-mode buffer-mode?  bytevector->string call-with-bytevector-output-port call-with-port
	   call-with-string-output-port close-port eol-style error-handling-mode file-options flush-output-port
	   get-bytevector-all get-bytevector-n get-bytevector-n! get-char get-datum get-line get-string-all get-string-n
	   get-string-n!  get-u8 &i/o &i/o-decoding i/o-decoding-error?  &i/o-encoding i/o-encoding-error-char
	   i/o-encoding-error?  i/o-error-filename i/o-error-port i/o-error-position i/o-error?  &i/o-file-already-exists
	   i/o-file-already-exists-error?  &i/o-file-does-not-exist i/o-file-does-not-exist-error?  &i/o-file-is-read-only
	i/o-file-is-read-only-error?  &i/o-file-protection i/o-file-protection-error?  &i/o-filename
	i/o-filename-error?  &i/o-invalid-position i/o-invalid-position-error?  &i/o-port
	i/o-port-error?  &i/o-read i/o-read-error?  &i/o-write i/o-write-error?  lookahead-char
	lookahead-u8 make-custom-binary-input-port make-custom-binary-input/output-port make-custom-binary-output-port
	make-custom-textual-input-port make-custom-textual-input/output-port make-custom-textual-output-port
	make-i/o-decoding-error make-i/o-encoding-error make-i/o-error make-i/o-file-already-exists-error
	make-i/o-file-does-not-exist-error make-i/o-file-is-read-only-error make-i/o-file-protection-error
	make-i/o-filename-error make-i/o-invalid-position-error make-i/o-port-error make-i/o-read-error
	make-i/o-write-error latin-1-codec make-transcoder native-eol-style native-transcoder open-bytevector-input-port
	open-bytevector-output-port open-file-input-port open-file-input/output-port open-file-output-port
	open-string-input-port open-string-output-port output-port-buffer-mode port-eof?  port-has-port-position?
	port-has-set-port-position!?  port-position port-transcoder port?  put-bytevector put-char put-datum put-string
	put-u8 set-port-position!  standard-error-port standard-input-port standard-output-port string->bytevector
	textual-port?  transcoded-port transcoder-codec transcoder-eol-style transcoder-error-handling-mode utf-16-codec
	utf-8-codec input-port?  output-port?  current-input-port current-output-port current-error-port eof-object eof-object?
	get-bytevector-some
	
	   )
	 (import (yuni scheme r6rs))
)

