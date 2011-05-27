; monapi.ss - MonAPI wrapper
;
;   Copyright (c) 2010  Higepon(Taro Minowa)  <higepon@users.sourceforge.jp>
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

#|
    Title: MonAPI


    Example:
    (start code)
    (end code)

    library: (monapi)

    MonAPI Library
|#
(library (monapi)
  (export (rename (%monapi-message-send monapi-message-send)
                  (%monapi-message-receive monapi-message-receive)
                  (%monapi-name-whereis monapi-name-whereis)
                  (%monapi-name-add! monapi-name-add!))
          MSG_OK
          MSG_STARTED
          MSG_INTERRUPTED
          MSG_TIMER
          MSG_READ_READY
          MSG_WRITE_READY
          MSG_TEXT)
  (import (rnrs) (mosh) (system))

#|
    Function: monapi-message-send

    Wrapper of Message::send API.

    Prototype:
    > (monapi-message-send dest header arg1 arg2 arg3 bv)

    Parameters:

      dest - destination thread id as exact integer.
      header - message header as exact integer.
      arg1 - message arg1 as exact integer.
      arg2 - message arg2 as exact integer.
      arg3 - message arg3 as exact integer.
      bv - message str as bytevector, should be less than equal 128 byte.

|#

#|
    Function: monapi-name-whereis

    Wrapper of monapi_name_whereis API.

    Prototype:
    > (monapi-name-whereis name)

    Parameters:

      name - name of server

|#


#|
    Constant: MSG_OK

    MSG_OK
|#
(define MSG_OK (os-constant 'MSG_OK))

#|
    Constant: MSG_STARTED

    MSG_STARTED
|#
(define MSG_STARTED (os-constant 'MSG_STARTED))

#|
    Constant: MSG_INTERRUPTED

    MSG_INTERRUPTED
|#
(define MSG_INTERRUPTED (os-constant 'MSG_INTERRUPTED))

#|
    Constant: MSG_TIMER

    MSG_TIMER
|#
(define MSG_TIMER (os-constant 'MSG_TIMER))

#|
    Constant: MSG_READ_READY

    MSG_READ_READY
|#
(define MSG_READ_READY (os-constant 'MSG_READ_READY))

#|
    Constant: MSG_WRITE_READY

    MSG_WRITE_READY
|#
(define MSG_WRITE_READY (os-constant 'MSG_WRITE_READY))

#|
    Constant: MSG_TEXT

    MSG_TEXT
|#
(define MSG_TEXT (os-constant 'MSG_TEXT))
)
