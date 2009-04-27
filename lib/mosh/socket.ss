; socket.ss - Socket interface
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
;  $Id: socket.ss 621 2008-11-09 06:22:47Z higepon $

#|
    Title: Socket

    Mosh provides a simple Socket library.

    This library is almost compatible with Ypsilon's (socket) library. Thanks to Yoshikatsu Fujita.

    Example:
    (start code)
    ;; Sample of IRC client bot
    (import (rnrs)
            (mosh)
            (mosh socket))

    (define (irc-bot server port nick channel)
      (let ([socket (make-client-socket server port)])
        (define (send text)
          (assert (<= (string-length text) 510))
          (socket-send socket (string->utf8 (string-append text "\r\n"))))
        (define (recv)
          (utf8->string (socket-recv socket 512)))
        (define (say text)
          (send (format "PRIVMSG ~a :~a" channel text)))
        (send (format "NICK ~a" nick))
        (send (format "USER ~a 0 * :~a" nick nick))
        (send (format "JOIN ~a" channel))
        (let loop ([data (recv)])
          (cond
           [(#/:([^!]+).*PRIVMSG[^:]+:(.*)/ data) =>
            (lambda (m)
              (format #t "<~a> ~a\n" (m 1) (m 2))
              (say (format "what is ~s?" (m 2))))]
           [(#/^PING/ data)
            (send "PONG 0")]
           [(#/:.*433.*Nickname is already in use.*/ data)
            (error 'irc "Nickname is already in use")])
          (loop (recv)))
        (socket-close socket)))

    (irc-bot "irc.freenode.net" "6666" "higepon" "#mosh")
    (end code)

    library: (mosh socket)

    Socket Library
|#
(library (mosh socket)
  (export
    socket?
    socket-accept
    make-client-socket
    make-server-socket
    socket-recv
    socket-recv!
    socket-send
    socket-close
    socket-shutdown
    socket-port
    call-with-socket
    AF_INET
    AF_INET6
    AF_UNSPEC
    SOCK_STREAM
    SOCK_DGRAM
;    SOCK_RAW
    AI_ADDRCONFIG
    AI_ALL
    AI_CANONNAME
    AI_NUMERICHOST
    AI_NUMERICSERV
    AI_PASSIVE
    AI_V4MAPPED
    IPPROTO_TCP
    IPPROTO_UDP
    IPPROTO_RAW
    SHUT_RD
    SHUT_WR
    SHUT_RDWR)
  (import (only (rnrs) let symbol->string string-append ... _ define define-syntax syntax-case cond lambda if syntax->datum syntax => quasiquote quasisyntax
                else unsyntax call-with-values apply values)
          (only (mosh control) let-optionals*)
          (only (mosh) os-constant)
          (rename (system)
                  (socket-recv %socket-recv)
                  (socket-recv! %socket-recv!)
                  (socket-send %socket-send)
                  (make-client-socket %make-client-socket)
                  (make-server-socket %make-server-socket)))

  #|
      Function: socket?

      Returns #t when obj is socket.

      Prototype:
      > (socket? obj)

      Returns:

        #t when obj is socket.
  |#

  ;; borrowed from Ypsilon Scheme by Yoshikatsu Fujita.
  (define-syntax define-const
    (lambda (x)
      (syntax-case x ()
        ((_ name)
         (cond [(os-constant (syntax->datum #'name))
                => (lambda (value) #`(define name #,value))]
               [else
                (let ([x (string-append (symbol->string (syntax->datum #'name)) " is not supported on your operating system" )])
                  #`(define name #,x))])))))

  #|
      Function: make-client-socket

      Returns a client socket connected to an Internet address.

      The Internet address is identified by node and service. make-client-socket uses getaddrinfo(3) to look up it.

      The arguments node, service, ai-family, ai-socktype, ai-flags, and ai-protocol will be passed to getaddrinfo(3) as a correspondent parameter.Refer to getaddrinfo(3) manual page for details

      Prototype:
      > (make-client-socket node service . options)

      Parameters:

        node - a network address. (examples: "www.w3.org", "localhost", "128.30.52.45")
        service - a network service. (examples: "http", "ssh", "80", "22")
        ai-family(optional) - an address family specifier. <AF_INET> (default), <AF_INET6> or <AF_UNSPEC>.
        ai-socktype(optional) - a socket type specifier. <SOCK_STREAM> (default) or <SOCK_DGRAM>.
        ai-flags - an additional options specifier. <AI_ADDRCONFIG>, <AI_ALL>, <AI_CANONNAME>, <AI_NUMERICHOST>, <AI_NUMERICSERV>, <AI_PASSIVE>, <AI_V4MAPPED> or 0(default).
        ai-protocol - a protocol specifier. <IPPROTO_TCP>, <IPPROTO_UDP>, <IPPROTO_RAW> or 0(default).

      Returns:

        client socket.
  |#
  (define (make-client-socket node service . options)
    (let-optionals* options ([ai-family AF_INET]
                             [ai-socktype SOCK_STREAM]
                             [ai-flags 0]
                             [ai-protocol 0])
      (%make-client-socket node service ai-family ai-socktype ai-flags ai-protocol)))

  #|
      Function: make-server-socket

      Returns a server socket waiting for connections.

      The arguments service, ai-family, ai-socktype, and ai-protocol will be passed to getaddrinfo(3) as a correspondent parameter to setup server socket.

      Refer to getaddrinfo(3) manual page for details

      Prototype:
      > (make-server-socket service . options)

      Parameters:

        service - a network service. (examples: "http", "ssh", "80", "22")
        ai-family(optional) - an address family specifier. <AF_INET> (default), <AF_INET6> or <AF_UNSPEC>.
        ai-socktype(optional) - a socket type specifier. <SOCK_STREAM> (default) or <SOCK_DGRAM>.
        ai-protocol(optional) - a protocol specifier. <IPPROTO_TCP>, <IPPROTO_UDP>, <IPPROTO_RAW> or 0(default).

      Returns:

        server socket.
  |#
  (define (make-server-socket service . options)
    (let-optionals* options ([ai-family AF_INET]
                             [ai-socktype SOCK_STREAM]
                             [ai-protocol 0])
      (%make-server-socket service ai-family ai-socktype ai-protocol)))

  #|
      Function: socket-recv

      Receives a binary data block from a socket.

      Socket-recv uses recv(2) to receive data.

      The arguments flags will be passed to recv(2) as a correspondent parameter.

      Refer to recv(2) manual page for details.

      Prototype:
      > (socket-recv socket size . flags)

      Parameters:

        socket - socket.
        size - size to receive.
        flags(optional) - flags to recv(2). Default values is 0.

      Returns:

        binary data as bytevector
  |#
  (define (socket-recv socket size . options)
    (let-optionals* options ([flags 0])
      (%socket-recv socket size flags)))

  #|
      Function: socket-recv!

      Receives a binary data block from a socket.

      Prototype:
      > (socket-recv! socket bytevector start size flags)

      Parameters:

        socket - socket.
        bytevector - buffer to store received data.
        start - index of buffer to written.
        size - size to receive.
        flags - flags to recv(2).

      Returns:

        data size
  |#
  (define (socket-recv! socket bv start size . options)
    (let-optionals* options ([flags 0])
      (%socket-recv! socket bv start size flags)))

  #|
      Function: socket-send

      Sends a binary data block to a socket.

      socket-send uses send(2) to send data.

      The arguments flags will be passed to send(2) as a correspondent parameter.

      Refer to send(2) manual page for details.

      Prototype:
      > (socket-send socket bytevector . flags)

      Parameters:

        socket - socket.
        bytevector - data to send.
        flags(optional) - flags to send(2). Default values is 0.

      Returns:

        sent data size.
  |#
  (define (socket-send socket data . options)
    (let-optionals* options ([flags 0])
      (%socket-send socket data flags)))

  #|
      Function: socket-close

      Closes a socket.

      On Windows, shutdown is called internally.

      Prototype:
      > (socket-close socket)

  |#


  #|
      Function: socket-shutdown

      Shutdowns a socket.

      Prototype:
      > (socket-shutdown socket how)

      Parameters:
        socket - socket.
        how - <SHUT_RD>, <SHUT_WR>, or <SHUT_RDWR>.

  |#

  #|
      Function: socket-accept

      Wait for an incoming connection request, and returns a fresh connected client socket.

      Prototype:
      > (accept socket)

      Returns:

        client socket.
  |#

  #|
      Function: socket-port

      Returns a fresh binary input/output port associated with a socket.

      Prototype:
      > (socket-port socket)

      Returns:

        binary input/output port.
  |#

  #|
      Function: call-with-socket

      Calls a procedure with a socket as an argument. This procedure has an analogy to call-with-port of (rnrs io ports).

      Prototype:
      > (call-with-socket socket proc)

      Returns:

        Returned value(s) of proc.
  |#
  (define (call-with-socket socket proc)
    (call-with-values
        (lambda () (proc socket))
      (lambda args
        (socket-close socket)
        (apply values args))))
  #|
      Constant: AF_INET
      ai-family:
  |#
  (define-const AF_INET)

  #|
      Constant: AF_INET6
      ai-family:
  |#
  (define-const AF_INET6)


  #|
      Constant: AF_UNSPEC
      ai-family:
  |#
  (define-const AF_UNSPEC)

  #|
      Constant: SOCK_STREAM
      ai-socktype:
  |#
  (define-const SOCK_STREAM)

  #|
      Constant: SOCK_DGRAM
      ai-socktype:
  |#
  (define-const SOCK_DGRAM)

;;   #|
;;       Constant: SOCK_RAW
;;       ai-socktype:
;;   |#
;;   (define-const SOCK_RAW)

  #|
      Constant: AI_ADDRCONFIG
      ai-flags:
  |#
  (define-const AI_ADDRCONFIG)

  #|
      Constant: AI_ALL
      ai-flags:
  |#
  (define-const AI_ALL)

  #|
      Constant: AI_CANONNAME
      ai-flags:
  |#
  (define-const AI_CANONNAME)

  #|
      Constant: AI_NUMERICHOST
      ai-flags:
  |#
  (define-const AI_NUMERICHOST)

  #|
      Constant: AI_NUMERICSERV
      ai-flags:
  |#
  (define-const AI_NUMERICSERV)

  #|
      Constant: AI_PASSIVE
      ai-flags:
  |#
  (define-const AI_PASSIVE)

  #|
      Constant: AI_V4MAPPED
      ai-flags:
  |#
  (define-const AI_V4MAPPED)

  #|
      Constant: IPPROTO_TCP
      ai-protocol:
  |#
  (define-const IPPROTO_TCP)

  #|
      Constant: IPPROTO_UDP
      ai-protocol:
  |#
  (define-const IPPROTO_UDP)

  #|
      Constant: IPPROTO_RAW
      ai-protocol:
  |#
  (define-const IPPROTO_RAW)

  #|
      Constant: SHUT_RD
  |#
  (define-const SHUT_RD)

  #|
      Constant: SHUT_WR
  |#
  (define-const SHUT_WR)

  #|
      Constant: SHUT_RDWR
  |#
  (define-const SHUT_RDWR)
)
