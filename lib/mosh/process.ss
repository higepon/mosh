; process.ss - Process utilities
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
;  $Id: process.ss 621 2008-11-09 06:22:47Z higepon $

#|
    Title: Process

    Process Management:
    The following procedures provide raw process management.

    library: (mosh process)

    Process Management Library
|#
(library (mosh process)
  (export (rename (%pipe pipe) (%fork fork) (%waitpid waitpid) (%spawn spawn)))
  (import (only (system) %spawn %waitpid %pipe %fork))

  #|
      Function: fork

      Fork the current process. Returns 0 if you're in the child process, and a child process' pid if you're in the parent process. All the opened file descriptors are shared between the parent and the child. See fork(2) of your system for details.

      Prototype:
      > (fork)

      Returns:

        0 if you're in the child process, and a child process' pid if you're in the parent process
  |#

  #|
      Function: waitpid

       This is an interface to waitpid(3), an extended version of wait.
       pid is an exact integer specifying which child(ren) to be waited.
       If it is a positive integer, it waits fot that specific child. If it is zero, it waits for any member of this process group. If it is -1, it waits for any child process. If it is less than -1, it waits for any child process whose process group id is equal to the absolute value of pid.

      Prototype:
      > (waitpid pid)

      Parameters:

        pid - pid of process to wait.

      Returns:

        The return values are two exact integers, the first one is the child process id, and the second is a status code.
  |#

  #|
      Function: spawn

      fork and exec.

      Prototype:
      > (spawn command args . io-list)

      Parameters:

        command - command string to spawn.
        args - list of command line arguments.
        io-list - (in out err). in, out and err should be binary-port or #f. #f means use parent's port.

      Example:
      (start code)
      ;; ls -l
      (let-values ([(pid cin cout cerr) (spawn "ls" '("-l") (list #f #f #f))])
        (waitpid pid))

      ;; get output as string
      (let-values ([(in out) (pipe)])
        (define (port->string p)
          (let loop ([ret '()][c (read-char p)])
            (if (eof-object? c)
                (list->string (reverse ret))
                (loop (cons c ret) (read-char p)))))
        (let-values ([(pid cin cout cerr) (spawn "ls" '("-l") (list #f out #f))])
          (close-port out)
          (write (port->string (transcoded-port in (make-transcoder (utf-8-codec)))))
          (close-port in)
          (waitpid pid)))
      (end code)

      Returns:

        pid in out err is returned as multiple values.
  |#

  #|
      Function: pipe

      Creates a pipe, and returns two ports. The first returned port is an input port and the second is an output port. The data put to the output port can be read from the input port.

      Prototype:
      > (pipe)

      Returns:

        Two ports
  |#
)
