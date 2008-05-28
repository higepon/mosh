Copyright (c) 2006, 2007 Abdulaziz Ghuloum and Kent Dybvig

Permission is hereby granted, free of charge, to any person obtaining a
copy of this software and associated documentation files (the "Software"),
to deal in the Software without restriction, including without limitation
the rights to use, copy, modify, merge, publish, distribute, sublicense,
and/or sell copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
DEALINGS IN THE SOFTWARE. 

------------------------------------------------------------------------
September 28, 2007
------------------------------------------------------------------------

This directory contains a reference implementation of the proposed
implicitly-phased libraries for R6RS.  The implementation is
portable, depending only on a small set of common primitives that
are readily available in many implementations. The implementation is
also written as a collection of R6RS libraries in order to take
advantage of the key R6RS libraries benefit: to easily isolate
implementation-dependent extensions in a mechanical way. 

------------------------------------------------------------------------
Supported Platforms:

The system is known to load and bootstrap itself properly on the
following Scheme implementations:

* Bigloo 3.0b
* Chez Scheme 7.2
* Chicken 2.710
* Gambit 4.0 beta 20
* Gauche 0.8.11
* Ikarus (Build 2007-09-24)
* Larceny 0.93
* MIT-Scheme 7.7.90.+
* MzScheme v371 [3m]
* Petite Chez Scheme 7.2
* Scheme48 1.7
* SISC (1.16.6)

For every supported platform, there is a "<impl>.r6rs.ss" file
containing a small number of compatibility definitions that allow
loading the expanded system (e.g. psyntax.pp) properly.

To port to another system, start by loading "psyntax.pp".  If you
get an error about undefined variables, look for how these variables
are defined in the other *.r6rs.ss files and port them over to your
implementation.  It would help to start with a supported
implementation that's most familiar to you.  Please let me know 
(email: aghuloum at cs.indiana.edu) if you get the system ported to
another platform.

------------------------------------------------------------------------
Basic Usage:

To run an r6rs script on your system, you'll need the following
files:
  * The script you want to run (say hello-world.ss).
  * The host.r6rs.ss file for your platform.
  * The pre-built/psyntax-host.pp file.

For example, we may have a script that looks like:

         $ cat hello-world.ss 
         (import (rnrs io simple))
         (display "Hello World\n")

We copy our host's pre-built/psyntax-host.pp to psyntax.pp
For Petite Chez Scheme, we'd do:

        $ cp pre-built/psyntax-petite.pp .

Run your implementation's r6rs.ss file passing the script name as
an extra argument.  For example, under petite, we do:

        $ petite --script petite.r6rs.ss hello-world.ss 
        r6rs psyntax ready
        Hello World

If you want to write a library, say (my-library) as:

        $ cat my-library.ss 
        (library (my-library)
          (export print-hello)
          (import (rnrs))
          (define (print-hello)
             (display "Hello World\n")))

And then write your hello-world.ss script as:

        $ cat hello-world.ss 
        (import (my-library))
        (print-hello)

Then, you can run it as before:

        $ petite --script petite.r6rs.ss hello-world.ss 
        r6rs psyntax ready
        Hello World

The library system takes care of mapping the library names that 
you import to file names via a primitive simple mapping:

       (foo)         =>    ./foo.ss
       (foo bar)     =>    ./foo/bar.ss
       ...

To avoid any problems now, stick with names that contain characters 
in [a..z], [A..Z], [0..9], "-", "_", "~", and ".".  If you have
ideas about a general library-name->file-name mapping that you'd
like to share, please do email me.

That's pretty much it as far as basic usage is concerned.  Let me
know if you have any problems.

------------------------------------------------------------------------
Structure of the system:

Makefile  
  Used to build the system under all supported platforms.

README.txt
  The file you're reading right now.

psyntax-buildscript.ss
  An r6rs script that's used to bootstrap the whole system.

*.r6rs.ss 
  Compatibility files for supported platforms with the exception of
  scheme48.r6rs.ss and kawa.r6rs.ss which are not working yet..

pre-built/*.pp
  Pre-built expanded files for the supported platforms.

session-id
  Used by gensym to generate unique ids across sessions.
  
examples hello-world.ss
  Example scripts.


psyntax/internal.ss                              (psyntax internal)
  Contains definitions of some internal procedures that are used by
  the system but may need to be modified to get optimal performance
  and usability on any platform.  It is currently written as a
  common-denominator of all supported platforms.

psyntax/builders.ss                              (psyntax builders)
  Exports procedures/macros that are used by the expander to build
  the output expression.  For example, build-letrec is used to build
  letrec expressions.  Implementations with tight integration may
  replace the builders with constructors of compiler-internal data
  structures.

psyntax/expander.ss                              (psynatx expander)
  The core of the expander.

psyntax/library-manager.ss                (psyntax library-manager)
  The library manager keeps track of what libraries are installed
  and available on the system.  It takes care of visiting/invoking
  such libraries as well as loading them from source files.  

psyntax/config.ss                                  (psyntax config)
  This is a compile-time configuration file for determining what
  forms the implementation supports (e.g. is case-lambda supported 
  or should it be expanded).  The provided configuration is one that
  works for all implementation (common denominator).

psyntax/compat.ss                                  (psyntax compat)
  This is a compatibility file for some non-r6rs forms that are used
  in other libraries but can be implemented portably in terms of
  other r6rs features.  make-parameter, parameterize, and
  define-record.

psyntax/main.ss                                      (psyntax main)
  This library is the main entry point to the r6rs world.  It 
  prints a greeting message and processes the file given in the
  command line argument as an r6rs-script.


------------------------------------------------------------------------
Non-R6RS requirements:

void:  The procedure void is used in the output of the expander when
we don't care about a value (e.g. to provide the initial values for
the letrec*/internal-defines).  A call to void should not signal any
errors.  (define (void) (if #f #f)) suffices for this purpose.

pretty-print:  The procedure pretty-print should take one or two
values: an expression (code) to be printed and, optionally, an
output port.  It should pretty-print the code to that port (or the
current-output-port if a port is not provided.  Implementations with
no pretty printer can (define pretty-print write) for this purpose
but the output won't be very readable.

eval-core:  This procedure should take a core s-expression and
evaluate it in a "top-level" environment.  R5RS implementations can
define it as 
(define (eval-core x) (eval x (interaction-environment)))

gensym: The procedure gensym should create a globally unique symbol
with read/write invariance.  It is used to create unique locations
for exported identifiers, unique names for lexical variables, and
unique labels in the expand-time environment.  Of the supported
Scheme implementations, only Chez Scheme and Ikarus provide a usable
gensym implementation for this purpose; yet their printed
represenation of gensyms are not portable (cannot be read by the
other R5RS implementations).  For this, we provide a gensym
implementation that's semi portable but relies on an external state
(session-id file) to ensure that the sequence of symbols generated
are unique across sessions.  Gensym takes an optional argument that
is either a string or a symbol that can be used to correlate the
source name of the identifier with its unique name.  The output of
our implementation of gensym looks like
    <name>$<session-id>$<gensym-id>
where both session-id and gensym-id are nonnegative integers.

symbol-value: This procedure takes a symbol (typically a gensym) and
returns the value associated with it in the global environment.  The
symbol is guaranteed to be initialized either via a global define
or via set-symbol-value! (below).

set-symbol-value!:  This procedure takes a symbol (typically a
gensym) and a value and associates the value with the symbol in the
global environment (in a way that eval-core can properly see it).

------------------------------------------------------------------------
Enjoy!
