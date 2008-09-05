/*
 *  read.cpp - Shiro Kawai's read.
 *
 *  Ported to Mosh by higepon@users.sourceforge.jp.
 *  $Id$
 */

/*
 * read.c - reader
 *
 *   Copyright (c) 2000-2007  Shiro Kawai  <shiro@acm.org>
 *
 *   Redistribution and use in source and binary forms, with or without
 *   modification, are permitted provided that the following conditions
 *   are met:
 *
 *   1. Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *
 *   2. Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *
 *   3. Neither the name of the authors nor the names of its contributors
 *      may be used to endorse or promote products derived from this
 *      software without specific prior written permission.
 *
 *   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 *   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 *   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 *   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 *   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 *   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 *   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 *   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 *   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *  read.c,v 1.92 2007/04/12 03:26:55 shirok Exp
 */

#include <stdio.h>
#include <ctype.h>
#include <math.h>
#ifndef MONA_SCHEME
#define LIBGAUCHE_BODY
#include "gauche.h"
#include "gauche/vm.h"
#include "gauche/port.h"
#include "gauche/builtin-syms.h"
#endif

#ifdef MONA_SCHEME

#include "scheme.h"
#include "SString.h"
#include "ErrorProcedures.h"
#include "StringProcedures.h"
#include "TextualInputPort.h"

using namespace scheme;

#define APPEND1(start, last, obj)                               \
{                                                               \
    if (start.isNil()) {                                        \
        (start) = (last) = Object::cons((obj), Object::Nil);    \
    } else {                                                    \
        last.cdr() = Object::cons((obj), Object::Nil);          \
        (last) = last.cdr();                                    \
    }                                                           \
}

#define SOME_VALUE (1)
#define ScmObj scheme::Object
#define SCM_NIL Object::Nil
#define SCM_STRING(obj) (obj.toString())
#define SCM_FALSEP(obj) (obju.isFalse())
#define SCM_VECTORP(obj) (obj.isVector())
#define SCM_PORT_DIR(port) SOME_VALUE
#define SCM_PORT_INPUT SOME_VALUE
#define Scm_VM() ((ScmVM*)SOME_VALUE)
#define Scm_Error(...) printf(__VA_ARGS__)
#define Scm_ReadError(port, ...) printf(__VA_ARGS__)
#define Scm_PortLine(port) port->getLine()
#if 0
typedef scheme::InputFilePort ScmPort;
#define SCM_PORTP(port) (port.isInputFilePort())
#define SCM_PORT(port) (port.toInputFilePort())
#define Scm_GetcUnsafe(port) port->getc()
#define Scm_GetbUnsafe(port) port->getc()
#define Scm_UngetcUnsafe(c, port) port->ungetc(c)
#else
typedef scheme::TextualInputPort ScmPort;
#define SCM_PORTP(port) (port.isTextualInputPort())
#define SCM_PORT(port) (port.toTextualInputPort())
#define Scm_GetcUnsafe(port) port->getChar()
#define Scm_GetbUnsafe(port) port->getChar()
#define Scm_UngetcUnsafe(c, port) port->unGetChar(c)
#endif

#define SCM_TRUE Object::True
#define SCM_FALSE Object::False
#define SCM_EOF Object::Eof
#define SCM_EOFP(obj) (obj.isEof())
#define SCM_UNDEFINED Object::Undef
#define SCM_UNDEFINEDP(obj) (obj.isUndef())
#define SCM_LIST1 list1
#define SCM_LIST2 Pair::list2
#define SCM_SYM_QUOTE Symbol::QUOTE
#define SCM_SYM_QUASIQUOTE Symbol::QUASIQUOTE
#define SCM_SYM_UNQUOTE_SPLICING  Symbol::UNQUOTE_SPLICING
#define SCM_SYM_UNQUOTE Symbol::UNQUOTE
#define SCM_SYM_SYNTAX Symbol::SYNTAX
#define SCM_SYM_QUACISYNTAX Symbol::QUASISYNTAX
#define SCM_SYM_UNSYNTAX Symbol::UNSYNTAX
#define SCM_SYM_UNSYNTAX_SPLICING Symbol::UNSYNTAX_SPLICING
#define SCM_SET_CDR(a, b) (a.cdr() = b)
#define SCM_SET_CAR(a, b) (a.car() = b)
#define SCM_APPEND1(start, last, obj) APPEND1(start, last, obj)
#define SCM_PAIRP(p) p.isPair
#define SCM_CAR(p) p.car()
#define SCM_CDR(p) p.cdr()
#define Scm_Cons(a, b) Object::cons(a, b)
#define SCM_PAIR(obj) obj.toPair()
#define SCM_CHAR_INVALID        ((ScmChar)(-1)) /* indicate invalid char */
#define Scm_UcsToChar(c) (c)
#define ScmString scheme::String
#define Scm_Intern(s) Symbol::intern(s->data().c_str())
#define SCM_ASSERT(expr)   {if (!(expr)) printf("assert"#expr);}
#define SCM_MAKE_CHAR(ch) Object::makeChar(ch)
#define Scm_Read scheme::read
#define Scm_RegComp(a, b) (a)
#define SCM_REGEXP_CASE_FOLD      (1L<<0)
#define makeString(s) Object::makeString(s)

#define SCM_FOR_EACH(p, list) \
    for((p) = (list); SCM_PAIRP(p); (p) = SCM_CDR(p))

ScmObj Scm_ListToVector(ScmObj l, int start, int end)
{
    ScmObj ov = Object::makeVector(Pair::length(l));
    Vector* v = ov.toVector();
    for (int i = 0; l != Object::Nil; l = l.cdr(), i++) {
        v->set(i, l.car());
    }
    return ov;
}

#define SCM_SYM_STRING_INTERPOLATE ((ScmObj)SOME_VALUE)

#include "oniguruma.h"
#if WORDS_BIGENDIAN
#define ONIG_ENCODING ONIG_ENCODING_UTF32_BE
#else
#define ONIG_ENCODING ONIG_ENCODING_UTF32_LE
#endif

bool match(const char* pattern, const char* text)
{
    regex_t* regexp;
    OnigErrorInfo einfo;
    int r = onig_new(&regexp,
                     (const uint8_t*)pattern,
                     (const uint8_t*)(pattern + strlen(pattern)),
                     ONIG_OPTION_DEFAULT,
                     ONIG_ENCODING_ASCII,
                     ONIG_SYNTAX_RUBY,
                     &einfo);
    OnigRegion* region = onig_region_new();
    const uint8_t* start = (const uint8_t*)text;
    const uint8_t* end   = (const uint8_t*)(text + strlen(text));
    const uint8_t* range = end;
    r = onig_search(regexp, start, end, start, range, region, ONIG_OPTION_NONE);
    if (r >= 0) {
        return true;
    } else {
        return false;
    }

}

ScmObj Scm_StringToNumber(ScmString* s, int base, int dummy)
{
    bool exact = false;
    ucs4string str = s->data();
    int length = str.size();
    const char* p = str.ascii_c_str();

    // todo
    if (match("#[e|i|x]", p) ||
        match("\\.\\d+", p) ||
        match("\\+nan\\.0", p) ||
        match("\\+inf\\.0", p) ||
        match("-inf\\.0", p)
        ) {
        return Object::makeInt(0);
    }

    // strtol returns no error for "." or "...".
    // strtol is !
    if (strcmp(p, "+") == 0 || strcmp(p, "-") == 0 || p[0] == '.' || (p[0] == '-' && !isdigit(p[1]))) {
        return SCM_FALSE;
    }


    errno = 0;
    long long ret = strtoll(p, NULL, base);
    if ((errno == ERANGE && (ret == LONG_MAX || ret == LONG_MIN))
        || (errno != 0 && ret == 0)) {
        return SCM_FALSE;
    } else {
        return Object::makeInt(ret);
    }
}


/* Some useful macros */
#ifndef FALSE
#define FALSE 0
#endif
#ifndef TRUE
#define TRUE (!FALSE)
#endif

typedef int ScmHashTable;
typedef int ScmVM;


typedef int ScmChar;

// from gauche.h
typedef struct ScmReadContextRec EXTEND_GC {
    int flags;                  /* see below */
    ScmHashTable *table;        /* used internally. */
    ScmObj pending;             /* used internally. */
} ScmReadContext;

struct ScmWriteContextRec EXTEND_GC {
    short mode;                 /* print mode */
    short flags;                /* internal */
    int limit;                  /* internal */
    int ncirc;                  /* internal */
    ScmHashTable *table;        /* internal */
    ScmObj obj;                 /* internal */
};

typedef struct ScmWriteContextRec ScmWriteContext;

enum {
    SCM_READ_SOURCE_INFO = (1L<<0),  /* preserving souce file information */
    SCM_READ_CASE_FOLD   = (1L<<1),  /* case-fold read */
    SCM_READ_LITERAL_IMMUTABLE = (1L<<2), /* literal should be read as immutable */
    SCM_READ_DISABLE_CTOR = (1L<<3), /* disable #,() */
    SCM_READ_RECURSIVELY = (1L<<4)   /* used internally. */
};

#define SCM_GETB(b, p)     (b = Scm_GetbUnsafe(SCM_PORT(p)))
#define SCM_GETC(c, p)     (c = Scm_GetcUnsafe(p))
#define SCM_UNGETC(c, port) Scm_UngetcUnsafe(c, port)

const char* objectToCStr(Object obj, bool inList = false);

/* '0' -> 0, 'a' -> 10, etc.
   Radix is assumed in the range [2, 36] */
int Scm_DigitToInt(ScmChar ch, int radix)
{
    if (ch < '0') return -1;
    if (radix <= 10) {
        if (ch < '0' + radix) return (ch - '0');
    } else {
        if (ch <= '9') return (ch - '0');
        if (ch < 'A') return -1;
        if (ch < 'A' + radix - 10) return (ch - 'A' + 10);
        if (ch < 'a') return -1;
        if (ch < 'a' + radix - 10) return (ch - 'a' + 10);
    }
    return -1;
}



ScmChar Scm_ReadXdigitsFromPort(ScmPort *port, int ndigits,
                                char *buf, int *nread)
{
    int i, c, val = 0, dig;

    for (i = 0; i < ndigits; i++) {
        SCM_GETC(c, port);
        if (c == EOF) break;
        dig = Scm_DigitToInt(c, 16);
        if (dig < 0) {
            SCM_UNGETC(c, port);
            break;
        }
        buf[i] = (char)c;       /* we know c is single byte char here. */
        val = val * 16 + dig;
    }
    *nread = i;
    if (i < ndigits) { /* error */
        return SCM_CHAR_INVALID;
    } else {
        return (ScmChar)val;
    }
}

ScmChar Scm_ReadXdigitsFromString(const char *buf, int ndigits,
                                  const char **nextbuf)
{
    int i, val = 0;
    for (i=0; i<ndigits; i++) {
        if (!isxdigit(buf[i])) {
            if (nextbuf == NULL) return SCM_CHAR_INVALID;
            else {
                *nextbuf = buf;
                return val;
            }
        }
        val = val * 16 + Scm_DigitToInt(buf[i], 16);
    }
    return (ScmChar)val;
}


#endif

/*
 * READ
 */

static void   read_context_init(ScmVM *vm, ScmReadContext *ctx);
static void   read_context_flush(ScmReadContext *ctx);
static ScmObj read_internal(ScmPort *port, ScmReadContext *ctx);
static ScmObj read_item(ScmPort *port, ScmReadContext *ctx);
static ScmObj read_list(ScmPort *port, ScmChar closer, ScmReadContext *ctx);
static ScmObj read_vector(ScmPort *port, ScmChar closer, ScmReadContext *ctx);
static ScmObj read_string(ScmPort *port, int incompletep, ScmReadContext *ctx);
static ScmObj read_quoted(ScmPort *port, ScmObj quoter, ScmReadContext *ctx);
static ScmObj read_char(ScmPort *port, ScmReadContext *ctx);
static ScmObj read_word(ScmPort *port, ScmChar initial, ScmReadContext *ctx,
                        int temp_case_fold);
static ScmObj read_symbol(ScmPort *port, ScmChar initial, ScmReadContext *ctx);
static ScmObj read_number(ScmPort *port, ScmChar initial, ScmReadContext *ctx);
static ScmObj read_symbol_or_number(ScmPort *port, ScmChar initial, ScmReadContext *ctx);
static ScmObj read_escaped_symbol(ScmPort *port, ScmChar delim);
#ifndef MONA_SCHEME
static ScmObj read_keyword(ScmPort *port, ScmReadContext *ctx);
#endif
static ScmObj read_regexp(ScmPort *port);
#ifndef MONA_SCHEME
static ScmObj read_charset(ScmPort *port);
static ScmObj read_sharp_comma(ScmPort *port, ScmReadContext *ctx);
static ScmObj process_sharp_comma(ScmPort *port, ScmObj key, ScmObj args,
                                  ScmReadContext *ctx, int has_ref);
static ScmObj read_reference(ScmPort *port, ScmChar ch, ScmReadContext *ctx);
static ScmObj maybe_uvector(ScmPort *port, char c, ScmReadContext *ctx);
#endif

/* Special hook for SRFI-4 syntax */
ScmObj (*Scm_ReadUvectorHook)(ScmPort *port, const char *tag,
                              ScmReadContext *ctx) = NULL;

#ifndef MONA_SCHEME
/* Table of 'read-time constructor' in SRFI-10 */
static struct {
    ScmHashTable *table;
    ScmInternalMutex mutex;

} readCtorData = { NULL };
#endif
/*----------------------------------------------------------------
 * Entry points
 *   Note: Entire read operation are done while locking the input port.
 *   So we can use 'unsafe' version of port operations inside this file.
 *   The lock is removed if reader routine signals an error.  It is OK
 *   to call read routine recursively.
 */
ScmObj Scm_ReadWithContext(ScmObj port, ScmReadContext *ctx)
{
#ifdef MONA_SCHEME
    ScmObj r = SCM_NIL;
#else
    ScmVM *vm = Scm_VM();
    volatile ScmObj r = SCM_NIL;
#endif

    if (!SCM_PORTP(port) || SCM_PORT_DIR(port) != SCM_PORT_INPUT) {
#ifdef MONA_SCHEME
//        Scm_Error("input port required:");// %s", objectToCStr(port));
#else
        Scm_Error("input port required: %S", port);
#endif
    }
    if (!(ctx->flags & SCM_READ_RECURSIVELY)) {
        ctx->table = NULL;
        ctx->pending = SCM_NIL;
    }
#ifndef MONA_SCHEME
    if (PORT_LOCKED(SCM_PORT(port), vm)) {
#endif
        r = read_item(SCM_PORT(port), ctx);

#ifndef MONA_SCHEME
    } else {
        PORT_LOCK(SCM_PORT(port), vm);
        PORT_SAFE_CALL(SCM_PORT(port), r = read_item(SCM_PORT(port), ctx));
        PORT_UNLOCK(SCM_PORT(port));
    }
#endif
    if (!(ctx->flags & SCM_READ_RECURSIVELY)) {
        read_context_flush(ctx);
    }
    return r;
}

ScmObj Scm_ReadWithContext(ScmPort* port, ScmReadContext *ctx)
{
    ScmObj r = SCM_NIL;
    if (!(ctx->flags & SCM_READ_RECURSIVELY)) {
        ctx->table = NULL;
        ctx->pending = SCM_NIL;
    }
    r = read_item(port, ctx);

    if (!(ctx->flags & SCM_READ_RECURSIVELY)) {
        read_context_flush(ctx);
    }
    return r;
}

static jmp_buf returnPoint;

#define TRY if (setjmp(returnPoint) == 0)
#define CATCH else
#define RAISE_READ_ERROR0(message) raiseReadError(port, UC(message), Object::Nil)
#define RAISE_READ_ERROR1(message, value1) raiseReadError(port, UC(message), Pair::list1(value1))
#define RAISE_READ_ERROR2(message, value1, value2) raiseReadError(port, UC(message), Pair::list2(value1, value2))

void raiseReadError(TextualInputPort* port, const ucs4char* message, Object values)
{
    port->setError(format(UC("~a at ~a:~d"),
                          Pair::list3(format(message, values),
                                      port->toString(),
                                      Object::makeInt(port->getLine()))));
    longjmp(returnPoint, -1);
}

ScmObj Scm_Read(ScmPort* port, bool& errorOccured)
{
    ScmReadContext ctx;
    read_context_init(Scm_VM(), &ctx);
    ScmObj o = Object::Nil;
    TRY {
        o = Scm_ReadWithContext(port, &ctx);
    } CATCH {
        errorOccured = true;
        o = Object::Undef;
    }
    return o;
}




#ifndef MONA_SCHEME
/* Convenience functions */
ScmObj Scm_ReadFromString(ScmString *str)
{
    ScmObj inp = Scm_MakeInputStringPort(str, TRUE), r;
    ScmReadContext ctx;
    read_context_init(Scm_VM(), &ctx);
    r = read_item(SCM_PORT(inp), &ctx);
    read_context_flush(&ctx);
    return r;
}

ScmObj Scm_ReadFromCString(const char *cstr)
{
    ScmObj s = SCM_MAKE_STR_IMMUTABLE(cstr);
    ScmObj inp = Scm_MakeInputStringPort(SCM_STRING(s), TRUE);
    ScmObj r;
    ScmReadContext ctx;
    read_context_init(Scm_VM(), &ctx);
    r = read_item(SCM_PORT(inp), &ctx);
    read_context_flush(&ctx);
    return r;
}

#endif

ScmObj Scm_ReadListWithContext(ScmObj port, ScmChar closer, ScmReadContext *ctx)
{
#ifdef MONA_SCHEME
    ScmObj r = SCM_NIL;
#else
    ScmVM *vm = Scm_VM();
    volatile ScmObj r = SCM_NIL;
#endif

    if (!SCM_PORTP(port) || SCM_PORT_DIR(port) != SCM_PORT_INPUT) {
#ifdef MONA_SCHEME
        // todo
//        Scm_Error("input port required: ");//%s", objectToCStr(port));
#else
        Scm_Error("input port required: %S", port);
#endif
    }
    if (!(ctx->flags & SCM_READ_RECURSIVELY)) {
        ctx->table = NULL;
        ctx->pending = SCM_NIL;
    }
#ifndef MONA_SCHEME
    if (PORT_LOCKED(SCM_PORT(port), vm)) {
#endif
        r = read_list(SCM_PORT(port), closer, ctx);
#ifndef MONA_SCHEME
    } else {
        PORT_LOCK(SCM_PORT(port), vm);
        PORT_SAFE_CALL(SCM_PORT(port), r = read_list(SCM_PORT(port), closer, ctx));
        PORT_UNLOCK(SCM_PORT(port));
    }
#endif
    if (!(ctx->flags & SCM_READ_RECURSIVELY)) {
        read_context_flush(ctx);
    }
    return r;
}

ScmObj Scm_ReadList(ScmObj port, ScmChar closer)
{
    ScmReadContext ctx;
    read_context_init(Scm_VM(), &ctx);
    return Scm_ReadListWithContext(port, closer, &ctx);
}

static void read_context_init(ScmVM *vm, ScmReadContext *ctx)
{
    ctx->flags = SCM_READ_SOURCE_INFO;
#ifndef MONA_SCHEME
    if (SCM_VM_RUNTIME_FLAG_IS_SET(vm, SCM_CASE_FOLD)) {
        ctx->flags |= SCM_READ_CASE_FOLD;
    }
#endif
    ctx->table = NULL;
    ctx->pending = SCM_NIL;
}

/*----------------------------------------------------------------
 * Error
 */
#ifndef MONA_SCHEME
void Scm_ReadError(ScmPort *port, const char *msg, ...)
{
    ScmObj ostr = Scm_MakeOutputStringPort(TRUE);
    ScmObj name = Scm_PortName(port);
    ScmObj rerr;
    int line = Scm_PortLine(port);
    va_list ap;

    Scm_Printf(SCM_PORT(ostr), "Read error at %S:",
               SCM_STRINGP(name)? name : SCM_OBJ(SCM_MAKE_STR("??")));
    if (line >= 0) {
        Scm_Printf(SCM_PORT(ostr), "line %d: ", line);
    }
    va_start(ap, msg);
    Scm_Vprintf(SCM_PORT(ostr), msg, ap, TRUE);
    va_end(ap);

    rerr = Scm_MakeReadError(Scm_GetOutputString(SCM_PORT(ostr), 0),
                             port, line);
    Scm_Raise(rerr);
}
#endif

/*----------------------------------------------------------------
 * Read reference
 */

/* Read reference is a proxy object to for referenced object (#N=).
 */

#ifndef MONA_SCHEME
static void read_reference_print(ScmObj obj, ScmPort *port,
                                 ScmWriteContext *ctx);
SCM_DEFINE_BUILTIN_CLASS_SIMPLE(Scm_ReadReferenceClass, read_reference_print);

ScmObj Scm_MakeReadReference(void)
{
    ScmReadReference *a;
    a = SCM_NEW(ScmReadReference);
    SCM_SET_CLASS(a, SCM_CLASS_READ_REFERENCE);
    a->value = SCM_UNBOUND;
    return SCM_OBJ(a);
}

static void read_reference_print(ScmObj obj, ScmPort *port,
                                 ScmWriteContext *ctx)
{
    Scm_Printf(port, "#<read-reference>");
}

static void ref_push(ScmReadContext *ctx, ScmObj obj, ScmObj finisher)
{
    ctx->pending = Scm_Acons(obj, finisher, ctx->pending);
}

static ScmObj ref_val(ScmObj ref)
{
    if (!SCM_READ_REFERENCE_REALIZED(ref)) {
        Scm_Error("reader encontered unresolved read reference.  Implementation error?");
    }
    return SCM_READ_REFERENCE(ref)->value;
}

static ScmObj ref_register(ScmReadContext *ctx, ScmObj obj, int refnum)
{
    SCM_ASSERT(ctx->table);
    Scm_HashTablePut(ctx->table, SCM_MAKE_INT(refnum), obj);
    return obj;
}
#endif
/* ctx->pending contains an assoc list of objects who contains read reference
   which should be resolved.
   The car of each entry is the object that needs to be fixed, and the
   cdr of eacy entry may contain a finisher procedure (if the object is
   created by read-time constructor.
*/
static void read_context_flush(ScmReadContext *ctx)
{
#ifndef MONA_SCHEME
    ScmObj cp, ep, entry, obj, finisher;

    SCM_FOR_EACH(cp, ctx->pending) {
        entry = SCM_CAR(cp);
        SCM_ASSERT(SCM_PAIRP(entry));
        obj = SCM_CAR(entry);
        finisher = SCM_CDR(entry);

        if (!SCM_FALSEP(finisher)) {
            Scm_ApplyRec(finisher, SCM_LIST1(obj));
        } else if (SCM_PAIRP(obj)) {
            SCM_FOR_EACH(ep, obj) {
                if (SCM_READ_REFERENCE_P(SCM_CAR(ep))) {
                    SCM_SET_CAR(ep, ref_val(SCM_CAR(ep)));
                }
                if (SCM_READ_REFERENCE_P(SCM_CDR(ep))) {
                    /* in case we have (... . #N#) */
                    SCM_SET_CDR(ep, ref_val(SCM_CDR(ep)));
                    break;
                }
            }
        } else if (SCM_VECTORP(obj)) {
            int i, len = SCM_VECTOR_SIZE(obj);
            for (i=0; i<len; i++) {
                ep = SCM_VECTOR_ELEMENT(obj, i);
                if (SCM_READ_REFERENCE_P(ep)) {
                    SCM_VECTOR_ELEMENTS(obj)[i] = ref_val(ep);
                }
            }
        } else {
            Scm_Error("read_context_flush: recursive reference only supported with vector and lists");
        }
    }
#endif
}

/*----------------------------------------------------------------
 * Miscellaneous routines
 */

/* Table of initial 128 bytes of ASCII characters to dispatch for
   special meanings.
    bit 0 : a valid constituent char of words
    bit 1 : candidate of case folding

   NB: '#' is marked as a constituent char, in order to read a possible
   number as a word in read_word.  The leading '#' is recognized by
   read_internal and will not be passed to read_word.
*/
static unsigned char ctypes[] = {
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
 /*     !   "   #   $   %   &   '   (   )   *   +   ,   -   .   /  */
    0,  1,  0,  1,  1,  1,  1,  0,  0,  0,  1,  1,  0,  1,  1,  1,
 /* 0   1   2   3   4   5   6   7   8   9   :   ;   <   =   >   ?  */
    1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  0,  1,  1,  1,  1,
 /* @   A   B   C   D   E   F   G   H   I   J   K   L   M   N   O  */
    1,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,
 /* P   Q   R   S   T   U   V   W   X   Y   Z   [   \   ]   ^   _  */
    3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  0,  0,  0,  1,  1,
 /* `   a   b   c   d   e   f   g   h   i   j   k   l   m   n   o  */
    0,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
 /* p   q   r   s   t   u   v   w   x   y   z   {   |   }   ~   ^? */
    1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  0,  0,  0,  1,  0,
};

inline static int char_word_constituent(int c)
{
    return (c >= 128 || (c >= 0 && (ctypes[(unsigned char)c]&1)));
}

inline static int char_word_case_fold(int c)
{
    return (c >= 0 && c < 128 && (ctypes[(unsigned char)c]&2));
}

static void read_nested_comment(ScmPort *port, ScmReadContext *ctx)
{
    int nesting = 0;
    int line = Scm_PortLine(port);
    ScmChar c, c1;

    for (c = Scm_GetcUnsafe(port);;) {
        switch (c) {
        case '#':
            c1 = Scm_GetcUnsafe(port);
            if (c1 == '|')   { nesting++; break; }
            else if (c1 == EOF) goto eof;
            else c = c1;
            continue;
        case '|':
            c1 = Scm_GetcUnsafe(port);
            if (c1 == '#') {
                if (nesting-- == 0) {
                    return;
                }
                break;
            }
            else if (c1 == EOF) goto eof;
            else c = c1;
            continue;
        case EOF:
          eof:
            RAISE_READ_ERROR1("encountered EOF inside nested multi-line comment (comment begins at line ~d)", Object::makeInt(line));
        default:
            break;
        }
        c = Scm_GetcUnsafe(port);
    }
}

static int skipws(ScmPort *port, ScmReadContext *ctx)
{
    for (;;) {
        int c = Scm_GetcUnsafe(port);
        if (c == EOF) return c;
        if (c <= 256 && isspace(c)) continue;
        if (c == ';') {
            for (;;) {
                /* NB: comment may contain unexpected character code.
                   for the safety, we read bytes here. */
                c = Scm_GetbUnsafe(port);
                if (c == '\n') {
#ifndef MONA_SCHEME
                    /* oops.  ugly. */
                    port->line++;
#endif
                    break;
                }
                if (c == EOF) return EOF;
            }
            continue;
        }
        return c;
    }
}

static ScmObj read_bytevector(ScmPort* port, ScmReadContext *ctx)
{
    ucs4char ch = Scm_GetcUnsafe(port);
    if (ch == 'u') {
        ch = Scm_GetcUnsafe(port);
        if (ch == '8') {
            ch = Scm_GetcUnsafe(port);
            if (ch == '(') {
                const Object list = read_list(port, ')', ctx);
                for (Object p = list; !p.isNil(); p = p.cdr()) {
                    const Object number = p.car();
                    if (p.car().isInt()) {
                        const int value = p.car().toInt();
                        if (value < -128 || value > 127) {
                            RAISE_READ_ERROR0("malformed bytevector");
                        }
                    } else {
                        RAISE_READ_ERROR0("malformed bytevector");
                    }
                }
                return Object::makeByteVector(list);
            } else {
                RAISE_READ_ERROR0("malformed bytevector");
            }
        } else {
            RAISE_READ_ERROR0("malformed bytevector");
        }
    } else {
        RAISE_READ_ERROR0("malformed bytevector");
    }
    return Object::Undef;
}

static ScmObj read_internal(ScmPort *port, ScmReadContext *ctx)
{
    int c = skipws(port, ctx);
    switch (c) {
    case '(':
        return read_list(port, ')', ctx);
    case '"':
        return read_string(port, FALSE, ctx);
    case '#':
        {
            int c1 = Scm_GetcUnsafe(port);
            switch (c1) {
            case EOF:
                RAISE_READ_ERROR0("premature #-sequence at EOF");
            case 't':; case 'T': return SCM_TRUE;
            case 'v':
                return read_bytevector(port, ctx);
#ifndef MONA_SCHEME
            case 'f':; case 'F': return maybe_uvector(port, 'f', ctx);
            case 's':; case 'S': return maybe_uvector(port, 's', ctx);
            case 'u':; case 'U': return maybe_uvector(port, 'u', ctx);
#else
            case 'f':; case 'F': return SCM_FALSE;
#endif
            case '(':
                return read_vector(port, ')', ctx);
            case '\\':
                return read_char(port, ctx);
            case 'x':; case 'X':; case 'o':; case 'O':;
            case 'b':; case 'B':; case 'd':; case 'D':;
            case 'e':; case 'E':; case 'i':; case 'I':;
                Scm_UngetcUnsafe(c1, port);
                return read_number(port, c, ctx);
            case '!':
                /* allow `#!' magic of executable */
                for (;;) {
                    c = Scm_GetcUnsafe(port);
                    if (c == '\n') return SCM_UNDEFINED;
                    if (c == EOF) return SCM_EOF;
                }
            case '/':
                /* #/.../ literal regexp */
                return read_regexp(port);
#ifndef MONA_SCHEME
            case '[':
                /* #[...] literal charset */
                return read_charset(port);
            case ',':
                /* #,(form) - SRFI-10 read-time macro */
                return read_sharp_comma(port, ctx);
#endif
            case '|':
                /* #| - block comment (SRFI-30)
                   it is equivalent to whitespace, so we return #<undef> */
                read_nested_comment(port, ctx);
                return SCM_UNDEFINED;
#ifndef MONA_SCHEME
            case '`':
                /* #`"..." is a special syntax of #,(string-interpolate "...") */
                {
                    ScmObj form = read_item(port, ctx);
                    return process_sharp_comma(port,
                                               SCM_SYM_STRING_INTERPOLATE,
                                               SCM_LIST1(form), ctx, FALSE);
                }
            case '?':
                /* #? - debug directives */
                {
                    int c2;
                    ScmObj form;

                    c2 = Scm_GetcUnsafe(port);
                    switch (c2) {
                    case '=':
                        /* #?=form - debug print */
                        form = read_item(port, ctx);
                        return SCM_LIST2(SCM_SYM_DEBUG, form);
                    case EOF:
                        return SCM_EOF;
                    default:
                        RAISE_READ_ERROR1("unsupported #?-syntax: #?~a", Object::makeChar(c2));
                    }
                }
            case '0': case '1': case '2': case '3': case '4':
            case '5': case '6': case '7': case '8': case '9':
                /* #N# or #N= form */
                return read_reference(port, c1, ctx);
#endif
            case '*':
                /* #*"...." byte string
                   #*01001001 for bit vector, maybe in future. */
                {
                    int c2;
                    c2 = Scm_GetcUnsafe(port);
                    if (c2 == '"') return read_string(port, TRUE, ctx);
                    RAISE_READ_ERROR1("unsupported #*-syntax: #*~a", Object::makeChar(c2));
                }
            case ';':
                /* #;expr - comment out sexpr */
                {
                    int orig = ctx->flags;
                    ctx->flags |= SCM_READ_DISABLE_CTOR;
                    read_item(port, ctx); /* read and discard */
                    ctx->flags = orig;
                    return SCM_UNDEFINED; /* indicate this is a comment */
                }
            case '\'':
                /* (syntax form) */
                {
                    ScmObj form = read_item(port, ctx);
                    return SCM_LIST2(SCM_SYM_SYNTAX, form);
                }
            case '`':
                /* (quasisyntax form) */
                {
                    ScmObj form = read_item(port, ctx);
                    return SCM_LIST2(SCM_SYM_QUACISYNTAX, form);
                }
            case ',':
                /* (unsyntax form) of (unsyntax-splicing form) */
                {
                    int c1 = Scm_GetcUnsafe(port);
                    if (c1 == EOF) {
                        RAISE_READ_ERROR0("unterminated unsyntax");
                    } else if (c1 == '@') {
                        return read_quoted(port, SCM_SYM_UNSYNTAX_SPLICING, ctx);
                    } else {
                        Scm_UngetcUnsafe(c1, port);
                        return read_quoted(port, SCM_SYM_UNSYNTAX, ctx);
                    }
                }
            default:
                RAISE_READ_ERROR1("unsupported #-syntax: #~C", Object::makeChar(c1));
            }
        }
    case '\'': return  read_quoted(port, SCM_SYM_QUOTE, ctx);
    case '`': return read_quoted(port, SCM_SYM_QUASIQUOTE, ctx);
#ifndef MONA_SCHEME
    case ':':
        return read_keyword(port, ctx);
#endif
    case ',':
        {
            int c1 = Scm_GetcUnsafe(port);
            if (c1 == EOF) {
                RAISE_READ_ERROR0("unterminated unquote");
            } else if (c1 == '@') {
                return read_quoted(port, SCM_SYM_UNQUOTE_SPLICING, ctx);
            } else {
                Scm_UngetcUnsafe(c1, port);
                return read_quoted(port, SCM_SYM_UNQUOTE, ctx);
            }
        }
    case '|':
        return read_escaped_symbol(port, '|');
    case '[':
        /* TODO: make it customizable */
        return read_list(port, ']', ctx);
    case '{':
        return read_list(port, '}', ctx);
    case '+':; case '-':
        /* Note: R5RS doesn't permit identifiers beginning with '+' or '-',
           but some Scheme programs use such identifiers. */
        return read_symbol_or_number(port, c, ctx);
    case '.':;
        {
            int c1 = Scm_GetcUnsafe(port);
            if (!char_word_constituent(c1)) {
                RAISE_READ_ERROR0("dot in wrong context");
            }
            Scm_UngetcUnsafe(c1, port);
            return read_symbol_or_number(port, c, ctx);
        }
    case '0':; case '1':; case '2':; case '3':; case '4':;
    case '5':; case '6':; case '7':; case '8':; case '9':;
        /* Note: R5RS doesn't permit identifiers beginning with digits,
           but some Scheme programs use such identifiers. */
        return read_symbol_or_number(port, c, ctx);
    case ')':; case ']':; case '}':;
        RAISE_READ_ERROR0("extra close parenthesis");
    case EOF:
        return SCM_EOF;
    default:
        return read_symbol(port, c, ctx);
    }
}

static ScmObj read_item(ScmPort *port, ScmReadContext *ctx)
{
    for (;;) {
        ScmObj obj = read_internal(port, ctx);
        if (!SCM_UNDEFINEDP(obj)) return obj;
    }
}

/*----------------------------------------------------------------
 * List
 */

/* Internal read_list.  returns whether the list contains unresolved
   reference or not within the flag has_ref */
static ScmObj read_list_int(ScmPort *port, ScmChar closer,
                            ScmReadContext *ctx, int *has_ref, int start_line)
{
    ScmObj start = SCM_NIL, last = SCM_NIL, item;
    int c, dot_seen = FALSE, ref_seen = FALSE;

    for (;;) {
        c = skipws(port, ctx);
        if (c == EOF) goto eoferr;
        if (c == closer) {
            *has_ref = !!ref_seen;
            return start;
        }

        if (dot_seen) goto baddot;

        if (c == '.') {
            int c2 = Scm_GetcUnsafe(port);
            if (c2 == closer) {
                goto baddot;
            } else if (c2 == EOF) {
                goto eoferr;
            } else if (isspace(c2)) {
                /* dot pair at the end */
                if (start == SCM_NIL) goto baddot;
                item = read_item(port, ctx);
#ifndef MONA_SCHEME
                if (SCM_READ_REFERENCE_P(item)) ref_seen = TRUE;
#endif
                SCM_SET_CDR(last, item);
                dot_seen = TRUE;
                continue;
            }
            Scm_UngetcUnsafe(c2, port);
            item = read_symbol_or_number(port, c, ctx);
        } else {
            Scm_UngetcUnsafe(c, port);
            item = read_internal(port, ctx);
            if (SCM_UNDEFINEDP(item)) continue;
#ifndef MONA_SCHEME
            if (SCM_READ_REFERENCE_P(item)) ref_seen = TRUE;
#endif
        }
        SCM_APPEND1(start, last, item);
    }
  eoferr:
    if (start_line >= 0) {
        RAISE_READ_ERROR1("EOF inside a list (starting from line ~d)", Object::makeInt(start_line));
    } else {
        RAISE_READ_ERROR0("EOF inside a list");
    }
  baddot:
    RAISE_READ_ERROR0("bad dot syntax");
    return SCM_NIL;             /* dummy */
}
    #include "VM.h"
extern VM* theVM;
static ScmObj read_list(ScmPort *port, ScmChar closer, ScmReadContext *ctx)
{
    int has_ref;
    int line = -1;
    ScmObj r;

#ifdef MONA_SCHEME
    line = Scm_PortLine(port);
#else
    if (ctx->flags & SCM_READ_SOURCE_INFO) line = Scm_PortLine(port);
#endif
    r = read_list_int(port, closer, ctx, &has_ref, line);

#ifdef MONA_SCHEME
    if (r.isPair() && line >= 0) {
        r = Object::cons(r.car(), r.cdr(), Pair::list2(Object::makeString(port->toString()), Object::makeInt(line)));
    }
#else
    if (SCM_PAIRP(r) && (ctx->flags & SCM_READ_SOURCE_INFO) && line >= 0) {
        /* Swap the head of the list for an extended pair to record
           source-code info.*/
        r = Scm_ExtendedCons(SCM_CAR(r), SCM_CDR(r));
        Scm_PairAttrSet(SCM_PAIR(r), SCM_SYM_SOURCE_INFO,
                        SCM_LIST2(Scm_PortName(port), SCM_MAKE_INT(line)));
    }
    if (has_ref) ref_push(ctx, r, SCM_FALSE);
#endif
    return r;
}

static ScmObj read_vector(ScmPort *port, ScmChar closer, ScmReadContext *ctx)
{
    int has_ref;
    int line = -1;
    ScmObj r;

#ifdef MONA_SCHEME
    line = Scm_PortLine(port);
#else
    if (ctx->flags & SCM_READ_SOURCE_INFO) line = Scm_PortLine(port);
#endif
    r = read_list_int(port, closer, ctx, &has_ref, line);
    r = Scm_ListToVector(r, 0, -1);
#ifndef MONA_SCHEME
    if (has_ref) ref_push(ctx, r, SCM_FALSE);
#endif
    return r;
}

static ScmObj read_quoted(ScmPort *port, ScmObj quoter, ScmReadContext *ctx)
{
    int line = -1;
    ScmObj item, r;
#ifdef MONA_SCHEME
    line = Scm_PortLine(port);
#else
    if (ctx->flags & SCM_READ_SOURCE_INFO) line = Scm_PortLine(port);
#endif

    item = read_item(port, ctx);
    if (SCM_EOFP(item)) RAISE_READ_ERROR0("unterminated quote");
#ifdef MONA_SCHEME
    if (line >= 0) {
        r = Object::cons(quoter, Object::cons(item, Object::Nil), Pair::list2(Object::makeString(port->toString()), Object::makeInt(line)));
    }
#else
    if (line >= 0) {
        r = Scm_ExtendedCons(quoter, Scm_Cons(item, SCM_NIL));
        Scm_PairAttrSet(SCM_PAIR(r), SCM_SYM_SOURCE_INFO,
                        SCM_LIST2(Scm_PortName(port), SCM_MAKE_INT(line)));
    } else {
#endif
//        extern Object STDOUT_PORT;;
//         portDisplay(STDOUT_PORT, quoter);
//         portDisplay(STDOUT_PORT, makeString("]"));

        r = Scm_Cons(quoter, Scm_Cons(item, SCM_NIL));
#ifndef MONA_SCHEME
    }
    if (SCM_READ_REFERENCE_P(item)) ref_push(ctx, SCM_CDR(r), SCM_FALSE);
#endif
    return r;
}

/*----------------------------------------------------------------
 * String
 */

static ScmChar read_string_xdigits(ScmPort *port, int ndigs, int key,
                                   int incompletep)
{
    char buf[8];
    int nread;
    ScmChar r;
    SCM_ASSERT(ndigs <= 8);
    r = Scm_ReadXdigitsFromPort(port, ndigs, buf, &nread);
    if (r == SCM_CHAR_INVALID) {
#ifndef MONA_SCHEME
        ScmDString ds;
        int c, i;
        /* skip chars to the end of string, so that the reader will read
           after the erroneous string */
        for (;;) {
            if (incompletep) c = Scm_GetbUnsafe(port);
            else c = Scm_GetcUnsafe(port);
            if (c == EOF || c == '"') break;
            if (c == '\\') {
                if (incompletep) c = Scm_GetbUnsafe(port);
                else c = Scm_GetcUnsafe(port);
            }
        }
        /* construct an error message */
        Scm_DStringInit(&ds);
        Scm_DStringPutc(&ds, '\\');
        Scm_DStringPutc(&ds, key);
        for (i=0; i<nread; i++) Scm_DStringPutc(&ds, (unsigned char)buf[i]);
        Scm_ReadError(port,
                      "Bad '\\%c' escape sequence in a string literal: %s",
                      key, Scm_DStringGetz(&ds));
#else
        printf("error:%s %s:%d\n", __func__, __FILE__, __LINE__);fflush(stdout);// debug
#endif
    }
    return r;
}

static ScmObj read_string(ScmPort *port, int incompletep,
                          ScmReadContext *ctx)
{
    int c = 0;
#ifdef MONA_SCHEME
    ucs4string ds;
#else
    ScmDString ds;
    Scm_DStringInit(&ds);
#endif

#define FETCH(var)                                      \
    if (incompletep) { var = Scm_GetbUnsafe(port); }    \
    else             { var = Scm_GetcUnsafe(port); }
#ifdef MONA_SCHEME
#define ACCUMULATE(var)                                 \
    ds += var;
#else
#define ACCUMULATE(var)                                 \
    if (incompletep) { SCM_DSTRING_PUTB(&ds, var); }    \
    else             { SCM_DSTRING_PUTC(&ds, var); }
#endif

    for (;;) {
        FETCH(c);
        switch (c) {
          case EOF: goto eof_exit;
          case '"': {
#ifdef MONA_SCHEME
              return makeString(ds.data());
#else
              int flags = ((incompletep? SCM_STRING_INCOMPLETE : 0)
                           | SCM_STRING_IMMUTABLE);
              return Scm_DStringGet(&ds, flags);
#endif
          }
          case '\\': {
            int c1 = Scm_GetcUnsafe(port);
            switch (c1) {
              case EOF: goto eof_exit;
              case 'n': ACCUMULATE('\n'); break;
              case 'r': ACCUMULATE('\r'); break;
              case 'f': ACCUMULATE('\f'); break;
              case 't': ACCUMULATE('\t'); break;
              case '\\': ACCUMULATE('\\'); break;
              case '0': ACCUMULATE('0'); break;
              case 'x': {
                  int cc = read_string_xdigits(port, 2, 'x', incompletep);
                  ACCUMULATE(cc);
                  break;
              }
              case 'u': {
                  int cc = read_string_xdigits(port, 4, 'u', incompletep);
                  ACCUMULATE(Scm_UcsToChar(cc));
                  break;
              }
              case 'U': {
                  int cc = read_string_xdigits(port, 8, 'U', incompletep);
                  ACCUMULATE(Scm_UcsToChar(cc));
                  break;
              }
              default:
                ACCUMULATE(c1); break;
            }
            break;
          }
          default: ACCUMULATE(c); break;
        }
    }
 eof_exit:
#ifdef MONA_SCHEME
    RAISE_READ_ERROR0("EOF encountered in a string literal");
#else
    Scm_ReadError(port, "EOF encountered in a string literal: %S",
                  Scm_DStringGet(&ds, 0));
#endif
    /* NOTREACHED */
    return SCM_FALSE;
}

/*----------------------------------------------------------------
 * Character
 */

static struct char_name {
    const char *name;
    ScmObj ch;
} char_names[] = {
    { "space",        SCM_MAKE_CHAR(' ')  },
    { "newline",      SCM_MAKE_CHAR('\n') },
    { "nl",           SCM_MAKE_CHAR('\n') },
    { "lf",           SCM_MAKE_CHAR('\n') },
    { "return",       SCM_MAKE_CHAR('\r') },
    { "cr",           SCM_MAKE_CHAR('\r') },
    { "tab",          SCM_MAKE_CHAR('\t') },
    { "ht",           SCM_MAKE_CHAR('\t') },
    { "page",         SCM_MAKE_CHAR('\f') },
    { "escape",       SCM_MAKE_CHAR(0x1b) },
    { "esc",          SCM_MAKE_CHAR(0x1b) },
    { "delete",       SCM_MAKE_CHAR(0x7f) },
    { "del",          SCM_MAKE_CHAR(0x7f) },
    { "null",         SCM_MAKE_CHAR(0)    },
#ifdef MONA_SCHEME
    { NULL, Object::Undef }
#else
    { NULL, 0 }
#endif
};

static ScmObj read_char(ScmPort *port, ScmReadContext *ctx)
{
    int c;
    ScmString *name;
#ifdef MONA_SCHEME
#else
    const char *cname;
#endif
    u_int32_t namelen, namesize;
    struct char_name *cntab = char_names;
    c = Scm_GetcUnsafe(port);
    switch (c) {
    case EOF: RAISE_READ_ERROR0("EOF encountered in character literal");
    case '(':; case ')':; case '[':; case ']':; case '{':; case '}':;
    case '"':; case ' ':; case '\\':; case '|':; case ';':;
    case '#':;
        return SCM_MAKE_CHAR(c);
    default:
        /* need to read word to see if it is a character name */
        name = SCM_STRING(read_word(port, c, ctx, TRUE));
#ifdef MONA_SCHEME
        ucs4string& cname = name->data();
        namelen = cname.size();
        namesize = namelen;
#else
        cname = Scm_GetStringContent(name, &namesize, &namelen, NULL);
#endif
        if (namelen == 1) {
            return SCM_MAKE_CHAR(c);
        }
        if (namelen != namesize) {
            /* no character name contains multibyte chars */
            goto unknown;
        }

#ifndef MONA_SCHEME
        /* handle #\x1f etc. */
        if (cname[0] == 'x' && isxdigit(cname[1])) {
            int code = Scm_ReadXdigitsFromString(cname+1, namesize-1, NULL);
            if (code < 0) goto unknown;
            return SCM_MAKE_CHAR(code);
        }

        /* handle #\uxxxx or #\uxxxxxxxx*/
        if ((cname[0] == 'u') && isxdigit(cname[1])) {
            int code;
            if (namesize == 5 || namesize == 9) {
                code = Scm_ReadXdigitsFromString(cname+1, namesize-1, NULL);
                if (code >= 0) return SCM_MAKE_CHAR(Scm_UcsToChar(code));
            }
            /* if we come here, it's an error. */
            Scm_ReadError(port, "Bad UCS character code: #\\%s", cname);
        }
#endif
        while (cntab->name) {
#ifdef MONA_SCHEME
            if (strncmp(cntab->name, cname.ascii_c_str(), strlen(cntab->name)) == 0) return cntab->ch;
#else
            if (strncmp(cntab->name, cname, namesize) == 0) return cntab->ch;
#endif
            cntab++;
        }

      unknown:
#ifdef MONA_SCHEME
        // todo
//        RAISE_READ_ERROR1("Unknown character name: ~d ", Object::makeChar(c));
        // todo
        return Object::makeChar(c);
#else
        Scm_ReadError(port, "Unknown character name: #\\%A", name);
#endif
    }
    return SCM_UNDEFINED;       /* dummy */
}

/*----------------------------------------------------------------
 * Symbols and Numbers
 */

/* Reads a sequence of word-constituent characters from PORT, and returns
   ScmString.  INITIAL may be a readahead character, or SCM_CHAR_INVALID
   if there's none.  TEMP_CASE_FOLD turns on case-fold mode regardless of
   the read context setting.
*/
static ScmObj read_word(ScmPort *port, ScmChar initial, ScmReadContext *ctx,
                        int temp_case_fold)
{
    int c = 0;
    int case_fold = temp_case_fold || (ctx->flags & SCM_READ_CASE_FOLD);
#ifndef MONA_SCHEME
    ScmDString ds;
    Scm_DStringInit(&ds);
#else
    ucs4string ds;
#endif
    if (initial != SCM_CHAR_INVALID) {
        if (case_fold && char_word_case_fold(initial)) initial = tolower(initial);
#ifndef MONA_SCHEME
        SCM_DSTRING_PUTC(&ds, initial);
#else
        ds += initial;
#endif
    }

    for (;;) {
        c = Scm_GetcUnsafe(port);
        if (c == EOF || !char_word_constituent(c)) {
            Scm_UngetcUnsafe(c, port);
#ifndef MONA_SCHEME
            return Scm_DStringGet(&ds, 0);
#else
//            printf("read_word [%s]", ds.c_str());
              return makeString(ds.data());
#endif
        }
        if (case_fold && char_word_case_fold(c)) c = tolower(c);
#ifndef MONA_SCHEME
        SCM_DSTRING_PUTC(&ds, c);
#else
        ds += c;
#endif
    }
}

static ScmObj read_symbol(ScmPort *port, ScmChar initial, ScmReadContext *ctx)
{
    ScmString *s = SCM_STRING(read_word(port, initial, ctx, FALSE));
    return Scm_Intern(s);
}

static ScmObj read_number(ScmPort *port, ScmChar initial, ScmReadContext *ctx)
{
    ScmString *s = SCM_STRING(read_word(port, initial, ctx, FALSE));
    ScmObj num = Scm_StringToNumber(s, 10, TRUE);
    if (num == SCM_FALSE)
#ifdef MONA_SCHEME
        RAISE_READ_ERROR1("bad numeric format: ~a", Object::makeString(s->data()));
#else
        Scm_ReadError(port, "bad numeric format: %S", s);
#endif
    return num;
}

static ScmObj read_symbol_or_number(ScmPort *port, ScmChar initial, ScmReadContext *ctx)

{
    ScmString *s = SCM_STRING(read_word(port, initial, ctx, FALSE));
    ScmObj num = Scm_StringToNumber(s, 10, TRUE);
    if (num == SCM_FALSE)
        return Scm_Intern(s);
    else
        return num;
}

#ifndef MONA_SCHEME
static ScmObj read_keyword(ScmPort *port, ScmReadContext *ctx)
{
    ScmString *s = SCM_STRING(read_word(port, SCM_CHAR_INVALID, ctx, FALSE));
    return Scm_MakeKeyword(s);
}
#endif

static ScmObj read_escaped_symbol(ScmPort *port, ScmChar delim)
{
    int c = 0;
#ifdef MONA_SCHEME
    ucs4string ds;
#else
    ScmDString ds;
    Scm_DStringInit(&ds);
#endif
    for (;;) {
        c = Scm_GetcUnsafe(port);
        if (c == EOF) {
            goto err;
        } else if (c == delim) {
#ifdef MONA_SCHEME
            ScmString *s = makeString(ds.c_str()).toString();
#else
            ScmString *s = SCM_STRING(Scm_DStringGet(&ds, 0));
#endif
            return Scm_Intern(s);
        } else if (c == '\\') {
            /* CL-style single escape */
            c = Scm_GetcUnsafe(port);
            /* TODO: we should recognize \xNN, since the symbol writer
               prints a symbol name in that syntax. */
            if (c == EOF) goto err;
#ifdef MONA_SCHEME
            ds += c;
#else
            SCM_DSTRING_PUTC(&ds, c);
#endif
        } else {
#ifdef MONA_SCHEME
            ds += c;
#else
            SCM_DSTRING_PUTC(&ds, c);
#endif
        }
    }
  err:
#ifndef MONA_SCHEME
    Scm_ReadError(port, "unterminated escaped symbol: |%s ...",
                  Scm_DStringGetz(&ds));
#else
    RAISE_READ_ERROR1("unterminated escaped symbol: |~s ...", Object::makeString(ds.c_str()));
#endif
    return SCM_UNDEFINED; /* dummy */
}

/*----------------------------------------------------------------
 * Regexp & charset
 */


/* gauche extension :  #/regexp/ */
static ScmObj read_regexp(ScmPort *port)
{
    ScmChar c = 0;
#ifdef MONA_SCHEME
    ucs4string ds;
#else
    ScmDString ds;
    Scm_DStringInit(&ds);
#endif
    for (;;) {
        c = Scm_GetcUnsafe(port);
        if (c == SCM_CHAR_INVALID) {
            RAISE_READ_ERROR0("unterminated literal regexp");
        }
        if (c == '\\') {
#ifdef MONA_SCHEME
            ds += c;
#else
            SCM_DSTRING_PUTC(&ds, c);
#endif
            c = Scm_GetcUnsafe(port);
            if (c == SCM_CHAR_INVALID) {
                RAISE_READ_ERROR0("unterminated literal regexp");
            }
#ifdef MONA_SCHEME
            ds += c;
#else
            SCM_DSTRING_PUTC(&ds, c);
#endif
        } else if (c == '/') {
            /* Read one more char to see if we have a flag */
            int flags = 0;
            c = Scm_GetcUnsafe(port);
#ifdef MONA_SCHEME
            if (c == 'i') flags |= SCM_REGEXP_CASE_FOLD;
            else          Scm_UngetcUnsafe(c, port);
#else
            if (c == 'i') flags |= SCM_REGEXP_CASE_FOLD;
            else          Scm_UngetcUnsafe(c, port);
#endif
#ifdef MONA_SCHEME
            const Object regexp = Object::makeRegexp(ds, flags & SCM_REGEXP_CASE_FOLD);
            Regexp* const regexpPointer = regexp.toRegexp();
            if (regexpPointer->isErrorOccured()) {
                callAssertionViolationImmidiaImmediately("read",
                                                         regexpPointer->errorMessage(),
                                                         regexpPointer->irritants());
                return Object::Undef;
            } else {
                return regexp;
            }
#else
            return Scm_RegComp(SCM_STRING(Scm_DStringGet(&ds, 0)), flags);
#endif
        } else {
#ifdef MONA_SCHEME
            ds += c;
#else
            SCM_DSTRING_PUTC(&ds, c);
#endif
        }
    }
}

#ifndef MONA_SCHEME
/* gauche extension :  #[charset] */
static ScmObj read_charset(ScmPort *port)
{
    return Scm_CharSetRead(port, NULL, TRUE, FALSE);
}


/*----------------------------------------------------------------
 * Back reference (#N# and #N=)
 */

static ScmObj read_reference(ScmPort *port, ScmChar ch, ScmReadContext *ctx)
{
    ScmHashEntry *e = NULL;
    int refnum = Scm_DigitToInt(ch, 10);

    for (;;) {
        ch = Scm_GetcUnsafe(port);
        if (ch == EOF) {
            RAISE_READ_ERROR0("unterminated reference form (#digits)");
        }
        if (SCM_CHAR_ASCII_P(ch) && isdigit(ch)) {
            refnum = refnum*10+Scm_DigitToInt(ch, 10);
            if (refnum < 0) RAISE_READ_ERROR0("reference number overflow");
            continue;
        }
        if (ch != '#' && ch != '=') {
            RAISE_READ_ERROR2("invalid reference form (must be either #digits# or #digits=) : #~d~a",
                              Object::makeInt(refnum),
                              Object::makeChar(ch));
        }
        break;
    }
    if (ch == '#') {
        /* #digit# - back reference */
        if (ctx->table == NULL
            || (e = Scm_HashTableGet(ctx->table, Scm_MakeInteger(refnum))) == NULL) {
            RAISE_READ_ERROR1("invalid reference number in #~d#", Object::makeInt(refnum));
        }
        if (SCM_READ_REFERENCE_P(e->value)
            && SCM_READ_REFERENCE_REALIZED(e->value)) {
            return SCM_READ_REFERENCE(e->value)->value;
        } else {
            return e->value;
        }
    } else {
        /* #digit= - register */
        ScmObj val;
        ScmObj ref = Scm_MakeReadReference();

        if (ctx->table == NULL) {
            ctx->table =
                SCM_HASH_TABLE(Scm_MakeHashTableSimple(SCM_HASH_EQV, 0));
        }
        if (Scm_HashTableGet(ctx->table, Scm_MakeInteger(refnum)) != NULL) {
            RAISE_READ_ERROR1("duplicate back-reference number in #~d=", Object::makeInt(refnum));
        }
        ref_register(ctx, ref, refnum);
        val = read_item(port, ctx);
        SCM_READ_REFERENCE(ref)->value = val;
        return val;
    }
}

/*----------------------------------------------------------------
 * SRFI-10 support
 */

ScmObj Scm_DefineReaderCtor(ScmObj symbol, ScmObj proc, ScmObj finisher)
{
    ScmObj pair;
    if (!SCM_PROCEDUREP(proc)) {
        Scm_Error("procedure required, but got %S\n", proc);
    }
    pair = Scm_Cons(proc, finisher);
    (void)SCM_INTERNAL_MUTEX_LOCK(readCtorData.mutex);
    Scm_HashTablePut(readCtorData.table, symbol, pair);
    (void)SCM_INTERNAL_MUTEX_UNLOCK(readCtorData.mutex);
    return SCM_UNDEFINED;
}

static ScmObj read_sharp_comma(ScmPort *port, ScmReadContext *ctx)
{
    int len, has_ref, line=-1;
    ScmChar next;
    ScmObj form, r;

    next = Scm_GetcUnsafe(port);
    if (next != '(') {
        RAISE_READ_ERROR1("bad #,-form: '(' should be followed, but got ~a",
                          Object::makeChar(next));
    }

#ifdef MONA_SCHEME
    line = Scm_PortLine(port);
#else
    if (ctx->flags & SCM_READ_SOURCE_INFO) line = Scm_PortLine(port);
#endif

    form = read_list_int(port, ')', ctx, &has_ref, line);
    len = Scm_Length(form);
    if (len <= 0) {
        RAISE_READ_ERROR1("bad #,-form: #,~a", form);
    }
    r = process_sharp_comma(port, SCM_CAR(form), SCM_CDR(form), ctx, has_ref);
    return r;
}

static ScmObj process_sharp_comma(ScmPort *port, ScmObj key, ScmObj args,
                                  ScmReadContext *ctx, int has_ref)
{
    ScmHashEntry *e;
    ScmObj r;

    if (ctx->flags & SCM_READ_DISABLE_CTOR) return SCM_FALSE;

    (void)SCM_INTERNAL_MUTEX_LOCK(readCtorData.mutex);
    e = Scm_HashTableGet(readCtorData.table, key);
    (void)SCM_INTERNAL_MUTEX_UNLOCK(readCtorData.mutex);

    if (e == NULL) RAISE_READ_ERROR1("unknown #,-key: ~a", key);
    SCM_ASSERT(SCM_PAIRP(e->value));
    r = Scm_ApplyRec(SCM_CAR(e->value), args);
    if (has_ref) ref_push(ctx, r, SCM_CDR(e->value));
    return r;
}

static ScmObj reader_ctor(ScmObj *args, int nargs, void *data)
{
    ScmObj optarg = (nargs > 2? args[2] : SCM_FALSE);
    return Scm_DefineReaderCtor(args[0], args[1], optarg);
}

/*----------------------------------------------------------------
 * Uvector
 */

/* Uvector support is implemented by extention.  When the extention
   is loaded, it sets up the pointer Scm_ReadUvectorHook. */

static ScmObj maybe_uvector(ScmPort *port, char ch, ScmReadContext *ctx)
{
    ScmChar c1, c2 = SCM_CHAR_INVALID;
    char *tag = NULL;

    c1 = Scm_GetcUnsafe(port);
    if (ch == 'f') {
        if (c1 != '1' && c1 != '3' && c1 != '6') {
            Scm_UngetcUnsafe(c1, port);
            return SCM_FALSE;
        }
        c2 = Scm_GetcUnsafe(port);
        if (c1 == '3' && c2 == '2') tag = "f32";
        else if (c1 == '6' && c2 == '4') tag = "f64";
        else if (c1 == '1' && c2 == '6') tag = "f16";
    } else {
        if (c1 == '8') tag = (ch == 's')? "s8" : "u8";
        else if (c1 == '1') {
            c2 = Scm_GetcUnsafe(port);
            if (c2 == '6') tag = (ch == 's')? "s16" : "u16";
        }
        else if (c1 == '3') {
            c2 = Scm_GetcUnsafe(port);
            if (c2 == '2') tag = (ch == 's')? "s32" : "u32";
        }
        else if (c1 == '6') {
            c2 = Scm_GetcUnsafe(port);
            if (c2 == '4') tag = (ch == 's')? "s64" : "u64";
        }
    }
    if (tag == NULL) {
        char buf[SCM_CHAR_MAX_BYTES*4], *bufp = buf;
        *bufp++ = ch;
        SCM_CHAR_PUT(bufp, c1);
        bufp += SCM_CHAR_NBYTES(c1);
        if (c2 != SCM_CHAR_INVALID) {
            SCM_CHAR_PUT(bufp, c2);
            bufp += SCM_CHAR_NBYTES(c2);
        }
        *bufp = '\0';
        RAISE_READ_ERROR1("invalid uniform vector tag: ~s", Object::makeString(buf));
    }
    if (Scm_ReadUvectorHook == NULL) {
        /* Require srfi-4 (gauche/uvector)
           NB: we don't need mutex here, for the loading of srfi-4 is
           serialized in Scm_Require. */
        Scm_Require(SCM_MAKE_STR("gauche/uvector"),
                    SCM_LOAD_PROPAGATE_ERROR, NULL);
        if (Scm_ReadUvectorHook == NULL)
            Scm_ReadError(port, "couldn't load srfi-4 module");
    }
    return Scm_ReadUvectorHook(port, tag, ctx);
}

/*----------------------------------------------------------------
 * Initialization
 */

void Scm__InitRead(void)
{
    readCtorData.table =
        SCM_HASH_TABLE(Scm_MakeHashTableSimple(SCM_HASH_EQ, 0));
    (void)SCM_INTERNAL_MUTEX_INIT(readCtorData.mutex);
    Scm_DefineReaderCtor(SCM_SYM_DEFINE_READER_CTOR,
                         Scm_MakeSubr(reader_ctor, NULL, 2, 1,
                                      SCM_SYM_DEFINE_READER_CTOR),
                         SCM_FALSE);
}

#endif
