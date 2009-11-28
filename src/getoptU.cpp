/*
 * getopt_long() -- long options parser
 *
 * Portions Copyright (c) 1987, 1993, 1994
 * The Regents of the University of California.  All rights reserved.
 *
 * Portions Copyright (c) 2003
 * PostgreSQL Global Development Group
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 * $PostgreSQL: pgsql/src/port/getopt_long.c,v 1.6 2007/03/26 21:44:11 momjian Exp $
 */

/*
 * getopt.cpp - 
 *
 *   Copyright (c) 2009  Higepon(Taro Minowa)  <higepon@users.sourceforge.jp>
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
 *  $Id: getopt.cpp 183 2008-07-04 06:19:28Z higepon $
 */

#include "getoptU.h"

using namespace scheme;

#ifndef HAVE_INT_OPTRESET
int optreset;
#endif

#define BADCH   '?'
#define BADARG  ':'
#define EMSG    (ucs4char*)(UC(""))

int  opterrU;
int  optindU= 1;
int  optoptU;
ucs4char *optargU;

static size_t strcspnU(const ucs4char* str1, const ucs4char* str2) {

    const ucs4char* const head = str1;
    const ucs4char* t;
    for (; *str1; str1++) {

        for (t = str2; *t; t++) if (*t == *str1) return str1 - head;
    }
    return str1 - head;
}

static int strncmpU(const ucs4char *s1, const ucs4char *s2, size_t n)
{
    for(size_t i = 0; i < n ; i++ ) {
        if( s1[i] > s2[i] ) {
            return 1;
        }
        if( s1[i] < s2[i] ) {
            return -1;
        }
        if( s1[i] == s2[i] ) {
            continue;
        }
        if( s1[i] == '\0' || s2[i] == '\0' ) {
            break;
        }
    }
    return 0;
}

static size_t strlenU(const ucs4char *s)
{
    int c = 0;
    while(*s++)
        c++;
    return c;
}

// strchr from OpenBSD
static ucs4char* strchrU(const ucs4char *p, int ch)
{
    for (;; ++p) {
        if (*p == ch)
            return((ucs4char *)p);
        if (!*p)
            return((ucs4char *)NULL);
    }
    /* NOTREACHED */
}

int scheme::getopt_longU(int argc, ucs4char *const argv[],
            const ucs4char *optstring,
            const struct optionU * longopts, int *longindex)
{
    static ucs4char *place = EMSG;  /* option letter processing */
    ucs4char       *oli;            /* option letter list index */

    if (optreset || !*place)
    {                           /* update scanning pointer */
        optreset = 0;

        if (optindU >= argc)
        {
            place = EMSG;
            return -1;
        }

        place = argv[optindU];

        if (place[0] != '-')
        {
            place = EMSG;
            return -1;
        }

        place++;

        if (place[0] && place[0] == '-' && place[1] == '\0')
        {                       /* found "--" */
            ++optindU;
            place = EMSG;
            return -1;
        }

        if (place[0] && place[0] == '-' && place[1])
        {
            /* long option */
            size_t      namelen;
            int         i;

            place++;
            namelen = strcspnU(place, UC("="));
            for (i = 0; longopts[i].name != NULL; i++)
            {
                if (strlenU(longopts[i].name) == namelen
                    && strncmpU(place, longopts[i].name, namelen) == 0)
                {
                    if (longopts[i].has_arg)
                    {
                        if (place[namelen] == '=')
                            optargU = place + namelen + 1;
                        else if (optindU < argc - 1)
                        {
                            optindU++;
                            optargU = argv[optindU];
                        }
                        else
                        {
                            if (optstring[0] == ':')
                                return BADARG;
                            if (opterrU)
                                fprintf(stderr,
                                   "%s: option requires an argument -- %s\n",
                                        ucs4string(argv[0]).ascii_c_str(), ucs4string(place).ascii_c_str());
                            place = EMSG;
                            optindU++;
                            return BADCH;
                        }
                    }
                    else
                    {
                        optargU = NULL;
                        if (place[namelen] != 0)
                        {
                            /* XXX error? */
                        }
                    }
                    optindU++;

                    if (longindex)
                        *longindex = i;

                    place = EMSG;

                    if (longopts[i].flag == NULL)
                        return longopts[i].val;
                    else
                    {
                        *longopts[i].flag = longopts[i].val;
                        return 0;
                    }
                }
            }

            if (opterrU && optstring[0] != ':')
                fprintf(stderr,
                        "%s: illegal option -- %s %d\n", ucs4string(argv[0]).ascii_c_str(), ucs4string(place).ascii_c_str(), __LINE__);
            place = EMSG;
            optindU++;
            return BADCH;
        }
    }

    /* short option */
    optoptU = (int) *place++;

    oli = strchrU(optstring, optoptU);
    if (!oli)
    {
        if (!*place)
            ++optindU;
        if (opterrU && *optstring != ':') {
            fprintf(stderr,
                    "%s: illegal option -- %c %d\n", ucs4string(argv[0]).ascii_c_str(), ucs4string(optoptU).ascii_c_str(), __LINE__);
        }
        return BADCH;
    }

    if (oli[1] != ':')
    {                           /* don't need argument */
        optargU = NULL;
        if (!*place)
            ++optindU;
    }
    else
    {                           /* need an argument */
        if (*place)             /* no white space */
            optargU = place;
        else if (argc <= ++optindU)
        {                       /* no arg */
            place = EMSG;
            if (*optstring == ':')
                return BADARG;
            if (opterrU)
                fprintf(stderr,
                        "%s: option requires an argument -- %c\n",
                        ucs4string(argv[0]).ascii_c_str(), ucs4string(optoptU).ascii_c_str());
            return BADCH;
        }
        else
            /* white space */
            optargU = argv[optindU];
        place = EMSG;
        ++optindU;
    }
    return optoptU;
}

