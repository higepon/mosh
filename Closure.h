/*
 * Closure.h - <closure>
 *
 *   Copyright (c) 2008  Higepon(Taro Minowa)  <higepon@users.sourceforge.jp>
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
 *  $Id: Closure.h 5306 2008-05-06 11:06:23Z higepon $
 */

#ifndef __SCHEME_CLOSURE_H__
#define __SCHEME_CLOSURE_H__

namespace scheme {

class Closure EXTEND_GC
{
public:
    Closure(Object* pc, int argLength, bool isOptionalArg, const Object* freeVars, int freeVariablesNum, int maxStack, Object sourceInfo)
        : pc(pc)
        ,argLength(argLength)
        ,isOptionalArg(isOptionalArg)
        ,freeVariablesNum(freeVariablesNum)
        ,maxStack(maxStack)
        ,sourceInfo(sourceInfo)
    {
#ifdef USE_BOEHM_GC
        freeVariables = new(GC) Object[freeVariablesNum];
#else
        freeVariables = new Object[freeVariablesNum];
#endif
        for (int i = 0; i < freeVariablesNum; i++) {
            this->freeVariables[i] = freeVars[freeVariablesNum - i - 1];
        }
    }

    ~Closure() {} // not virtual

    inline Object referFree(int n)
    {
#ifdef DEBUG
        if (n >= freeVariablesNum)
        {
            // todo
            fprintf(stderr, "refer free ");
            exit(-1);
        }
#endif
        return freeVariables[n];
    }

public:
    Object* pc;
    const int argLength;
    bool isOptionalArg;
    Object*  freeVariables;
    const int freeVariablesNum;
    const int maxStack;
    const Object sourceInfo;
};

}; // namespace scheme

#endif // __SCHEME_CLOSURE_H__
