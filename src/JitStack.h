/*
 * JitStack.h - Debugging helper for Jit.
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
 *  $Id: JitStack.h 261 2008-07-25 06:16:44Z higepon $
 */

#ifndef SCHEME_JITSTACK_
#define SCHEME_JITSTACK_

#include "scheme.h"
#include "Instruction.h"

namespace scheme {

class JitStack EXTEND_GC
{
public:
    JitStack() {}
    ~JitStack() {}

    void push(int instruction)
    {
        stack_.push_back(instruction);
    }

    ucs4string getTrace()
    {
        ucs4string trace;
        while (!stack_.empty()) {
            gc_vector<int>::iterator it = stack_.end();
            it--;
            trace += ucs4string(Instruction::toString(Object::makeInstruction(*it).val));
            trace += UC("\n");
            stack_.pop_back();
        }
        return trace;
    }

private:
    gc_vector<int> stack_;
};

}; // namespace scheme

#endif // SCHEME_JITSTACK_
